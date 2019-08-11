#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((common utils) #:prefix utils:)
 ((common deps) #:prefix deps:)
 ((ice-9 readline))
 ((ice-9 format))
 ((ice-9 regex) #:prefix regex:)
 ((ice-9 rdelim) #:prefix rdelim:)
 ((ice-9 popen) #:prefix popen:)
 ((srfi srfi-1) #:prefix srfi1:))

(define (device-size dev)
  (let* ((dev-size (utils:system->string* "blockdev" "--getsize64" dev))
	 (dev-size (regex:string-match "[0-9]+" dev-size)))
    (if dev-size
	(regex:match:substring dev-size 0)
	(error "Could not calculate device size" dev))))

(define (partdev device part-id)
  (cond
   ((regex:string-match "/dev/[hsv]d[a-z]+" device)
    (string-append device part-id))
   ((or (regex:string-match "/dev/nvme[0-9]+n[0-9]+" device)
	(regex:string-match "/dev/mmcblk[0-9]+" device))
    (string-append device "p" part-id))
   (else (error "Unsupported device type:" device))))

(define (partuuid path n)
  (if (utils:block-device? path)
      (let ((matches
	     (regex:string-match
	      "Partition unique GUID: ([0-9A-F-]+)"
	      (utils:system->string* "sgdisk" "-i" (number->string n) path))))
	(if matches (regex:match:substring matches 1) #f))
      (error "Not a block device:" path)))

(define (fsuuid path)
  (if (utils:block-device? path)
      (let ((matches
	     (regex:string-match
	      "[^\n]+?"
	      (utils:system->string* "blkid" "-s" "UUID" "-o" "value" path))))
	(if matches (regex:match:substring matches 0) #f))
      (error "Not a block device:" path)))

(define (init-boot-parts-bios boot-dev)
  (system* "sgdisk" boot-dev "-Z"
	   "-n" "1:0:+2M"
	   "-t" "1:ef02"
	   "-N" "2"
	   "-t" "2:8300")
  (system* "partprobe" boot-dev)
  (let ((boot-partdev (partdev boot-dev "2")))
    (utils:println "Formatting boot partition device as EXT4:" boot-partdev)
    (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" "-j" boot-partdev)))
	  (error "Failed to create EXT4 filesystem on:" boot-partdev))
    boot-partdev))

(define (init-boot-parts-uefi boot-dev)
  (system* "sgdisk" boot-dev "-Z"
	   "-N" "1"
	   "-t" "1:ef00")
  (system* "partprobe" boot-dev)
  (let ((boot-partdev (partdev boot-dev "1")))
    (utils:println "Formatting boot partition device as FAT32:" boot-partdev)
    (when (not (zero? (system* "mkfs.fat" "-F32" boot-partdev)))
	  (error "Failed to create FAT32 filesystem on:" boot-partdev))
    boot-partdev))

(define* (init-boot-parts boot-dev #:key uefiboot?)
  (let ((boot-partdev
	 (if uefiboot?
	     (init-boot-parts-uefi boot-dev)
	     (init-boot-parts-bios boot-dev))))
    (utils:println "Finished setting up partitions on:" boot-dev)
    boot-partdev))

(define* (init-root-parts root-dev #:key boot-dev uefiboot?)
  (cond
   (boot-dev
    (system* "sgdisk" root-dev "-Z" "-N" "1" "-t" "1:8300")
    (system* "partprobe" root-dev)
    (vector
     (init-boot-parts boot-dev #:uefiboot? uefiboot?)
     (partdev root-dev "1")))
   (uefiboot?
    (system* "sgdisk" root-dev "-Z"
	     "-n" "1:0:+500M"
	     "-N" "2"
	     "-t" "1:ef00"
	     "-t" "2:8300")
    (system* "partprobe" root-dev)
    (let ((boot-partdev (partdev root-dev "1"))
	  (root-partdev (partdev root-dev "2")))
      (utils:println "Formatting boot partition device as FAT32:" boot-partdev)
      (when (not (zero? (system* "mkfs.fat" "-F32" boot-partdev)))
	    (error "Failed to create FAT32 filesystem on:" boot-partdev))
      (vector boot-partdev root-partdev)))
   (else
    (system* "sgdisk" root-dev "-Z"
	     "-n" "1:0:+2M"
	     "-n" "2:0:+500M"
	     "-N" "3"
	     "-t" "1:ef02"
	     "-t" "2:8300"
	     "-t" "3:8300")
    (system* "partprobe" root-dev)
    (let ((boot-partdev (partdev root-dev "2"))
	  (root-partdev (partdev root-dev "3")))
      (utils:println "Formatting boot partition device as EXT4:" boot-partdev)
      (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" "-j" boot-partdev)))
	    (error "Failed to create EXT4 filesystem on:" boot-partdev))
      (vector boot-partdev root-partdev)))))

(define (init-cryptroot partdev label)
  (utils:println "Formatting" partdev "to be used as LUKS device...")
  (when (not (zero? (system* "cryptsetup" "luksFormat" partdev)))
    (error "Failed formatting of LUKS device" partdev))
  (newline)
  (utils:println "Finished formatting device" partdev "for LUKS encryption!")
  (utils:println "Opening LUKS device" partdev "as" label "...")
  (when (not (zero? (system* "cryptsetup" "luksOpen" partdev label)))
    (error "Failed to open LUKS device:" label))
  (newline)
  (utils:println "It is recommended to overwrite a new LUKS device with random data.")
  (utils:println "WARNING: This can take quite a long time!")
  (let ((shred-prompt (readline "Would you like to overwrite LUKS device with random data? [Y/n]")))
    (cond
     ((regex:string-match "[nN]" shred-prompt)
      (utils:println "Skipped shredding of LUKS device."))
     (else
      (utils:println "Shredding LUKS device...")
      (let* ((luks-dev (string-append "/dev/mapper/" label))
	     (dev-size (device-size luks-dev)))
	(system* "dd" "if=/dev/zero" (string-append "of=" luks-dev) "status=progress"))))))

(define (init-cryptdevs keyfile dev-list)
  (map
   (lambda (s)
     (let* ((m (string-split s #\:))
	    (device (car m))
	    (label (cadr m)))
       (when (not (file-exists? (string-append "/dev/mapper/" label)))
	 (system* "cryptsetup" "luksOpen" "--key-file" keyfile device label))))
   (string-split dev-list #\,)))

(define* (init-zfsroot zpool rootfs #:key swap-size dir-list)
  (utils:system->devnull* "zpool" "import" zpool)
  (when (not (zero? (utils:system->devnull* "zpool" "list" zpool)))
    (error "could not find or import ZFS pool:" zpool))
  (let* ((root-dataset (utils:path zpool rootfs))
	 (swap-dataset (utils:path root-dataset "swap"))
	 (swap-zvol (utils:path "" "dev" "zvol" swap-dataset)))
    (when (zero? (utils:system->devnull* "zfs" "list" root-dataset))
      (error "root dataset already exists!" root-dataset))
    (utils:system->devnull*
     "zfs" "create"
     "-o" "compression=lz4"
     root-dataset)
    (map
     (lambda (dir-name)
       (utils:system->devnull* "zfs" "create" (utils:path root-dataset dir-name)))
     dir-list)
    (when swap-size
      (utils:println "Creating ZFS volume for swap device...")
      (utils:system->devnull*
       "zfs" "create"
       "-V" swap-size
       "-o" "sync=always"
       "-o" "primarycache=metadata"
       "-o" "secondarycache=none"
       "-o" "logbias=throughput"
       swap-dataset)
      (utils:system->devnull* "mkswap" swap-zvol)
      (if (zero? (utils:system->devnull* "swapon" swap-zvol))
	  (utils:system->devnull* "swapoff" swap-zvol)
	  (utils:println "WARNING:" "failed to swapon" swap-zvol)))
    (utils:println "Finished setting up ZFS pool:" zpool)))

(define (parse-swapfile-args swap-size swapfiles)
  (let* ((swap-bytes (utils:parse-unit-as-bytes swap-size))
	 (swapfile-bytes (floor (/ swap-bytes swapfiles)))
	 (swapfile-size (utils:emit-bytes-as-unit swapfile-bytes)))
    (map
     (lambda (idx)
       (list
	(string-append
	 "file" (format #f "~4,'0d" idx)
	 "_" swapfile-size)
	swapfile-bytes))
     (srfi1:iota swapfiles 1 1))))

(define (init-swapfiles root-dir swapfile-args)
  (when (not (file-exists? root-dir))
    (error "Directory" root-dir "does not exists!"))
  (let ((swap-dir (utils:path root-dir "swap")))
    (when (not (file-exists? swap-dir))
      (mkdir swap-dir))
    (map
     (lambda (args)
       (let* ((filename (car args))
	      (size (cadr args))
	      (size (number->string size))
	      (swapfile (utils:path swap-dir filename)))
	 (utils:println "Allocating" size "of swap space in" swapfile "...")
	 (system* "dd" "if=/dev/zero"
		  (string-append "of=" swapfile)
		  (string-append "bs=" size)
		  "count=1" "status=progress")
	 (chmod swapfile #o600)
	 (utils:system->devnull* "mkswap" swapfile)
	 (if (zero? (utils:system->devnull* "swapon" swapfile))
	     (utils:system->devnull* "swapoff" swapfile)
	     (utils:println "WARNING:" swapfile "failed to swap on!"))))
     swapfile-args)))

(define (fstab-entry-root root-dev)
  (utils:println (string-append "UUID=" (fsuuid root-dev)) "/" "ext4" "errors=remount-ro" "0" "1"))

(define (fstab-entry-boot boot-dev)
  (utils:println (string-append "UUID=" (fsuuid boot-dev)) "/boot" "ext4" "defaults" "0" "2"))

(define* (gen-fstab etc-dir #:key boot-partdev luks-label swapfile-args zpool rootfs dir-list)
  (when (not (file-exists? etc-dir))
    (error "Directory" etc-dir "does not exists!"))
  (with-output-to-file (utils:path etc-dir "fstab")
    (lambda ()
      (newline)
      (utils:println "# <file system> <mountpoint> <type> <options> <dump> <pass>")
      (newline)
      (cond
       (luks-label
      (cond
       (zpool
	(fstab-entry-root (utils:path "/dev/mapper" luks-label))
	(fstab-entry-boot boot-partdev)
	(utils:println (utils:path "/dev/zvol" zpool rootfs "swap") "none" "swap" "sw" "0" "0")
	(newline)
	(utils:println "# systemd specific legacy mounts of ZFS datasets")
	(newline)
	(map
	 (lambda (dirfs)
	   (utils:println "#" (utils:path zpool rootfs dirfs) (utils:path "" dirfs) "zfs" "defaults,x-systemd.after=zfs.target" "0" "0"))
	 dir-list))
       ((and swapfile-args (not (null? swapfile-args)))
	(fstab-entry-root (utils:path "/dev/mapper" luks-label))
	(fstab-entry-boot boot-partdev)
	(newline)
	(utils:println "#swapfiles")
	(newline)
	(map
	 (lambda (args)
	   (let* ((filename (car args))
		  (file-path (utils:path "/root/swap" filename)))
	     (utils:println file-path "none" "swap" "sw" "0" "0")))
	 swapfile-args))
       (else
	(let* ((vg-name (string-append luks-label "_vg"))
	       (lv-root (string-append "/dev/mapper/" vg-name "-root"))
	       (lv-swap (string-append "/dev/mapper/" vg-name "-swap")))
	  (fstab-entry-root lv-root)
	  (fstab-entry-boot boot-partdev)
	  (utils:println (string-append "UUID=" (fsuuid lv-swap)) "none" "swap" "sw" "0" "0"))))
      )
       (zpool
	(utils:println (utils:path "/dev/zvol" zpool rootfs "swap") "none" "swap" "sw" "0" "0")
	(fstab-entry-boot boot-partdev)))
      (utils:println "tmpfs" "/tmp" "tmpfs" "defaults" "0" "0"))))

(define (backup-header headers-dir device label)
  (let ((file (utils:path headers-dir label)))
    (with-output-to-file "/dev/null"
      (lambda ()
	(system* "cryptsetup" "luksHeaderBackup" device
		 "--header-backup-file" file)
	(chmod file #o400)))))

(define* (gen-crypttab etc-dir root-dir #:key luks-partdev luks-label keyfile dev-list)
  (let* ((crypttab-file (utils:path etc-dir "crypttab"))
	 (crypt-dir (utils:path root-dir "crypt"))
	 (headers-dir (utils:path crypt-dir "headers")))
    (when (not (file-exists? crypt-dir))
      (mkdir crypt-dir))
    (when (not (file-exists? headers-dir))
      (mkdir headers-dir))
    ;; ROOOTDEV
    (when luks-partdev
    (with-output-to-file crypttab-file
      (lambda ()
	(utils:println "# LUKS device containing root filesystem")
	(utils:println luks-label (string-append "UUID=" (fsuuid luks-partdev)) "none" "luks")))
    (backup-header headers-dir luks-partdev luks-label)
    )
    ;; DEVLISTS
    (when keyfile
      (let ((keyfile-name (basename keyfile)))
	(chmod keyfile #o400)
	(copy-file keyfile (utils:path crypt-dir keyfile-name))
	(with-output-to-file crypttab-file
	  (lambda ()
	    (newline)
	    (utils:println "# LUKS devices containing encrypted ZFS vdevs")
	    (newline)
	    (map
	     (lambda (args)
	       (let ((device (car args))
		     (label (cadr args)))
		 (utils:println label
				(string-append "UUID=" (fsuuid device))
				(string-append "/root/crypt/" keyfile-name)
				"luks")
		 (backup-header headers-dir device label)))
	     (map
	      (lambda (s)
		(string-split s #\:))
	      (string-split dev-list #\,)))))))))

(define* (init-instroot-zfs
	  instroot boot-partdev
	  zpool rootfs dir-list swap-size swapfiles
	  #:key keyfile dev-list luks-partdev luks-label)
  (when (file-exists? instroot)
    (error "Target" instroot "already exists!"))
  (when (not (utils:block-device? boot-partdev))
    (error "Cannot find device" boot-partdev "for boot partition!"))
  (when (not (zero? (utils:system->devnull* "zpool" "list" zpool)))
    (error "zpool" zpool "not available!"))
  (let ((systemfs (utils:path zpool rootfs)))
    (when (not (zero? (utils:system->devnull* "zfs" "list" systemfs)))
      (error "ZFS dataset does not exist:" systemfs))
    ;; BEGIN
    (mkdir instroot)
    (cond
     (luks-partdev
      (let ((luks-dev (utils:path "/dev/mapper" luks-label)))
      (when (not (utils:block-device? luks-dev))
        (error "Cannot find LUKS device" luks-label))
      (utils:println "Formatting LUKS device" luks-label "with ext4 to be used as root filesystem...")
      (when (not (zero? (system* "mkfs.ext4" luks-dev)))
	    (error "Failed to create EXT4 filesystem on:" luks-dev))
      (utils:println "Mounting LUKS root filesystem...")
      (when (not (zero? (system* "mount" luks-dev instroot)))
	(error "Failed to mount" luks-dev "as" instroot)))
      (utils:println "Mounting all ZFS root directories...")
      (system* "zfs" "set" (string-append "mountpoint=" instroot) systemfs))
     (zpool
      (utils:println "Mounting ZFS root...")
      (system* "zpool" "set" (string-append "bootfs=" systemfs) zpool)
      (system* "zfs" "umount" "-a")
      (system* "zfs" "set" (string-append "mountpoint=" instroot) systemfs)
      (system* "zfs" "mount" "-a")
      (system* "mount" "-o" "remount,exec,dev" instroot))
     (else
      (error "Either LUKS device or zfs pool must have been specified!")))
    (let ((boot-dir (utils:path instroot "boot")))
      (mkdir boot-dir)
      (when (not (zero? (system* "mount" boot-partdev boot-dir)))
	(error "Failed to mount" boot-partdev "as" boot-dir)))
    (let ((etc-dir (utils:path instroot "etc"))
	  (root-dir (utils:path instroot "root"))
	  (swapfile-args (parse-swapfile-args swap-size swapfiles)))
      (mkdir etc-dir)
      (mkdir root-dir #o700)
      (init-swapfiles root-dir swapfile-args)
      (gen-fstab
       etc-dir
       #:boot-partdev boot-partdev
       #:luks-label luks-label
       #:swapfile-args swapfile-args
       #:zpool zpool
       #:rootfs rootfs
       #:dir-list dir-list)
      (gen-crypttab
       etc-dir root-dir
       #:luks-partdev luks-partdev
       #:luks-label luks-label
       #:keyfile keyfile
       #:dev-list dev-list))))

(define (init-instroot-swapfile instroot boot-partdev luks-partdev luks-label swap-size swapfiles)
  (utils:println "Setting up installation root with swapfile for swap space...")
  (when (file-exists? instroot)
    (error "Target" instroot "already exists!"))
  (mkdir instroot)
  (utils:println "Formatting LUKS device" luks-label "with ext4 to be used as root filesystem...")
  (let ((luks-dev (utils:path "/dev/mapper" luks-label)))
    (when (not (zero? (system* "mkfs.ext4" luks-dev)))
	  (error "Failed to create EXT4 filesystem on:" luks-dev))
    (when (not (zero? (system* "mount" luks-dev instroot)))
      (error "Failed to mount" luks-dev "as" instroot)))
  (let ((boot-dir (utils:path instroot "boot")))
    (mkdir boot-dir)
    (when (not (zero? (system* "mount" boot-partdev boot-dir)))
      (error "Failed to mount" boot-partdev "as" boot-dir)))
  (let ((etc-dir (utils:path instroot "etc"))
	(root-dir (utils:path instroot "root"))
	(swapfile-args (parse-swapfile-args swap-size swapfiles)))
    (mkdir etc-dir)
    (mkdir root-dir #o700)
    (init-swapfiles root-dir swapfile-args)
    (gen-fstab
     etc-dir
     #:boot-partdev boot-partdev
     #:luks-label luks-label
     #:swapfile-args swapfile-args)
    (gen-crypttab
     etc-dir root-dir
     #:luks-partdev luks-partdev
     #:luks-label luks-label)))

(define* (init-instroot-lvm
	  instroot boot-partdev luks-partdev luks-label swap-size)
  (when (file-exists? instroot)
    (error "Target" instroot "already exists!"))
  (when (not (utils:block-device? boot-partdev))
    (error "Cannot find device" boot-partdev "for boot partition!"))
  (when (not (utils:block-device? luks-partdev))
    (error "Cannot find device" luks-partdev "for root partition!"))
  (let ((luks-dev (utils:path "/dev/mapper" luks-label))
	(vg-name (string-append luks-label "_vg")))
    (utils:println "Setting up LVM with volumes for root and swap filesystems...")
    (system* "pvcreate" luks-dev)
    (system* "vgcreate" vg-name luks-dev)
    (system* "lvcreate" "-L" swap-size  "-nswap" vg-name)
    (system* "lvcreate" "-l" "100%FREE" "-nroot" vg-name)
    (let ((lv-root (string-append "/dev/mapper/" vg-name "-root"))
	  (lv-swap (string-append "/dev/mapper/" vg-name "-swap"))
	  (boot-dir (utils:path instroot "boot"))
	  (root-dir (utils:path instroot "root"))
	  (etc-dir (utils:path instroot "etc")))
      (when (not (zero? (system* "mkfs.ext4" lv-root)))
	    (error "Failed to create EXT4 filesystem on:" lv-root))
      (mkdir instroot)
      (when (not (zero? (system* "mount" lv-root instroot)))
	(error "Failed to mount" instroot))
      (mkdir boot-dir)
      (when (not (zero? (system* "mount" boot-partdev boot-dir)))
	(error "Failed to mount" boot-partdev "as" boot-dir))
      (mkdir etc-dir)
      (mkdir root-dir #o700)
      (utils:println "Formatting" lv-swap "to be used as swap space...")
      (utils:system->devnull* "mkswap" lv-swap)
      (if (zero? (utils:system->devnull* "swapon" lv-swap))
	  (utils:system->devnull* "swapoff" lv-swap)
	  (utils:println "WARNING:" "failed to swap on" lv-swap))
      (gen-fstab
       etc-dir
       #:boot-partdev boot-partdev
       #:luks-label luks-label)
      (gen-crypttab
       etc-dir root-dir
       #:luks-partdev luks-partdev
       #:luks-label luks-label))))

(define options-spec
  `((target
     (single-char #\t)
     (description
      "Root mountpoint for installation")
     (default "/mnt/instroot")
     (value-arg "path")
     (value #t))
    (label
     (single-char #\l)
     (description
      "LUKS encrypted device name for root")
     (predicate
      ,(lambda (s) (regex:string-match "^[[:alnum:]_]+$" s)))
     (default "crypt_root")
     (value-arg "label")
     (value #t))
    (bootdev
     (single-char #\b)
     (description
      "Use separate boot device for /boot and insalling GRUB")
     (predicate ,utils:block-device?)
     (value-arg "device")
     (value #t))
    (rootdev
     (single-char #\r)
     (description
      "Device to use for root filesystem")
     (predicate ,utils:block-device?)
     (value-arg "device")
     (value #t))
    (zpool
     (single-char #\z)
     (description
      "ZFS pool name for system directories and swap device")
     (value-arg "zpool")
     (value #t))
    (rootfs
     (single-char #\f)
     (description
      "Name of the system root dataset in the ZFS pool")
     (default "system")
     (value-arg "name")
     (value #t))
    (dirlst
     (single-char #\d)
     (description
      "Coma separated list of root directories to mount as ZFS datasets")
     (default "home,var,var/lib,gnu")
     (value-arg "dirlist")
     (value #t))
    (devlst
     (single-char #\v)
     (description
      "Coma separeted list of colon separated pairs of other encrypted devices
\(e.g. members of ZFS pool), and their repsective LUKS labels.
\(e.g. /dev/sdb:foo,/dev/sdc:bar,/dev/sdd:baz)
These device mappings are used to:
 a) unlock these devices before importing ZFS pools
 b) create crypttab entries for automatic unlocking during boot
Specifying a keyfile is necessary for this feature!")
     (value-arg "devlist")
     (value #t))
    (keyfile
     (single-char #\k)
     (description
      "Keyfile used to decrypt other encrypted devices (i.e. ZFS pool members)")
     (predicate
      ,(lambda (s) (file-exists? s)))
     (value-arg "keyfile")
     (value #t))
    (genkey
     (single-char #\K)
     (description
      "Generate new keyfile with the given value as filename")
     (predicate
      ,(lambda (s) (not (file-exists? s))))
     (value-arg "filename")
     (value #t))
    (swapsize
     (single-char #\s)
     (description
      "Size of the total swap space to use (KMGT suffixes allowed)")
     (predicate
      ,(lambda (s) (regex:string-match "^[0-9]+[KMGT]?$" s)))
     (value-arg "size")
     (value #t))
    (swapfiles
     (single-char #\S)
     (description
      "Number of swapfiles to use to break total swap-space up into. Swapfiles are created
in equally sized chunks. COUNT zero means to use LVM volumes instead of swapfiles.")
     (default "0")
     (value-arg "count")
     (value #t))
    (uefiboot
     (description
      "Use UEFI boot partitions instead of BIOS.")
     (single-char #\E))
    (initdeps
     (description
      "Install and configure necessary ZFS dependencies only, then exit.")
     (single-char #\Z))
    (help
     (description
      "This usage help...")
     (single-char #\h))))

(define (create-keyfile f)
  (let* ((fname (basename f))
	 (fname (utils:path ".keys" fname)))
    (when (file-exists? fname)
	  (error "Cannot create keyfile! File already exists:" fname))
    (when (not (file-exists? ".keys"))
	  (mkdir ".keys"))
    (system* "dd" "if=/dev/random" (string-append "of=" fname) "bs=1024" "count=4")
    (chmod fname #o400)
    (utils:println "Finished creating keyfile:" fname)))

(define state-dir ".state")
(define lastrun-file (utils:path state-dir "lastrun.scm"))
(define lockfile-deps-base (utils:path state-dir "deps_base"))
(define lockfile-deps-zfs (utils:path state-dir "deps_zfs"))

(define (main args)
  (let* ((lastrun-map (utils:read-lastrun lastrun-file))
	 (options (utils:getopt-extra args options-spec lastrun-map))
	 (target (hash-ref options 'target))
	 (boot-dev (hash-ref options 'bootdev))
	 (root-dev (hash-ref options 'rootdev))
	 (luks-label (hash-ref options 'label))
	 (zpool (hash-ref options 'zpool))
	 (rootfs (hash-ref options 'rootfs))
	 (dir-list (hash-ref options 'dirlst))
	 (dir-list (if dir-list (string-split dir-list #\,) #f))
	 (keyfile (hash-ref options 'keyfile))
	 (new-keyfile (hash-ref options 'genkey))
	 (dev-list (hash-ref options 'devlst))
	 (dev-list (if dev-list (utils:parse-pairs dev-list) #f))
	 (swap-size (hash-ref options 'swapsize))
	 (swapfiles (hash-ref options 'swapfiles))
	 (swapfiles (string->number swapfiles))
	 (uefiboot? (hash-ref options 'uefiboot))
	 (initdeps? (hash-ref options 'initdeps))
	 (help? (hash-ref options 'help)))
    (if (not (file-exists? state-dir))
	(mkdir state-dir))
    (cond
     (help?
      (utils:println
       (string-append "
USAGE:

" (basename (car args)) " [OPTIONS]

Initialise and mount root filesystem. Uses LUKS encryption for root partition, and allows choice between LVM or swapfiles for swap configuration. Optionally allows for using a ZFS pool for custom root directories and swap volume. Also, allows configuring separate boot device, either BIOS or UEFI.

Valid options are:
"))
      (display (utils:usage options-spec lastrun-map))
      (newline))
     (new-keyfile
      (create-keyfile new-keyfile))
     ((utils:root-user?)
      (cond
       (initdeps?
	(deps:install-deps-base lockfile-deps-base)
	(deps:install-deps-zfs lockfile-deps-zfs)
	(utils:println "Finished installing all package dependencies!"))
       (else
	(when (not swap-size)
	  (error "Swap size must be specified!"))
	(when (and dev-list (not keyfile))
	  (error "Keyfile must be specified to unlock encrypted devices!"))
	(utils:write-lastrun lastrun-file options)
	(deps:install-deps-base lockfile-deps-base)
	(cond
	 (root-dev
	  (cond
	   (boot-dev
	    (error "Separate boot device is not supported!"))
	   (uefiboot?
	    (error "UEFI boot is not yet supported!"))
	   (else
	    (let* ((parts (init-root-parts root-dev))
		   (boot-partdev (vector-ref parts 0))
		   (root-partdev (vector-ref parts 1)))
	      (init-cryptroot root-partdev luks-label)
	      (cond
	       (zpool
		(when (and keyfile dev-list)
		      (init-cryptdevs keyfile dev-list))
		(deps:install-deps-zfs lockfile-deps-zfs)
		(init-zfsroot zpool rootfs #:dir-list dir-list)
		(init-instroot-zfs
		 target boot-partdev
		 zpool rootfs dir-list
		 swap-size swapfiles
		 #:root-partdev root-partdev
		 #:luks-label luks-label
		 #:dev-list dev-list
		 #:keyfile keyfile))
	       ((< 0 swapfiles)
		(init-instroot-swapfile
		 target boot-partdev root-partdev luks-label
		 swap-size swapfiles))
	       (else
		(deps:install-deps-lvm)
		(init-instroot-lvm
		 target boot-partdev root-partdev luks-label
		 swap-size)))))))
	 (zpool
	  (when (not boot-dev)
	    (error "Separate boot device must be specified when using ZFS as root!"))
	  (deps:install-deps-zfs lockfile-deps-zfs)
	  (let ((boot-partdev (init-boot-parts boot-dev #:uefiboot? uefiboot?)))
	    (init-zfsroot
	     zpool rootfs
	     #:swap-size swap-size
	     #:dir-list dir-list)
	    (init-instroot-zfs
	     target boot-partdev
	     zpool rootfs dir-list
	     swap-size swapfiles
	     #:dev-list dev-list
	     #:keyfile keyfile)))
	 (else
	  (error "Either block device for LUKS formatted root or a ZFS pool must be specified for root!")))
	(utils:write-lastrun (utils:path target "CONFIG_VARS.scm") options)
	;; to support backwards compatibility with debconf.sh shell script
	(utils:write-lastrun-vars (utils:path target "CONFIG_VARS.sh") options)
	(utils:println "Finished setting up installation root" target))))
     (else
      (error "This script must be run as root!")))))
