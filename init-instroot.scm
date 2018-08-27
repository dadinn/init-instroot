#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((common utils) #:prefix utils:)
 ((common zfs) #:prefix zfs:)
 ((ice-9 readline))
 ((ice-9 format))
 ((ice-9 regex) #:prefix regex:)
 ((ice-9 rdelim) #:prefix rdelim:)
 ((ice-9 popen) #:prefix popen:))

(define (device-size dev)
  (let* ((dev-size (utils:system->string* "blockdev" "--getsize64" dev))
	 (dev-size (regex:string-match "[0-9]+" dev-size)))
    (if dev-size
	(regex:match:substring dev-size 0)
	(error "Could not calculate device size" dev))))

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

(define (install-deps-base)
  (let ((missing (utils:which* "sgdisk" "partprobe" "cryptsetup" "pvcreate" "vgcreate" "lvcreate")))
    (if (not (null? missing))
	(if (file-exists? "/etc/debian_version")
	    (begin
	      (display "Installing necessary packages...")
	      (system "apt update")
	      (system "apt install -y gdisk parted cryptsetup lvm2"))
	    (error "Necessary binaries are missing" missing)))))

(define* (init-boot-parts boot-dev #:key uefi?)
    (cond
     (uefi?
      (system* "sgdisk" boot-dev "-Z"
	       "-N" "1"
	       "-t" "1:ef00")
      (system* "partprobe" boot-dev)
      (let ((boot-partdev (string-append boot-dev "1")))
	(system* "mkfs.fat" "-F32" boot-partdev)
	(utils:println "Finished setting up partitions on:" boot-dev)
	boot-partdev))
     (else
      (system* "sgdisk" boot-dev "-Z"
	       "-n" "1:0:+2M"
	       "-t" "1:ef02"
	       "-N" "2"
	       "-t" "2:8300")
      (system* "partprobe" boot-dev)
      (let ((boot-partdev (string-append boot-dev "2")))
	(system* "mkfs.ext4" "-q" "-m" "0" "-j" boot-partdev)
	(utils:println "Finished setting up partitions on:" boot-dev)
	boot-partdev))))

(define* (init-root-parts root-dev #:key boot-dev uefi?)
  (cond
   (boot-dev
    (system* "sgdisk" root-dev "-Z" "-N" "1" "-t" "1:8300")
    (system* "partprobe" root-dev)
    (vector
     (init-boot-parts boot-dev #:uefi? uefi?)
     (string-append root-dev "1")))
   (else
    (cond
     (uefi?
      (system* "sgdisk" root-dev "-Z"
	       "-n" "1:0:+500M"
	       "-N" "2"
	       "-t" "1:ef00"
	       "-t" "2:8300")
      (system* "partprobe" root-dev)
      (vector
       (string-append root-dev "1")
       (string-append root-dev "2")))
     (else
      (system* "sgdisk" root-dev "-Z"
	       "-n" "1:0:+2M"
	       "-n" "2:0:+500M"
	       "-N" "3"
	       "-t" "1:ef02"
	       "-t" "2:8300"
	       "-t" "3:8300")
      (system* "partprobe" root-dev)
      (vector
       (string-append root-dev "2")
       (string-append root-dev "3")))))))

(define (init-cryptroot partdev label)
  (utils:println "formatting" partdev "to be used as LUKS device...")
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
      (utils:println "Skipping shredding of LUKS device."))
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

(define* (init-zfsroot zpool rootfs swap-size #:key swapfiles dir-list)
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
     "-o" "canmount=off"
     root-dataset)
    (map
     (lambda (dir-name)
       (utils:system->devnull* "zfs" "create" (utils:path root-dataset dir-name)))
     dir-list)
    (when (not swapfiles)
      (utils:println "Creating ZFS volume for swap device...")
      (utils:system->devnull*
       "zfs" "create"
       "-V" swap-size
       "-o" "sync=always"
       "-o" "primary=cache=metadata"
       "-o" "logbias=throughput"
       swap-dataset)
      (utils:system->devnull* "mkswap" swap-zvol)
      (if (zero? (utils:system->devnull* "swapon" swap-zvol))
	  (utils:system->devnull* "swapoff" swap-zvol)
	  (utils:println "WARNING:" "failed to swapon" swap-zvol)))
    (utils:println "Finished setting up ZFS pool:" zpool)))

(define* (get-swapfile-args swap-size swapfiles)
  (let* ((swapsize-num (regex:match:substring
			(regex:string-match "^([0-9]+)[KMGT]?$" swap-size) 1))
	 (swapsize-num (string->number swapsize-num))
	 (swapsize-unit
	  (regex:match:substring
	   (regex:string-match "^[0-9]+([KMGT])?$" swap-size) 1)))
    (if (< 0 swapfiles)
	(let* ((swapfile-size (quotient swapsize-num swapfiles))
	       (swapfile-size (number->string swapfile-size))
	       (swapfile-size (string-append swapfile-size swapsize-unit)))
	  (map
	   (lambda (idx)
	     (let ((filename (string-append "file" (format #f "~4,'0d" idx) "_" swapfile-size)))
	       (list filename swapfile-size)))
	   (cdr (iota (+ 1 swapfiles)))))
	'())))

(define* (init-swapfiles root-dir swapfile-args)
  (when (not (file-exists? root-dir))
    (error "Directory" root-dir "does not exists!"))
  (let ((swap-dir (utils:path root-dir "swap")))
    (when (not (file-exists? swap-dir))
      (mkdir swap-dir))
    (map
     (lambda (args)
       (let* ((filename (car args))
	      (size (cadr args))
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

(define* (gen-fstab etc-dir #:key boot-partdev luks-partdev luks-label swapfile-args zpool rootfs dir-list)
  (when (not (file-exists? etc-dir))
    (error "Directory" etc-dir "does not exists!"))
  (with-output-to-file (utils:path etc-dir "fstab")
    (lambda ()
      (newline)
      (utils:println "# <file system> <mountpoint> <type> <options> <dump> <pass>")
      (newline)
      (utils:println (string-append "UUID=" (fsuuid luks-partdev)) "/" "ext4" "errors=remount-ro" "0" "1")
      (utils:println (string-append "UUID=" (fsuuid boot-partdev)) "/boot" "ext4" "default" "0" "2")
      (cond
       (zpool
	(utils:println (utils:path "/dev/zvol" zpool rootfs "swap") "none" "swap" "sw" "0" "0")
	(newline)
	(utils:println "# systemd specific legacy mounts of ZFS datasets")
	(newline)
	(map
	 (lambda (dirfs)
	   (utils:println "#" (utils:path zpool rootfs dirfs) (utils:path "" dirfs) "zfs" "default,x-systemd.after=zfs.target" "0" "0"))
	 dir-list))
       ((and swapfile-args (not (null? swapfile-args)))
	(newline)
	(utils:println "#swapfiles")
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
	  (utils:println (string-append "UUID=" (fsuuid lv-swap)) "none" "swap" "sw" "0" "0")))))))

(define* (backup-header headers-dir device label)
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
    (with-output-to-file crypttab-file
      (lambda ()
	(utils:println "# LUKS device containing root filesystem")
	(utils:println luks-label (string-append "UUID=" (fsuuid luks-partdev)) "none" "luks")))
    (backup-header headers-dir luks-partdev luks-label)
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
	  instroot boot-partdev luks-partdev luks-label
	  zpool rootfs dir-list swap-size swapfiles
	  #:key keyfile dev-list)
  (when (file-exists? instroot)
    (error "Target" instroot "already exists!"))
  (when (not (utils:block-device? boot-partdev))
    (error "Cannot find device" boot-partdev "for boot partition!"))
  (when (not (utils:block-device? luks-partdev))
    (error "Cannot find device" luks-partdev "for root partition!"))
  (when (not (utils:block-device? (string-append "/dev/mapper/" luks-label)))
    (error "Cannot find LUKS device" luks-label))
  (when (not (zero? (utils:system->devnull* "zpool" "list" zpool)))
    (error "zpool" zpool "not available!"))
  (let ((systemfs (utils:path zpool rootfs)))
    (when (not (zero? (utils:system->devnull* "zfs" "list" systemfs)))
      (error "ZFS dataset" systemfs "does not exist!"))
    ;; BEGIN
    (mkdir instroot)
    (utils:println "Formatting LUKS device" luks-label "with ext4 to be used as root filesystem...")
    (let ((luks-dev (utils:path "/dev/mapper" luks-label)))
      (utils:system->devnull* "mkfs.ext4" luks-dev)
      (when (not (zero? (system* "mount" luks-dev instroot)))
	(error "Failed to mount" luks-dev "as" instroot)))

    (let ((boot-dir (utils:path instroot "boot")))
      (mkdir boot-dir)
      (utils:system->devnull* "mkfs.ext4" "-m 0" "-j" boot-partdev)
      (when (not (zero? (system* "mount" boot-partdev boot-dir)))
	(error "Failed to mount" boot-partdev "as" boot-dir)))

    (let ((etc-dir (utils:path instroot "etc"))
	  (root-dir (utils:path instroot "root"))
	  (swapfile-args (get-swapfile-args swap-size swapfiles)))
      (mkdir etc-dir)
      (mkdir root-dir #o700)
      (init-swapfiles root-dir swapfile-args)
      (gen-fstab
       etc-dir
       #:boot-partdev boot-partdev
       #:luks-partdev luks-partdev
       #:swapfile-args swapfile-args
       #:zpool zpool
       #:rootfs rootfs
       #:dir-list dir-list)
      (gen-crypttab
       etc-dir root-dir
       #:luks-partdev luks-partdev
       #:luks-label luks-label
       #:keyfile keyfile
       #:dev-list dev-list))

    (utils:println "Mounting all ZFS root directories...")
    (system* "zfs" "set" (string-append "mountpoint=" instroot) systemfs)))

(define* (init-instroot-swapfile
	  instroot boot-partdev luks-partdev luks-label swap-size swapfiles)
  (utils:println "Setting up installation root with swapfile for swap space...")
  (when (file-exists? instroot)
    (error "Target" instroot "already exists!"))
  (mkdir instroot)
  (utils:println "Formatting LUKS device" luks-label "with ext4 to be used as root filesystem...")
  (let ((luks-dev (utils:path "/dev/mapper" luks-label)))
    (utils:system->devnull* "mkfs.ext4" luks-dev)
    (when (not (zero? (system* "mount" luks-dev instroot)))
      (error "Failed to mount" luks-dev "as" instroot)))
  (let ((boot-dir (utils:path instroot "boot")))
    (mkdir boot-dir)
    (utils:system->devnull* "mkfs.ext4" "-m" "0" "-j" boot-partdev)
    (when (not (zero? (system* "mount" boot-partdev boot-dir)))
      (error "Failed to mount" boot-partdev "as" boot-dir)))
  (let ((etc-dir (utils:path instroot "etc"))
	(root-dir (utils:path instroot "root"))
	(swapfile-args (get-swapfile-args swap-size swapfiles)))
    (mkdir etc-dir)
    (mkdir root-dir #o700)
    (init-swapfiles root-dir swapfile-args)
    (gen-fstab
     etc-dir
     #:boot-partdev boot-partdev
     #:luks-partdev luks-partdev
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
      (utils:system->devnull* "mkfs.ext4" lv-root)
      (mkdir instroot)
      (when (not (zero? (system* "mount" lv-root instroot)))
	(error "Failed to mount" instroot))
      (mkdir boot-dir)
      (utils:println "Formatting partition" boot-partdev "with ext4 to be used as /boot...")
      (when (not (zero? (system* "mount" boot-partdev boot-dir)))
	(error "Failed to mount" boot-dir))
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
       #:luks-partdev luks-partdev
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
     (default "home,var,gnu")
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
  (let ((fname (basename f)))
    (if (file-exists? fname)
	(throw 'file-already-exists fname)
	(system* "dd" "if=/dev/random" (string-append "of=" fname) "bs=1024" "count=4"))))

(define (main args)
  (let* ((lastrun-map (utils:read-lastrun ".lastrun"))
	 (options (utils:getopt-lastrun args options-spec lastrun-map))
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
	(install-deps-base)
	(zfs:install-deps-zfs)
	(utils:println "Finished installing all package dependencies!"))
       (else
	(when (not swap-size)
	  (error "Swap size must be specified!"))
	(when (and dev-list (not keyfile))
	  (error "Keyfile must be specified to unlock encrypted devices!"))
	(utils:write-lastrun ".lastrun" options)
	(install-deps-base)
	(cond
	 (root-dev
	  (cond
	   (boot-dev
	    (error "Separate boot device is not supported!"))
	   (else
	    (cond
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
		  (zfs:install-deps-zfs)
		  (init-zfsroot zpool rootfs swap-size
				#:swapfiles swapfiles
				#:dir-list dir-list)
		  (init-instroot-zfs
		   target boot-partdev
		   root-partdev luks-label
		   zpool rootfs dir-list
		   swap-size swapfiles
		   #:dev-list dev-list
		   #:keyfile keyfile))
		 ((< 0 swapfiles)
		  (init-instroot-swapfile
		   target boot-partdev root-partdev luks-label
		   swap-size swapfiles))
		 (else
		  (init-instroot-lvm
		   target boot-partdev root-partdev luks-label
		   swap-size))))
	      (copy-file ".lastrun" (utils:path target "CONFIG_ME"))
	      ;; to support backwards compatibility with debconf shell script
	      (utils:write-lastrun-vars (utils:path target "CONFIG_ME.sh") options)
	      (utils:println "Finished setting up installation root" target))))))
	 (else
	  (error "Block device must me specified for root filesystem!"))))))
     (else
      (error "This script must be run as root!")))))
