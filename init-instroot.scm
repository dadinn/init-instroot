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

(define (part-probe boot-dev)
  (system* "partprobe" boot-dev)
  ;; needs some delay to avoid timing issues
  (sleep 1))

(define (init-boot-parts-bios boot-dev)
  (system* "sgdisk" boot-dev "-Z"
	   "-n" "1:0:+2M"
	   "-t" "1:ef02"
	   "-N" "2"
	   "-t" "2:8300")
  (part-probe boot-dev)
  (let ((boot-partdev (partdev boot-dev "2"))
	(result (make-hash-table 3)))
    (utils:println "Formatting boot partition device as EXT4:" boot-partdev)
    (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" boot-partdev)))
      (error "Failed to create EXT4 filesystem on:" boot-partdev))
    (hash-set! result 'boot boot-partdev)
    result))

(define (init-boot-parts-uefi boot-dev)
  (system* "sgdisk" boot-dev "-Z"
	   "-n" "1:0:+50M"
	   "-t" "1:ef00"
	   "-N" "2"
	   "-t" "2:8300")
  (part-probe boot-dev)
  (let ((uefi-partdev (partdev boot-dev "1"))
	(boot-partdev (partdev boot-dev "2"))
	(result (make-hash-table 3)))
    (utils:println "Formatting boot partition device as FAT32:" boot-partdev)
    (when (not (zero? (system* "mkfs.fat" "-F32" uefi-partdev)))
      (error "Failed to create FAT32 filesystem on:" uefi-partdev))
    (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" boot-partdev)))
      (error "Failed to create EXT4 filesystem on:" boot-partdev))
    (hash-set! result 'uefi uefi-partdev)
    (hash-set! result 'boot boot-partdev)
    result))

(define* (init-boot-parts boot-dev #:optional uefiboot?)
  (let ((result
	 (if uefiboot?
	     (init-boot-parts-uefi boot-dev)
	     (init-boot-parts-bios boot-dev))))
    (utils:println "Finished setting up partitions on:" boot-dev)
    result))

(define* (init-root-parts root-dev #:key boot-dev uefiboot?)
  (cond
   (boot-dev
    (system* "sgdisk" root-dev "-Z" "-N" "1" "-t" "1:8300")
    (part-probe root-dev)
    (let ((result (init-boot-parts boot-dev uefiboot?)))
      (hash-set! result 'root (partdev root-dev "1"))
      result))
   (uefiboot?
    (system* "sgdisk" root-dev "-Z"
	     "-n" "1:0:+50M"
	     "-n" "2:0:+500M"
	     "-N" "3"
	     "-t" "1:ef00"
	     "-t" "2:8300"
	     "-t" "3:8300")
    (part-probe root-dev)
    (let ((uefi-partdev (partdev root-dev "1"))
	  (boot-partdev (partdev root-dev "2"))
	  (root-partdev (partdev root-dev "3"))
	  (result (make-hash-table 3)))
      (utils:println "Formatting UEFI partition device as FAT32:" uefi-partdev)
      (when (not (zero? (system* "mkfs.fat" "-F32" uefi-partdev)))
	(error "Failed to create FAT32 filesystem on:" uefi-partdev))
      (utils:println "Formatting boot partition device as EXT4:" boot-partdev)
      (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" boot-partdev)))
	(error "Failed to create EXT4 filesystem on:" boot-partdev))
      (hash-set! result 'uefi uefi-partdev)
      (hash-set! result 'boot boot-partdev)
      (hash-set! result 'root root-partdev)
      result))
   (else
    (system* "sgdisk" root-dev "-Z"
	     "-n" "1:0:+2M"
	     "-n" "2:0:+500M"
	     "-N" "3"
	     "-t" "1:ef02"
	     "-t" "2:8300"
	     "-t" "3:8300")
    (part-probe root-dev)
    (let ((boot-partdev (partdev root-dev "2"))
	  (root-partdev (partdev root-dev "3"))
	  (result (make-hash-table 2)))
      (utils:println "Formatting boot partition device as EXT4:" boot-partdev)
      (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" boot-partdev)))
	(error "Failed to create EXT4 filesystem on:" boot-partdev))
      (hash-set! result 'boot boot-partdev)
      (hash-set! result 'root root-partdev)
      result))))

(define* (init-cryptroot partdev label #:key luks-v2?)
  (utils:println "Formatting" partdev "to be used as LUKS device...")
  (when (not (zero? (system* "cryptsetup" "luksFormat" "--type" (if luks-v2? "luks2" "luks1") partdev)))
    (error "Failed formatting of LUKS device" partdev))
  (newline)
  (utils:println "Finished formatting device" partdev "for LUKS encryption!")
  (utils:println "Opening LUKS device" partdev "as" label "...")
  (when (not (zero? (system* "cryptsetup" "luksOpen" partdev label)))
    (error "Failed to open LUKS device:" label))
  (newline)
  (utils:println "It is recommended to overwrite a new LUKS device with random data.")
  (utils:println "WARNING: This can take quite a long time!")
  (let ((resp (readline "Would you like to overwrite LUKS device with random data? [y/N]")))
    (cond
     ((regex:string-match "[yY]" resp)
      (utils:println "Shredding LUKS device...")
      (let* ((luks-dev (string-append "/dev/mapper/" label))
	     (dev-size (device-size luks-dev)))
	(system* "dd" "if=/dev/zero" (string-append "of=" luks-dev) "status=progress")))
     (else
      (utils:println "Skipped shredding of LUKS device.")))))

(define (init-cryptdevs keyfile dev-list)
  (for-each
   (lambda (s)
     (let* ((m (string-split s #\:))
	    (device (car m))
	    (label (cadr m)))
       (when (not (file-exists? (string-append "/dev/mapper/" label)))
	 (system* "cryptsetup" "luksOpen" "--key-file" keyfile device label))))
   (string-split dev-list #\,)))

(define (modprobe? module)
  (zero? (utils:system->devnull* "modprobe" module)))

(define (load-zfs-kernel-module)
  (when (not (modprobe? "zfs"))
    (error "ZFS kernel modules are not loaded!")))

(define (reimport-and-check-pool zpool)
  (when (zero? (utils:system->devnull* "zpool" "list" zpool))
    (when (not (zero? (utils:system->devnull* "zpool" "export" zpool)))
      (error "Failed to export ZFS pool:" zpool)))
  (when (not (zero? (utils:system->devnull* "zpool" "import" zpool)))
    (error "Failed to import ZFS pool:" zpool))
  (when (not (zero? (utils:system->devnull* "zpool" "list" zpool)))
    (error "Cannot find ZFS pool:" zpool))
  ;; force loading encryption keys for root dataset
  (system* "zfs" "load-key" zpool))

(define (init-zpool name vdevs)
  (utils:println "Creating ZFS pool:" name)
  (if (zero? (apply system*
     "zpool" "create" "-f"
     "-o" "ashift=12"
     ;; encryption options
     "-O" "encryption=aes-128-gcm"
     "-O" "pbkdf2iters=1000000"
     "-O" "keyformat=passphrase"
     "-O" "keylocation=prompt"
     ;; filesystem options
     "-O" "normalization=formD"
     "-O" "atime=off"
     "-O" "devices=off"
     "-O" "acltype=posixacl"
     "-O" "xattr=sa"
     name vdevs))
   (utils:println "Finished creating ZFS pool:" name)
   (error "Failed to create ZFS pool:" name)))

(define* (init-zfsroot zpool rootfs #:key swap-size zdirs)
  (reimport-and-check-pool zpool)
  (let* ((root-dataset (utils:path zpool rootfs))
	 (swap-dataset (utils:path root-dataset "swap"))
	 (swap-zvol (utils:path "" "dev" "zvol" swap-dataset)))
    (when (zero? (utils:system->devnull* "zfs" "list" root-dataset))
      (error "root dataset already exists!" root-dataset))
    (utils:println "Creating root ZFS dataset" root-dataset "...")
    (when (not (zero?
    (system* "zfs" "create" "-o" "compression=lz4" root-dataset)))
      (error "Failed creating dataset" root-dataset))
    (for-each
     (lambda (dir-name)
       (utils:system->devnull* "zfs" "create" (utils:path root-dataset dir-name)))
     zdirs)
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
    (for-each
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
  (let* ((swap-dir (utils:path root-dir "swap"))
	 (pagesize (utils:system->string* "getconf" "PAGESIZE"))
	 (pagesize (regex:string-match "([0-9]+)" pagesize))
	 (pagesize (regex:match:substring pagesize 1))
	 (pagesize (string->number pagesize)))
    (when (not (file-exists? swap-dir))
      (mkdir swap-dir))
    (for-each
     (lambda (args)
       (let* ((filename (car args))
	      (filesize (cadr args))
	      (swapfile (utils:path swap-dir filename)))
	 (when (< filesize (* 10 pagesize))
	   (utils:println "ERROR: Swapfile size must be at least 10 times the virtual memory page size:" (number->string (* 10 pagesize)) "bytes!")
	       (error "Swapfile size is too small:" filesize))
	 (utils:println "Allocating" (number->string filesize) "of swap space in" swapfile "...")
	 (system* "dd" "if=/dev/zero"
		  (string-append "of=" swapfile)
		  (string-append "bs=" (number->string filesize))
		  "count=1" "status=progress")
	 (chmod swapfile #o600)
	 (utils:system->devnull* "mkswap" swapfile)
	 (if (zero? (utils:system->devnull* "swapon" swapfile))
	     (utils:system->devnull* "swapoff" swapfile)
	     (utils:println "WARNING:" swapfile "failed to swap on!"))))
     swapfile-args)))

(define (print-fstab-entry-root root-dev)
  (utils:println (string-append "UUID=" (fsuuid root-dev)) "/" "ext4" "errors=remount-ro,noatime" "0" "1"))

(define* (print-fstab-entry-boot boot-dev #:optional uefi-dev)
  (utils:println (string-append "UUID=" (fsuuid boot-dev)) "/boot" "ext4" "defaults,noatime" "0" "2")
  (when uefi-dev
    (utils:println (string-append "UUID=" (fsuuid uefi-dev)) "/boot/efi" "vfat" "defaults,noatime" "0" "1")))

(define (print-fstab-headers)
  (newline)
  (utils:println "# <file system> <mountpoint> <type> <options> <dump> <pass>")
  (newline))

(define-syntax-rule (print-fstab* output-file body ...)
  (with-output-to-file output-file
    (lambda ()
      (print-fstab-headers)
      body ...
      (newline)
      (utils:println "tmpfs" "/tmp" "tmpfs" "defaults" "0" "0"))))

(define (parse-dev-list dev-list)
  (map
   (lambda (s) (string-split s #\:))
   (string-split dev-list #\,)))

(define (backup-header headers-dir device label)
  (let ((file (utils:path headers-dir label)))
    (with-output-to-file "/dev/null"
      (lambda ()
	(system* "cryptsetup" "luksHeaderBackup" device
		 "--header-backup-file" file)
	(chmod file #o400)
	(system* "chattr" "+i" file)))))

(define* (backup-headers headers-dir #:key luks-partdev luks-label dev-list)
  (when luks-label
   (backup-header headers-dir luks-partdev luks-label))
  (when dev-list
   (for-each
    (lambda (args)
      (let ((device (car args))
	    (label (cadr args)))
       (backup-header headers-dir device label)))
    (parse-dev-list dev-list))))

(define* (print-crypttab output-file luks-partdev luks-label #:key keyfile dev-list)
  (with-output-to-file output-file
    (lambda ()
      ;; ROOOTDEV
      (utils:println "# LUKS device containing root filesystem")
      (utils:println luks-label (string-append "UUID=" (fsuuid luks-partdev)) "none" "luks")
      ;; DEVLISTS
      (when (and keyfile dev-list)
	(newline)
	(utils:println "# LUKS devices containing encrypted ZFS vdevs")
	(newline)
	(for-each
	 (lambda (args)
	   (let ((device (car args))
		 (label (cadr args)))
	     (utils:println label (string-append "UUID=" (fsuuid device)) keyfile "luks")))
	 (parse-dev-list dev-list))))))

(define*
  (init-instroot
   target
   #:key
   boot-dev uefiboot?
   root-dev luks-label luks-v2?
   dev-list keyfile
   zpool rootfs zdirs
   swap-size swapfiles)
  (deps:install-deps-base)
  (when (file-exists? target)
    (error "Target" target "already exists!"))
  (mkdir target)
  (let* ((boot-dir (utils:path target "boot"))
	 (uefi-dir (utils:path boot-dir "efi"))
	 (etc-dir (utils:path target "etc"))
	 (root-dir (utils:path target "root"))
	 (crypt-dir (utils:path root-dir "crypt"))
	 (headers-dir (utils:path crypt-dir "headers")))
    (cond
     (root-dev
      (let* ((parts (init-root-parts root-dev #:uefiboot? uefiboot? #:boot-dev boot-dev))
	     (uefi-partdev (hash-ref parts 'uefi))
	     (boot-partdev (hash-ref parts 'boot))
	     (luks-partdev (hash-ref parts 'root)))
	(init-cryptroot luks-partdev luks-label #:luks-v2? luks-v2?)
	(cond
	 (zpool
	  (when (and keyfile dev-list)
	    (init-cryptdevs keyfile dev-list))
	  (deps:install-deps-zfs)
	  (load-zfs-kernel-module)
	  (init-zfsroot zpool rootfs #:zdirs zdirs)
	  (let* ((systemfs (utils:path zpool rootfs))
		 (luks-dev (utils:path "/dev/mapper" luks-label))
		 (keyfile-stored (if keyfile (utils:path crypt-dir (basename keyfile)) #f)))
	    (utils:println "Formatting LUKS device" luks-label "with ext4 to be used as root filesystem...")
	    (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" luks-dev)))
	      (error "Failed to create EXT4 filesystem on:" luks-dev))
	    (utils:println "Mounting LUKS root filesystem...")
	    (when (not (zero? (system* "mount" luks-dev target)))
	      (error "Failed to mount" luks-dev "as" target))
	    (utils:println "Mounting all ZFS root directories...")
	    (system* "zfs" "set" (string-append "mountpoint=" target) systemfs)
	    (mkdir boot-dir)
	    (when (not (zero? (system* "mount" boot-partdev boot-dir)))
	      (error "Failed to mount" boot-partdev "as" boot-dir))
	    (when uefiboot?
	      (mkdir uefi-dir)
	      (when (not (zero? (system* "mount" uefi-partdev uefi-dir)))
		(error "Failed to mount" uefi-partdev "as" uefi-dir)))
	    (mkdir etc-dir)
	    (if (file-exists? root-dir)
		(chmod root-dir #o700)
		(mkdir root-dir #o700))
	    (mkdir crypt-dir)
	    (mkdir headers-dir)
	    (backup-headers
	     headers-dir
	     #:luks-partdev luks-partdev
	     #:luks-label luks-label
	     #:dev-list dev-list)
	    (when keyfile-stored
	      (copy-file keyfile keyfile-stored)
	      (chmod keyfile-stored #o400))
	    (print-crypttab
	     (utils:path etc-dir "crypttab")
	     luks-partdev luks-label
	     #:dev-list dev-list
	     #:keyfile keyfile-stored)
	    (print-fstab*
	     (utils:path etc-dir "fstab")
	     (print-fstab-entry-root (utils:path "/dev/mapper" luks-label))
	     (print-fstab-entry-boot boot-partdev uefi-partdev)
	     (utils:println (utils:path "/dev/zvol" zpool rootfs "swap") "none" "swap" "sw" "0" "0")
	     (newline)
	     (utils:println "# systemd specific legacy mounts of ZFS datasets")
	     (newline)
	     (for-each
	      (lambda (zdir)
		(utils:println "#" (utils:path zpool rootfs zdir) (utils:path "" zdir) "zfs" "defaults,x-systemd.after=zfs.target" "0" "0"))
	      zdirs))))
	 ((< 0 swapfiles)
	  (utils:println "Setting up installation root with swapfile for swap space...")
	  (utils:println "Formatting LUKS device" luks-label "with ext4 to be used as root filesystem...")
	  (let ((luks-dev (utils:path "/dev/mapper" luks-label)))
	    (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" luks-dev)))
	      (error "Failed to create EXT4 filesystem on:" luks-dev))
	    (when (not (zero? (system* "mount" luks-dev target)))
	      (error "Failed to mount" luks-dev "as" target)))
	  (mkdir boot-dir)
	  (when (not (zero? (system* "mount" boot-partdev boot-dir)))
	    (error "Failed to mount" boot-partdev "as" boot-dir))
	  (when uefiboot?
	    (mkdir uefi-dir)
	    (when (not (zero? (system* "mount" uefi-partdev uefi-dir)))
	      (error "Failed to mount" uefi-partdev "as" uefi-dir)))
	  (let ((swapfile-args (parse-swapfile-args swap-size swapfiles)))
	    (mkdir etc-dir)
	    (mkdir root-dir #o700)
	    (mkdir crypt-dir)
	    (mkdir headers-dir)
	    (backup-headers headers-dir
			    #:luks-partdev luks-partdev
			    #:luks-label luks-label)
	    (init-swapfiles root-dir swapfile-args)
	    (print-crypttab
	     (utils:path etc-dir "crypttab")
	     luks-partdev luks-label)
	    (print-fstab*
	     (utils:path etc-dir "fstab")
	     (print-fstab-entry-root (utils:path "/dev/mapper" luks-label))
	     (print-fstab-entry-boot boot-partdev uefi-partdev)
	     (newline)
	     (utils:println "#swapfiles")
	     (for-each
	      (lambda (args)
		(let* ((filename (car args))
		       (file-path (utils:path "/root/swap" filename)))
		  (utils:println file-path "none" "swap" "sw" "0" "0")))
	      swapfile-args))))
	 (else
	  (deps:install-deps-lvm)
	  (let* ((luks-dev (utils:path "/dev/mapper" luks-label))
		 (vg-name (string-append luks-label "_vg"))
		 (lv-root (string-append "/dev/mapper/" vg-name "-root"))
		 (lv-swap (string-append "/dev/mapper/" vg-name "-swap")))
	    (utils:println "Setting up LVM with volumes for root and swap filesystems...")
	    (system* "pvcreate" luks-dev)
	    (system* "vgcreate" vg-name luks-dev)
	    (system* "lvcreate" "-L" swap-size  "-n" "swap" vg-name)
	    (system* "lvcreate" "-l" "100%FREE" "-n" "root" vg-name)
	    (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" lv-root)))
	      (error "Failed to create EXT4 filesystem on:" lv-root))
	    (when (not (zero? (system* "mount" lv-root target)))
	      (error "Failed to mount" target))
	    (mkdir boot-dir)
	    (when (not (zero? (system* "mount" boot-partdev boot-dir)))
	      (error "Failed to mount" boot-partdev "as" boot-dir))
	    (when uefiboot?
	      (mkdir uefi-dir)
	      (when (not (zero? (system* "mount" uefi-partdev uefi-dir)))
		(error "Failed to mount" uefi-partdev "as" uefi-dir)))
	    (mkdir etc-dir)
	    (mkdir root-dir #o700)
	    (mkdir crypt-dir)
	    (mkdir headers-dir)
	    (backup-headers headers-dir
			    #:luks-partdev luks-partdev
			    #:luks-label luks-label)
	    (utils:println "Formatting" lv-swap "to be used as swap space...")
	    (utils:system->devnull* "mkswap" lv-swap)
	    (if (zero? (utils:system->devnull* "swapon" lv-swap))
		(utils:system->devnull* "swapoff" lv-swap)
		(utils:println "WARNING:" "failed to swap on" lv-swap))
	    (print-crypttab
	     (utils:path etc-dir "crypttab")
	     luks-partdev luks-label)
	    (print-fstab*
	     (utils:path etc-dir "fstab")
	     (print-fstab-entry-root lv-root)
	     (print-fstab-entry-boot boot-partdev uefi-partdev)
	     (utils:println (string-append "UUID=" (fsuuid lv-swap)) "none" "swap" "sw" "0" "0")))))))
     (zpool
      (when (not boot-dev)
	(error "Separate boot device must be specified when using ZFS as root!"))
      (deps:install-deps-zfs)
      (load-zfs-kernel-module)
      (let* ((parts (init-boot-parts boot-dev uefiboot?))
	     (uefi-partdev (hash-ref parts 'uefi))
	     (boot-partdev (hash-ref parts 'boot))
	     (systemfs (utils:path zpool rootfs)))
	(init-zfsroot
	 zpool rootfs
	 #:swap-size swap-size
	 #:zdirs zdirs)
	(utils:println "Mounting ZFS root...")
	(system* "zpool" "set" (string-append "bootfs=" systemfs) zpool)
	(system* "zfs" "umount" "-a")
	(system* "zfs" "set" (string-append "mountpoint=" target) systemfs)
	(system* "zfs" "mount" "-a")
	(system* "mount" "-o" "remount,exec,dev" target)
	(mkdir boot-dir)
	(when (not (zero? (system* "mount" boot-partdev boot-dir)))
	  (error "Failed to mount" boot-partdev "as" boot-dir))
	(when uefiboot?
	  (mkdir uefi-dir)
	  (when (not (zero? (system* "mount" uefi-partdev uefi-dir)))
	    (error "Failed to mount" uefi-partdev "as" uefi-dir)))
	(mkdir etc-dir)
	(print-fstab*
	 (utils:path etc-dir "fstab")
	 (utils:println (utils:path "/dev/zvol" zpool rootfs "swap") "none" "swap" "sw" "0" "0")
	 (print-fstab-entry-boot boot-partdev uefi-partdev))))
     (else
      (error "Either block device (for using LUKS encryption), or a ZFS pool (using native ZFS encrption) must be specified for root!")))))

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
      "Device to use for LUKS encrypted root filesystem")
     (predicate ,utils:block-device?)
     (value-arg "device")
     (value #t))
    (zpool
     (single-char #\z)
     (description
      "ZFS pool to use as root device, if no root device is specified (separate boot device must be specified too).
      Alternatively, if a separate root device is specified too, the pool is used for additional system directories and swap device.")
     (value-arg "zpool")
     (value #t))
    (rootfs
     (single-char #\f)
     (description
      "Name of the system root dataset in the ZFS pool")
     (default "system")
     (value-arg "name")
     (value #t))
    (zdirs
     (single-char #\d)
     (description
      "Coma separated list of root directories to mount as ZFS datasets")
     (default "home,var,var/lib")
     (value-arg "dirs")
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
      "Generate new keyfile with given name in current directory.")
     (predicate
      ,(lambda (s) (equal? s (basename s))))
     (value-arg "filename")
     (value #t))
    (swapsize
     (single-char #\s)
     (description
      "Size of the total swap space to use (KMGTPEZY binary unit suffixes allowed)")
     (predicate
      ,(lambda (s) (regex:string-match "^[0-9]+[KMGTPEZY]?$" s)))
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
    (luksv2
     (description
      "Use LUKS format version 2 to encrypt root filesystem")
     (single-char #\L))
    (init-zpool
     (description
      "Install and configure necessary ZFS dependencies only, then exit.")
     (single-char #\Z))
    (help
     (description
      "This usage help...")
     (single-char #\h))))

(define (create-keyfile filename)
  (when (file-exists? filename)
    (error "Cannot create keyfile! File already exists:" filename))
  (system* "dd" "if=/dev/random" (string-append "of=" filename) "bs=1024" "count=4")
  (chmod filename #o400)
  (utils:println "Finished creating keyfile:" filename))

(define (main args)
  (let* ((options (utils:getopt-extra args options-spec))
	 (target (hash-ref options 'target))
	 (boot-dev (hash-ref options 'bootdev))
	 (root-dev (hash-ref options 'rootdev))
	 (luks-label (hash-ref options 'label))
	 (zpool (hash-ref options 'zpool))
	 (rootfs (hash-ref options 'rootfs))
	 (zdirs (hash-ref options 'zdirs))
	 (zdirs (and zdirs (string-split zdirs #\,)))
	 (keyfile (hash-ref options 'keyfile))
	 (new-keyfile (hash-ref options 'genkey))
	 (dev-list (hash-ref options 'devlst))
	 (dev-list (and dev-list (utils:parse-pairs dev-list)))
	 (swap-size (hash-ref options 'swapsize))
	 (swapfiles (hash-ref options 'swapfiles))
	 (swapfiles (and swapfiles (string->number swapfiles)))
	 (luks-v2? (hash-ref options 'luksv2))
	 (uefiboot? (hash-ref options 'uefiboot))
	 (init-zpool? (hash-ref options 'init-zpool))
	 (help? (hash-ref options 'help)))
    (cond
     (help?
      (display
       (string-append "
USAGE:

" (basename (car args)) " [OPTION...]

Initialises and mounts a root filesystem.

Uses LUKS encrypted and EXT4 formatted root filesystem. Allows using either plain files, or LVM for swap space. Supports both BIOS or UEFI boot partitions. Also, using a separate boot device is an option.

Alternatively, a ZFS pool can be used for both root filesystem and swap space. Using ZFS pool as root filesystem requires a separate boot device.

Valid options are:

" (utils:usage options-spec)))
      (newline)
      (newline))
     (new-keyfile
      (create-keyfile new-keyfile))
     ((not (utils:root-user?))
      (error "This script must be run as root!"))
     (init-zpool?
      (deps:install-deps-base)
      (deps:install-deps-zfs)
      (load-zfs-kernel-module)
      (let ((args (hash-ref options '())))
	(cond
	 ((not (nil? args))
	  (let ((name (car args))
		(vdevs (cdr args)))
	    (init-zpool name vdevs)))
	 (else
	  (utils:println "Finished installing all package dependencies!")))))
     ((not swap-size)
      (error "Swap size must be specified!"))
     ((and dev-list (not keyfile))
      (error "Keyfile must be specified to unlock additional LUKS encrypted devices!"))
     ((and luks-v2? (<= 10 (or (deps:read-debian-version) 0)))
      (error "LUKS format version 2 is only supported in Debian Buster or later!"))
     ((and uefiboot? (not (modprobe? "efivars")))
      (error "Cannot use UEFI boot, when efivars module is not loaded!"))
     (else
      (utils:write-config utils:config-filename options)
      (init-instroot target
       #:boot-dev boot-dev
       #:uefiboot? uefiboot?
       #:root-dev root-dev
       #:luks-label luks-label
       #:luks-v2? luks-v2?
       #:dev-list dev-list
       #:keyfile keyfile
       #:zpool zpool
       #:rootfs rootfs
       #:zdirs zdirs
       #:swap-size swap-size
       #:swapfiles swapfiles)
      (utils:move-file utils:config-filename (utils:path target utils:config-filename))
      (utils:println "Finished setting up installation root" target)))))
