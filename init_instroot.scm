#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((ice-9 readline))
 ((local utils) #:prefix utils:)
 ((ice-9 hash-table) #:prefix hash:)
 ((ice-9 regex) #:prefix regex:)
 ((ice-9 popen) #:prefix popen:))

(define (device-size dev)
  (let* ((dev-size (utils:process->string "blockdev" "--getsize64" dev))
	 (dev-size (regex:string-match "[0-9]+" dev-size)))
    (if dev-size
	(regex:match:substring dev-size 0)
	(error "Could not calculate device size" dev))))

(define (create-keyfile f)
  (let ((fname (basename f)))
    (if (file-exists? fname)
	(throw 'file-already-exists fname)
	(system* "dd" "if=/dev/random" (string-append "of=" fname) "bs=1024" "count=4"))))

(define (partuuid path n)
  (if (eq? 'block-special (stat:type (stat path)))
      (let ((match
	     (regex:string-match
	      "Partition unique GUID: ([0-9A-F-]+)"
	      (utils:process->string "sgdisk" "-i" (number->string n) path))))
	(if match (regex:match:substring match 1) #f))
      (error (string-append "Not a block device: " path))))

(define (which* acc args)
  (if (not (null? args))
      (let ((curr (car args)))
	(if (zero? (system* "which" curr))
	    (which* acc (cdr args))
	    (which* (cons curr acc) (cdr args))))
      acc))

(define* (which #:rest args)
  (with-output-to-file "/dev/null"
    (lambda () (which* #nil args))))

(define (install-deps-base)
  (let ((missing (which "sgdisk" "partprobe" "cryptsetup" "pv" "pvcreate" "vgcreate" "lvcreate")))
    (if (not (null? missing))
	(if (file-exists? "/etc/debian_version")
	    (begin
	      (display "Installing necessary packages...")
	      (system "apt update")
	      (system "apt install -y gdisk parted cryptsetup pv lvm2"))
	    (error "Necessary binaries are missing" missing)))))

(define* (init-boot-parts boot-dev #:key uefi?)
    (cond
     (uefi?
      (system* "sgdisk" boot-dev "-Z"
	       "-N 1"
	       "-t 1:ef00")
      (system* "partprobe" boot-dev)
      (let ((boot-partdev (string-append boot-dev "1")))
	(system* "mkfs.fat" "-F32" boot-partdev)
	(utils:println "Finished setting up partitions on:" boot-dev)
	boot-partdev))
     (else
      (system* "sgdisk" boot-dev "-Z"
	       "-n 1:0:+2M"
	       "-t 1:ef02"
	       "-N 2"
	       "-t 2:8300")
      (system* "partprobe" boot-dev)
      (let ((boot-partdev (string-append boot-dev "2")))
	(system* "mkfs.ext4 -q -m 0 -j" boot-partdev)
	(utils:println "Finished setting up partitions on:" boot-dev)
	boot-partdev))))

(define* (init-root-parts root-dev #:key boot-dev uefi?)
  (cond
   (boot-dev
    (system* "sgdisk" root-dev "-Z" "-N 1" "-t 1:8300")
    (system* "partprobe" root-dev)
    (vector
     (init-boot-parts boot-dev #:uefi? uefi?)
     (string-append root-dev "1")))
   (else
    (cond
     (uefi?
      (system* "sgdisk" root-dev "-Z"
	       "-n 1:0:+500M"
	       "-N 2"
	       "-t 1:ef00"
	       "-t 2:8300")
      (system* "partprobe" root-dev)
      (vector
       (string-append root-dev "1")
       (string-append root-dev "2")))
     (else
      (system* "sgdisk" root-dev "-Z"
	       "-n 1:0:+2M"
	       "-n 2:0:+500M"
	       "-N 3"
	       "-t 1:ef02"
	       "-t 2:8300"
	       "-t 3:8300")
      (system* "partprobe" root-dev)
      (vector
       (string-append root-dev "2")
       (string-append root-dev "3")))))))

(define (init-cryptroot partdev label)
  (utils:println "formatting" partdev "to be used as LUKS device...")
  (when (not (eqv? 0 (system* "cryptsetup" "luksFormat" partdev)))
    (error "Failed formatting of LUKS device" partdev))
  (newline)
  (utils:println "Finished formatting device" partdev "for LUKS encryption!")
  (utils:println "Opening LUKS device" partdev "as" label "...")
  (when (not (eqv? 0 (system* "cryptsetup" "luksOpen" partdev label)))
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
	(with-input-from-file "/dev/zero"
	  (lambda ()
	    (with-output-to-file luks-dev
	      (lambda () (system* "pv" "-Ss" dev-size))))))))))

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

(define (main args)
  (let* ((lastrun-map (utils:get-lastrun ".lastrun"))
	 (options (utils:getopt-lastrun args options-spec lastrun-map))
	 (target (hash-ref options 'target))
	 (boot-dev (hash-ref options 'bootdev))
	 (root-dev (hash-ref options 'rootdev))
	 (luks-label (hash-ref options 'label))
	 (zpool (hash-ref options 'zpool))
	 (rootfs (hash-ref options 'rootfs))
	 (dir-list (hash-ref options 'dirlst))
	 (keyfile (hash-ref options 'keyfile))
	 (new-keyfile (hash-ref options 'genkey))
	 (dev-list (hash-ref options 'devlst))
	 (swap-size (hash-ref options 'swapsize))
	 (swapfiles (string->number (hash-ref options 'swapfiles)))
	 (uefiboot? (hash-ref options 'uefiboot))
	 (initdeps? (hash-ref options 'initdeps))
	 (help? (hash-ref options 'help)))
    (cond
     (help?
      (display (utils:usage options-spec lastrun-map))
      (newline))
     (new-keyfile
      (create-keyfile new-keyfile))
     (else
      (when (not (utils:root-user?))
	(error "This script must be run as root!"))
      (when (not swap-size)
	(error "Swap size must be specified!"))
      (when (and dev-list (not keyfile))
	(error "Keyfile must be specified to unlock encrypted devices!"))
      (utils:write-lastrun ".lastrun" options)
      (install-deps-base)
      (cond
       (root-dev
	(let* ((parts (init-root-parts root-dev))
	       (boot-partdev (vector-ref parts 0))
	       (root-partdev (vector-ref parts 1)))
	  (init-cryptroot root-partdev luks-label)))
       (else
	(error "Block device must me specified for root filesystem!")))))))
