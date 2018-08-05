#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((local utils) #:prefix utils:)
 ((ice-9 readline)))

(define options-spec
  `((target
     (single-char #\m)
     (description
      "Root mountpoint for installation")
     (default "/mnt/instroot")
     (value-arg "PATH")
     (value #t))
    (label
     (single-char #\l)
     (description
      "LUKS encrypted root device name")
     (default "crypt_root")
     (value-arg "LABEL")
     (value #t))
    (bootdev
     (single-char #\b)
     (description
      "Device for /boot partition")
     (predicate ,utils:block-device?)
     (value-arg "DEVICE")
     (value #t))
    (rootdev
     (single-char #\r)
     (description
      "Device to use for root filesystem")
     (predicate ,utils:block-device?)
     (value-arg "DEVICE")
     (value #t))
    (zpool
     (single-char #\z)
     (description
      "ZFS pool name for system directories and swap device")
     (value-arg "ZPOOL")
     (value #t))
    (rootfs
     (single-char #\f)
     (description
      "Name of the system root dataset in the ZFS pool")
     (default "system")
     (value-arg "NAME")
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
     (value-arg "DEVLIST")
     (value #t))
    (swapfiles
     (single-char #\S)
     (default "0")
     (description
      "Flag that a swapfile has been used instead of LVM or ZFS volume"))
    (help
     (description
      "This usage help...")
     (single-char #\h))))

(define (main args)
  (let* ((lastrun-map (utils:get-lastrun ".lastrun"))
	 (options (utils:getopt-lastrun args options-spec lastrun-map))

	 (instroot (hash-ref options 'target))
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
	 (dev-list (if dev-list (string-split dev-list #\,) #f))
	 (swap-size (hash-ref options 'swapsize))
	 (swapfiles (hash-ref options 'swapfiles))
	 (swapfiles (if swapfiles (string->number swapfiles) 0))
	 (uefiboot? (hash-ref options 'uefiboot))
	 (initdeps? (hash-ref options 'initdeps)))
    (when (not root-dev)
      (error "Root device is not specified!"))
    (when (not luks-label)
      (error "LUKS label is not specified!"))
    (when (not instroot)
      (error "Mounted root directory is not specified!"))
    (when (not (eqv? 0 (getuid)))
      (error "This script must be run as root!"))
    (system* "umount" (string-append instroot "/boot"))
    (when boot-dev
      (with-output-to-file "/dev/null"
	(lambda ()
	  (system* "sgdisk" "-Z" boot-dev)
	  (system* "partprobe" boot-dev))))
    (cond
     (zpool
      (system* "zfs" "destroy" "-r" (string-append zpool "/" rootfs))
      (system* "zpool" "export" zpool)
      (system* "umount" instroot)
      (map
       (lambda (dev)
	 (let* ((split (string-split dev #\:))
		(device (car split))
		(label (cdr split)))
	   (system* "cryptsetup" "luksClose" label)))))
     (swapfiles
      (system* "umount" instroot))
     (else
      (system* "umount" instroot)
      (system* "vgremove" "-f" (string-append luks-label "_vg"))))
    (when root-dev
      (system* "cryptsetup" "luksClose" luks-label)
      (with-output-to-file "/dev/null"
	(lambda ()
	  (system* "sgdisk" "-Z" root-dev)
	  (system* "partprobe" root-dev))))
    (when (and (file-exists? instroot)
	       (eq? 'directory (stat:type (stat instroot))))
      (rmdir instroot)
      (when (file-exists? instroot)
	(let ((delinstroot (readline (string-append "Directory " instroot " is not empty. Would you still like te remove it? [y/N]"))))
	  (cond
	   ((regex:string-match "[yY]" delinstroot)
	    (utils:println "Removing directory" instroot " with its content...")
	    (system* "rm" "-rf" instroot))
	   (else
	    (utils:println "Skipped removing" instroot "directory."))))))
    (utils:println "Finished destroying initialized root structure:" instroot)))
