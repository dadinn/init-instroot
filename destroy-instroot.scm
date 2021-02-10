#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((common utils) #:prefix utils:)
 ((common deps) #:prefix deps:)
 ((ice-9 regex) #:prefix regex:)
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
     (description
      "Flag that a swapfile has been used instead of LVM or ZFS volume"))
    (uefiboot
     (description
      "Flag that UEFI boot partition has been used instead of BIOS.")
     (single-char #\E))
    (help
     (description
      "This usage help...")
     (single-char #\h))))

(define lastrun-file (utils:path ".defaults.scm"))

(define (main args)
  (let* ((lastrun-map (utils:read-config lastrun-file))
	 (options (utils:getopt-extra args options-spec lastrun-map))

	 (target (hash-ref options 'target))
	 (boot-dev (hash-ref options 'bootdev))
	 (root-dev (hash-ref options 'rootdev))
	 (luks-label (hash-ref options 'label))
	 (zpool (hash-ref options 'zpool))
	 (rootfs (hash-ref options 'rootfs))
	 (dev-list (hash-ref options 'devlst))
	 (dev-specs (if dev-list (utils:parse-pairs dev-list) #f))
	 (swapfiles (hash-ref options 'swapfiles))
	 (swapfiles (string->number swapfiles))
	 (uefiboot? (hash-ref options 'uefiboot))
	 (help? (hash-ref options 'help)))
    (cond
     (help?
       (utils:println
	(string-append "
USAGE:

" (basename (car args)) " [OPTION...]

Unmounts and destroys installation root directory, that has been set up previously by init-instroot script. Also, unmounts boot partition, closes LVM volumes and LUKS devices, and destroys all device partitions used.

When the file " lastrun-file " exists, the default values to option arguments are read from it. This file always contains the arguments given during the last execution of the init-instroot script.

Valid options are:
"))
       (utils:println (utils:usage options-spec lastrun-map)))
     ((not (utils:root-user?))
       (error "This script must be run as root!"))
     ((not (utils:directory? target))
      (error "Target directory doesn't exist!" target))
     (else
      (deps:install-deps-base)
      (when uefiboot?
	(utils:println "Unmounting /boot/efi...")
	(system* "umount" (utils:path target "boot" "efi")))
      (system* "umount" (utils:path target "boot"))
      (when boot-dev
	(utils:system->devnull* "sgdisk" "-Z" boot-dev)
	(utils:system->devnull* "partprobe" boot-dev))
      (when zpool
	(deps:install-deps-zfs)
	(system* "zfs" "destroy" "-r" (utils:path zpool "/" rootfs))
	(system* "zpool" "export" zpool))
      (when dev-specs
	(map
	 (lambda (spec)
	   (let* ((device (car spec))
		  (label (cdr spec)))
	     (system* "cryptsetup" "luksClose" label)))
	 dev-specs))
      (when root-dev
	(system* "umount" target)
	(when (< 0 swapfiles)
	  (system* "vgremove" "-f" (string-append luks-label "_vg")))
	(system* "cryptsetup" "luksClose" luks-label)
	(utils:system->devnull* "sgdisk" "-Z" root-dev)
	(utils:system->devnull* "partprobe" root-dev))))
    (when (utils:directory? target)
      (catch #t
       (lambda () (rmdir target))
       (lambda* (key #:rest args)
	(let ((resp (readline (string-append "Directory " target " is not empty. Would you still like te remove it? [y/N]"))))
	  (cond
	   ((regex:string-match "[yY]" resp)
	    (utils:println "Removing directory" target " with its content...")
	    (system* "rm" "-rf" target))
	   (else
	    (utils:println "Skipped removing" target "directory.")))))))
    (utils:println "Finished destroying initialized root structure:" target)))
