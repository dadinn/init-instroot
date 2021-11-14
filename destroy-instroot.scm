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

(define (main args)
  (let* ((options (utils:getopt-extra args options-spec))
	 (target (hash-ref options 'target))
	 (defaults
	   (utils:read-config
	    (let ((config-path (utils:path target utils:config-filename)))
	      (if (file-exists? config-path) config-path utils:config-filename))))
	 (options-ref (lambda (key) (or (hash-ref options key) (hash-ref defaults key))))
	 (boot-dev (options-ref 'bootdev))
	 (root-dev (options-ref 'rootdev))
	 (luks-label (options-ref 'label))
	 (zpool (options-ref 'zpool))
	 (rootfs (options-ref 'rootfs))
	 (dev-list (options-ref 'devlst))
	 (dev-specs (if dev-list (utils:parse-pairs dev-list) #f))
	 (swapfiles (options-ref 'swapfiles))
	 (swapfiles (if swapfiles (string->number swapfiles)))
	 (uefiboot? (options-ref 'uefiboot))
	 (help? (options-ref 'help)))
    (cond
     (help?
       (utils:println
	(string-append "
USAGE:

" (basename (car args)) " [OPTION...]

Unmounts and destroys installation root directory, that has been set up previously by init-instroot script. Also, unmounts boot partition, closes LVM volumes and LUKS devices, and destroys all device partitions used.

Valid options are:
"))
       (utils:println (utils:usage options-spec)))
     ((not (utils:root-user?))
       (error "This script must be run as root!"))
     ((not (utils:directory? target))
      (error "Target directory doesn't exist!" target))
     (else
      (deps:install-deps-base)
      (let* ((boot-dir (utils:path target "boot"))
	     (efi-dir (utils:path boot-dir "efi")))
	(when (and uefiboot? (utils:directory? efi-dir))
	  (utils:println "Unmounting /boot/efi...")
	  (system* "umount" efi-dir))
	(when (utils:directory? boot-dir)
	  (system* "umount" boot-dir)))
      (when boot-dev
	(utils:system->devnull* "sgdisk" "-Z" boot-dev)
	(utils:system->devnull* "partprobe" boot-dev))
      (when zpool
	(deps:install-deps-zfs)
	(system* "zfs" "destroy" "-r" (utils:path zpool rootfs))
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
	(when (not (< 0 swapfiles))
	  (system* "vgremove" "-f" (string-append luks-label "_vg")))
	(system* "cryptsetup" "luksClose" luks-label)
	(utils:system->devnull* "sgdisk" "-Z" root-dev)
	(utils:system->devnull* "partprobe" root-dev))
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
      (utils:println "Finished destroying initialized root structure:" target)))))
