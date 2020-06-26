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
     (description
      "Flag that a swapfile has been used instead of LVM or ZFS volume")
     (default "0"))
    (help
     (description
      "This usage help...")
     (single-char #\h))))

(define lastrun-file (utils:path ".defaults.scm"))

(define (main args)
  (let* ((lastrun-map (utils:read-config lastrun-file))
	 (options (utils:getopt-extra args options-spec lastrun-map))

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
	 (dev-specs (if dev-list (utils:parse-pairs dev-list) #f))
	 (swap-size (hash-ref options 'swapsize))
	 (swapfiles (hash-ref options 'swapfiles))
	 (swapfiles (string->number swapfiles))
	 (uefiboot? (hash-ref options 'uefiboot))
	 (initdeps? (hash-ref options 'initdeps))
	 (help? (hash-ref options 'help)))
    ;; todo fix these imperative whens
    (when help?
      (utils:println
       (string-append "
USAGE:

" (basename (car args)) " [OPTIONS]

Unmounts and destroys installation root directory, that has been set up previously by init-instroot script. Also, unmounts boot partition, closes LVM volumes and LUKS devices, and destroys all device partitions used.

When the file " lastrun-file " exists, the default values to option arguments are read from it. This file always contains the arguments given during the last execution of the init-instroot script.

Valid options are:
"))
      (display (utils:usage options-spec lastrun-map))
      (newline)
      (exit 0))
    (when (not (eqv? 0 (getuid)))
      (error "This script must be run as root!"))
    (when (not instroot)
      (error "Mounted root directory is not specified!"))
    (when (not luks-label)
      (error "LUKS label is not specified!"))
    (deps:install-deps-base)
    (when zpool
     (deps:install-deps-zfs))
    (system* "umount" (string-append instroot "/boot"))
    (when boot-dev
      (utils:system->devnull* "sgdisk" "-Z" boot-dev)
      (utils:system->devnull* "partprobe" boot-dev))
    (cond
     (root-dev
      (cond
       (zpool
	(system* "zfs" "destroy" "-r" (utils:path zpool "/" rootfs))
	(system* "zpool" "export" zpool)
	(when dev-specs
	  (map
	   (lambda (spec)
	     (let* ((device (car spec))
		    (label (cdr spec)))
	       (system* "cryptsetup" "luksClose" label)))
	   dev-specs))
	(system* "umount" instroot))
       ((< 0 swapfiles)
	(system* "umount" instroot))
       (else
	(system* "umount" instroot)
	(system* "vgremove" "-f" (string-append luks-label "_vg"))))
      (system* "cryptsetup" "luksClose" luks-label)
      (utils:system->devnull* "sgdisk" "-Z" root-dev)
      (utils:system->devnull* "partprobe" root-dev))
     (zpool
      (system* "zfs" "destroy" "-r" (utils:path zpool rootfs))
      (system* "zpool" "export" zpool))
     (else
      (error "Either a root device, and/or a ZFS pool name must be specified!")))
    (when (utils:directory? instroot)
      (catch #t
       (lambda () (rmdir instroot))
       (lambda* (key #:rest args)
	(let ((delinstroot (readline (string-append "Directory " instroot " is not empty. Would you still like te remove it? [y/N]"))))
	  (cond
	   ((regex:string-match "[yY]" delinstroot)
	    (utils:println "Removing directory" instroot " with its content...")
	    (system* "rm" "-rf" instroot))
	   (else
	    (utils:println "Skipped removing" instroot "directory.")))))))
    (utils:println "Finished destroying initialized root structure:" instroot)))
