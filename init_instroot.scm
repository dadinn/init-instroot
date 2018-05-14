#!/usr/bin/guile \
-e main -s
!#

(use-modules
 (srfi srfi-1)
 (ice-9 getopt-long)
 (ice-9 hash-table)
 (ice-9 pretty-print)
 (ice-9 regex)
 (ice-9 rdelim)
 (ice-9 popen))

(define* (process->string #:rest args)
  (let* ((command (string-join args " "))
	 (in (open-input-pipe command))
	 (text (read-string in)))
    (close in)
    text))

(define (create-keyfile f)
  (let ((fname (basename f)))
    (if (file-exists? fname)
	(throw 'file-already-exists fname)
	(system* "dd" "if=/dev/random" (string-append "of=" fname) "bs=1024" "count=4"))))

(define (partuuid path n)
  (if (eq? 'block-special (stat:type (stat path)))
      (let ((match
	     (string-match
	      "Partition unique GUID: ([0-9A-F-]+)"
	      (process->string "sgdisk -i" (number->string n) path))))
	(if match (match:substring match 1) #f))
      (error (string-append "Not a block device: " path))))

(define deps-base
  (list "sgdisk" "partprobe" "cryptsetup" "pv" "pvcreate" "vgcreate" "lvcreate"))

(define (which acc args)
  (if (not (null? args))
      (let ((curr (car args)))
	(if (zero? (system* "which" curr))
	    (which acc (cdr args))
	    (which (cons curr acc) (cdr args))))
      acc))

(define (install-deps-base)
  (let ((missing (which #nil deps-base)))
    (if (not (null? missing))
	(if (file-exists? "/etc/debian_version")
	    (begin
	      (display "Installing necessary packages...")
	      (system "apt update")
	      (system "apt install -y gdisk parted cryptsetup pv lvm2"))
	    (error "Necessary binaries are missing" missing)))))

(define (usage specs lastrun)
  (string-join
   (map
    (lambda (spec)
      (let* ((long-name (car spec))
	     (props (cdr spec))
	     (single-char (assoc-ref props 'single-char))
	     (description (assoc-ref props 'description))
	     (value (assoc-ref props 'value))
	     (value-arg (assoc-ref props 'value-arg))
	     (default (assoc-ref props 'default))
	     (lastrun (hash-ref lastrun long-name)))
	(string-append
	 (if single-char
	     (string #\- (car single-char)))
	 " "
	 (string-append "--" (symbol->string long-name))
	 (if value
	     (if value-arg
		 (string-append " " (car value-arg) "\n")
		 " ARG\n")
	     "\n")
	 (if description (car description) "NO DESCRIPTION")
	 (if value
	     (cond
	      (lastrun (string-append " (default " lastrun ")"))
	      (default (string-append " (default " (car default) ")"))
	      (else ""))
	     (if (or lastrun default) " (default)" "")))))
    specs)
   "\n\n"))

(define options-spec
  `((target
     (single-char #\t)
     (description
      "Root mountpoint for installation")
     (default "/mnt/instroot")
     (value #t))
    (label
     (single-char #\l)
     (description
      "LUKS encrypted device name for root")
     (default "crypt_root")
     (value #t))
    (bootdev
     (single-char #\b)
     (description
      "Use separate boot device for /boot and insalling GRUB")
     (value #t))
    (rootdev
     (single-char #\r)
     (description
      "Device to use for root filesystem")
     (value #t))
    (zpool
     (single-char #\z)
     (description
      "ZFS pool name for system directories and swap device")
     (value #t))
    (rootfs
     (single-char #\f)
     (description
      "Name of the system root dataset in the ZFS pool")
     (default "system")
     (value #t))
    (dirlst
     (single-char #\d)
     (description
      "Coma separated list of root directories to mount as ZFS datasets")
     (default "home,var,gnu")
     (value #t))
    (devlst
     (single-char #\c)
     (description
      "Coma separeted list of colon separated pairs of other encrypted devices")
     (value #t))
    (keyfile
     (single-char #\k)
     (description
      "Keyfile used to decrypt other encrypted devices (i.e. ZFS pool members)")
     (predicate
      ,(lambda (s) (file-exists? s)))
     (value #t))
    (genkey
     (single-char #\K)
     (description
      "Generate new keyfile with the given value as filename")
     (predicate
      ,(lambda (s) (not (file-exists? s))))
     (value #t))
    (swapsize
     (single-char #\s)
     (description
      "Size of the total swap space to use (KMGT suffixes allowed)")
     (predicate
      ,(lambda (s) (string-match "^[0-9]+[KMGT]?$" s)))
     (value #t))
    (swapfiles
     (single-char #\S)
     (description
      "Number of swapfiles to use to break total swap-space up into. Swapfiles are created
in equally sized chunks. COUNT zero means to use LVM volumes instead of swapfiles.")
     (default "0")
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

(define supported-props
  (alist->hash-table
   (map
    (lambda (k) (cons k #t))
    '(single-char value required? predicate))))

(define (conform-props props)
  (fold
   (lambda (kv new-props)
     (if (hash-ref supported-props (car kv))
	 (cons kv new-props)
	 new-props))
   #nil
   props))

(define (conform-spec spec)
  (map
   (lambda (kv)
     (cons
      (car kv)
      (conform-props (cdr kv))))
   spec))

(define (getopt-long args options-spec)
  ((@ (ice-9 getopt-long) getopt-long)
   args
   (conform-spec options-spec)))

(define* (make-optref spec lastrun)
  (lambda* (options key)
    (let* ((default (assoc-ref (assoc-ref spec key) 'default))
	   (default (hash-ref lastrun key (and default (car default)))))
      (option-ref options key default))))

(define (with-defaults spec lastrun)
  (if lastrun
      (make-optref spec lastrun)
      (make-optref spec (make-hash-table 0))))

(define (get-lastrun path)
  (if (file-exists? path)
      (let* ((lr-file (open-input-file path))
	     (lr-alist
	      (map
	       (lambda (kv) (cons (car kv) (cadr kv)))
	       (read lr-file))))
	(close lr-file)
	(alist->hash-table lr-alist))
      #f))

(define (main args)
  (let* ((lastrun-map (get-lastrun ".lastrun"))
	 (option-ref (with-defaults options-spec lastrun-map))
	 (options (getopt-long args options-spec))
	 (target (option-ref options 'target))
	 (boot-dev (option-ref options 'bootdev))
	 (root-dev (option-ref options 'rootdev))
	 (label (option-ref options 'label))
	 (zpool (option-ref options 'zpool))
	 (rootfs (option-ref options 'rootfs))
	 (dir-list (option-ref options 'dirlst))
	 (keyfile (option-ref options 'keyfile))
	 (new-keyfile (option-ref options 'genkey))
	 (dev-list (option-ref options 'devlst))
	 (swap-size (option-ref options 'swapsize))
	 (swapfiles (string->number (option-ref options 'swapfiles)))
	 (uefiboot? (option-ref options 'uefiboot))
	 (initdeps? (option-ref options 'initdeps))
	 (help? (option-ref options 'help)))
    (cond
     (help?
      (display (usage options-spec lastrun-map))
      (newline))
     (new-keyfile
      (create-keyfile new-keyfile))
     (else
      (let* ((id-res (process->string "id -u"))
	     (id-match (string-match "[0-9]+" id-res))
	     (id-str (match:substring id-match))
	     (id (string->number id-str)))
	(if (not (eqv? 0 id))
	    (error "This script must be run as root!")))
      (if
       (not swap-size)
       (error "Swap size must be specified!"))
      (if
       (and dev-list (not keyfile))
       (error "Keyfile must be specified to unlock encrypted devices!"))
      (let ((lrfile (open-output-file ".lastrun")))
	(pretty-print
	 `((target ,target)
	   (label ,label)
	   (bootdev ,boot-dev)
	   (rootdev ,root-dev)
	   (label ,label)
	   (zpool ,zpool)
	   (rootfs ,rootfs)
	   (dirlst ,dir-list)
	   (keyfile ,keyfile)
	   (devlst ,dev-list)
	   (swapsize ,swap-size)
	   (swapfiles ,(number->string swapfiles))
	   (uefiboot ,uefiboot?))
	 lrfile)
	(close lrfile))))))
