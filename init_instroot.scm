#!/usr/bin/guile \
-e main -s
!#

(use-modules
 (ice-9 getopt-long)
 (ice-9 hash-table)
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

(define* (usage #:rest args)
  (display "Here is some help!")
  (newline))

(define options-spec
  `((target ; default "/mnt/instroot"
     (single-char #\t)
     (value #t))
    (label ; default "crypt_root"
     (single-char #\l)
     (value #t))
    (bootdev
     (single-char #\b)
     (value #t))
    (rootdev
     (single-char #\r)
     (value #t))
    (zpool
     (single-char #\z)
     (value #t))
    (rootfs ; default "system"
     (single-char #\f)
     (value #t))
    (dirlst ; default "home,var,gnu"
     (single-char #\d)
     (value #t))
    (devlst
     (single-char #\c)
     (value #t))
    (keyfile
     (single-char #\k)
     (predicate
      ,(lambda (s) (file-exists? s)))
     (value #t))
    (genkey
     (single-char #\K)
     (predicate
      ,(lambda (s) (not (file-exists? s))))
     (value #t))
    (swapsize
     (single-char #\s)
     (predicate
      ,(lambda (s) (string-match "^[0-9]+[KMGT]?$" s)))
     (value #t))
    (swapfiles
     ;; default 0
     (single-char #\S)
     (value #t))
    (uefiboot
     (single-char #\E))
    (initdeps
     (single-char #\Z))
    (help
     (single-char #\h))))

(define* (make-optref #:optional lastrun-map)
  (if lastrun-map
      (lambda* (options key #:optional default)
	(let ((lrval (hash-ref lastrun-map key)))
	  (or lrval (option-ref options key default))))
      (lambda* (options key #:optional default)
	(option-ref options key default))))

(define (with-lastrun lr-path)
  (if (file-exists? lr-path)
      (let* ((lr-file (open-input-file lr-path))
	     (lr-map (alist->hash-table (read lr-file))))
	(close lr-file)
	(make-optref lr-map))
      (make-optref)))

(define (main args)
  (let* ((option-ref (with-lastrun ".lastrun"))
	 (options (getopt-long args options-spec))
	 (target (option-ref options 'target "/mnt/instroot"))
	 (boot-dev (option-ref options 'bootdev))
	 (root-dev (option-ref options 'rootdev))
	 (label (option-ref options 'label "crypt_root"))
	 (zpool (option-ref options 'zpool))
	 (rootfs (option-ref options 'rootfs "system"))
	 (dir-list (option-ref options 'dirlst "home,var,gnu"))
	 (keyfile (option-ref options 'keyfile))
	 (new-keyfile (option-ref options 'genkey))
	 (dev-list (option-ref options 'devlst))
	 (swap-size (option-ref options 'swapsize))
	 (swapfiles (string->number (option-ref options 'swapfiles "0")))
	 (uefiboot? (option-ref options 'uefiboot))
	 (initdeps? (option-ref options 'initdeps))
	 (help? (option-ref options 'help)))
    (cond
     (help?
      (usage))
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
	(write
	 `((target . ,target)
	   (label . ,label)
	   (bootdev . ,boot-dev)
	   (rootdev . ,root-dev)
	   (label . ,label)
	   (zpool . ,zpool)
	   (rootfs . ,rootfs)
	   (dirlst . ,dir-list)
	   (keyfile . ,keyfile)
	   (devlst . ,dev-list)
	   (swapsize . ,swap-size)
	   (swapfiles . ,(number->string swapfiles))
	   (uefiboot . ,uefiboot?))
	 lrfile)
	(close lrfile))))))
