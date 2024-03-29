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

(define* (init-boot-parts-bios boot-dev #:optional force?)
  (system* "sgdisk" boot-dev "-Z"
           "-n" "1:0:+2M"
           "-t" "1:ef02"
           "-N" "2"
           "-t" "2:8300")
  (part-probe boot-dev)
  (let ((boot-partdev (partdev boot-dev "2"))
        (result (make-hash-table 3)))
    (utils:println "Formatting boot partition device as EXT4:" boot-partdev)
    (when (not (zero? (system (format #f "mkfs.ext4 -q -m 0 ~A ~A"
                                      (if force? "-F" "")
                                      boot-partdev))))
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

(define* (init-boot-parts boot-dev #:key uefiboot? force?)
  (let ((result
         (if uefiboot?
             (init-boot-parts-uefi boot-dev)
             (init-boot-parts-bios boot-dev force?))))
    (utils:println "Finished setting up partitions on:" boot-dev)
    result))

(define* (init-root-parts root-dev #:key boot-dev uefiboot? force?)
  (cond
   (boot-dev
    (system* "sgdisk" root-dev "-Z" "-N" "1" "-t" "1:8300")
    (part-probe root-dev)
    (let ((result
           (init-boot-parts boot-dev
            #:uefiboot? uefiboot?
            #:force? force?)))
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
      (when (not (zero? (system (format #f "mkfs.ext4 -q -m 0 ~A ~A"
                                        (if force? "-F" "")
                                        boot-partdev))))
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
      (when (not (zero? (system (format #f "mkfs.ext4 -q -m 0 ~A ~A"
                                        (if force? "-F" "")
                                        boot-partdev))))
        (error "Failed to create EXT4 filesystem on:" boot-partdev))
      (hash-set! result 'boot boot-partdev)
      (hash-set! result 'root root-partdev)
      result))))

(define* (luks-format partdev #:key passphrase luks-v2? force?)
  (utils:println "Formatting" partdev "to be used as LUKS device...")
  (if passphrase
   (let ((output-port
          (popen:open-output-pipe
           (format #f "cryptsetup luksFormat --type ~A ~A ~A -"
            (if luks-v2? "luks2" "luks1")
            (if force? "--batch-mode" "")
            partdev))))
     (display passphrase output-port)
     (zero? (status:exit-val (popen:close-pipe output-port))))
   (zero? (system (format #f "cryptsetup luksFormat --type ~A ~A ~A"
                   (if luks-v2? "luks2" "luks1")
                   (if force? "--batch-mode" "")
                   partdev)))))

(define (luks-open partdev label passphrase)
  (if passphrase
   (let ((output-port
          (popen:open-output-pipe
           (format #f "cryptsetup luksOpen ~A ~A ~A"
                   (if passphrase (string-append "--key-file -") "")
                   partdev label))))
     (display passphrase output-port)
     (zero? (status:exit-val (popen:close-pipe output-port))))
   (zero? (system* "cryptsetup" "luksOpen" partdev label))))

(define* (init-cryptroot partdev label #:key passphrase luks-shred? luks-v2? force?)
  (when (not (luks-format partdev
              #:passphrase passphrase
              #:luks-v2? luks-v2?
              #:force? force?))
    (error "Failed formatting of LUKS device" partdev))
  (utils:println "Finished formatting device" partdev "for LUKS encryption!")
  (utils:println "Opening LUKS device" partdev "as" label "...")
  (when (not (luks-open partdev label passphrase))
    (error "Failed to open LUKS device:" label))
  (let ((resp
         (cond
          ((not force?)
           (newline)
           (utils:println "It is recommended to overwrite a new LUKS device with random data.")
           (utils:println "WARNING: This can take quite a long time!")
           (readline "Would you like to overwrite LUKS device with random data? [y/N]"))
          (else (if luks-shred? "Y" "N")))))
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
   (lambda (item)
     (when (pair? item)
       (let ((device (car item))
             (label (cdr item)))
         (cond
          ((not (file-exists? (string-append "/dev/mapper/" label)))
           (format #t "Opening LUKS device ~A as ~A...\n" device label)
           (system* "cryptsetup" "luksOpen" "--key-file" keyfile device label))
          (else (error "Some LUKS device is already open with this name!" label))))))
   dev-list))

(define (modprobe? module)
  (zero? (system* "modprobe" "-q" module)))

(define (load-zfs-kernel-module)
  (when (not (modprobe? "zfs"))
    (error "Failed to load ZFS kernel module!")))

(define (read-zfs-version)
  (if (file-exists? "/sys/module/zfs/version")
   (call-with-input-file "/sys/module/zfs/version"
     (lambda (port)
       (let* ((content (rdelim:read-string port))
              (matches
               (regex:string-match
                "([0-9]+)\\.([0-9]+)\\.([0-9]+)(:?-(.*))?"
                content))
              (major-version (regex:match:substring matches 1))
              (major-version (string->number major-version))
              (minor-version (regex:match:substring matches 2))
              (minor-version (string->number minor-version))
              (patch-version (regex:match:substring matches 3))
              (patch-version (string->number patch-version))
              (build-label (regex:match:substring matches 4))
              (build-label (and build-label (substring build-label 1))))
         (list
          major-version
          minor-version
          patch-version
          build-label))))
   #f))

(define (zfs-native-encryption-available?)
  (let ((version (read-zfs-version)))
    (and version (<= 2 (car version)))))

(define* (load-zfs-encryption-keys dataset #:optional passphrase)
  (cond
   (passphrase
    (let ((output-pipe
           (popen:open-pipe* OPEN_WRITE
            "zfs" "load-key" dataset)))
      (rdelim:write-line passphrase output-pipe)
      (zero? (status:exit-val (popen:close-pipe output-pipe)))))
   (else ; When passphrase is not provided, prompt for it!
    (zero? (system* "zfs" "load-key" dataset)))))

(define* (reimport-and-check-pool zpool #:key passphrase
          without-zfs-native-encryption?)
  (when (zero? (utils:system->devnull* "zpool" "list" zpool))
    (when (not (zero? (utils:system->devnull* "zpool" "export" zpool)))
      (error "Failed to export ZFS pool:" zpool)))
  (when (not (zero? (utils:system->devnull* "zpool" "import" "-d" "/dev/disk/by-id" zpool)))
    (error "Failed to import ZFS pool:" zpool))
  (when (not (zero? (utils:system->devnull* "zpool" "list" zpool)))
    (error "Cannot find ZFS pool:" zpool))
  ;; force loading encryption keys for root dataset
  (when (and (zfs-native-encryption-available?)
             (not without-zfs-native-encryption?))
    (when (not (load-zfs-encryption-keys zpool passphrase))
      (error "Failed to load encryption keys for ZFS pool:" zpool))))

(define* (init-zpool name vdevs #:key passphrase
          without-zfs-native-encryption?)
  (utils:println "Creating ZFS pool:" name)
  (let ((zfs-encryption-options
         (list
          "-O" "encryption=aes-128-gcm"
          "-O" "pbkdf2iters=1000000"
          "-O" "keyformat=passphrase"
          "-O" "keylocation=prompt"))
        (zfs-filesystem-options
         (list
          "-O" "normalization=formD"
          "-O" "atime=off"
          "-O" "devices=off"
          "-O" "acltype=posixacl"
          "-O" "xattr=sa")))
    (cond
     ((and (zfs-native-encryption-available?) passphrase)
      (let ((output-pipe
             (apply popen:open-pipe* OPEN_WRITE
                    "zpool" "create" "-f" "-o" "ashift=12"
                    (append
                     zfs-encryption-options
                     zfs-filesystem-options
                     (cons name vdevs)))))
        (rdelim:write-line passphrase output-pipe)
        (zero? (status:exit-val (popen:close-pipe output-pipe)))))
     ((and (zfs-native-encryption-available?)
           (not without-zfs-native-encryption?))
      (zero?
       (apply system*
        "zpool" "create" "-f" "-o" "ashift=12"
        (append
         zfs-encryption-options
         zfs-filesystem-options
         (cons name vdevs)))))
     (else
      (zero?
       (apply system*
        "zpool" "create" "-f" "-o" "ashift=12"
        (append
         zfs-filesystem-options
         (cons name vdevs))))))))

(define* (init-zfsroot zpool zroot #:key swap-size zdirs)
  (let* ((root-dataset (utils:path zpool zroot))
         (swap-dataset (utils:path root-dataset "swap"))
         (swap-zvol (utils:path "" "dev" "zvol" swap-dataset))
         (comp-type (if (<= 2 (car (read-zfs-version))) "zstd" "lz4")))
    (when (zero? (utils:system->devnull* "zfs" "list" root-dataset))
      (error "root dataset already exists!" root-dataset))
    (utils:println "Creating root ZFS dataset" root-dataset "...")
    (when (not (zero?
    (system* "zfs" "create" "-o" (string-append "compression=" comp-type) root-dataset)))
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
  (for-each
   (lambda (item)
     (let ((device (car item))
           (label (cdr item)))
       (backup-header headers-dir device label)))
   (or dev-list '())))

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
         (lambda (item)
           (let ((device (car item))
                 (label (cdr item)))
             (utils:println label (string-append "UUID=" (fsuuid device)) keyfile "luks")))
         dev-list)))))

(define*
  (init-instroot
   target
   #:key
   boot-dev uefiboot?
   root-dev luks-label
   luks-v2? luks-shred?
   dev-list keyfile
   accept-openzfs-license?
   zpool zroot zdirs
   swap-size swapfiles
   force-format-ext4?
   force-format-luks?
   without-zfs-native-encryption?
   unattended?
   passphrase)
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
      (let* ((parts
              (init-root-parts
               root-dev
               #:uefiboot? uefiboot?
               #:boot-dev boot-dev
               #:force? force-format-ext4?))
             (uefi-partdev (hash-ref parts 'uefi))
             (boot-partdev (hash-ref parts 'boot))
             (luks-partdev (hash-ref parts 'root)))
        (init-cryptroot
         luks-partdev luks-label
         #:force? force-format-luks?
         #:luks-shred? luks-shred?
         #:passphrase passphrase
         #:luks-v2? luks-v2?)
        (cond
         (zpool
          (when (and keyfile dev-list)
            (init-cryptdevs keyfile dev-list))
          (deps:install-deps-zfs
           accept-openzfs-license?)
          (load-zfs-kernel-module)
          (when (and unattended? (not passphrase)
                     (zfs-native-encryption-available?)
                     (not without-zfs-native-encryption?))
            (error "Encryption passphrase for ZFS pool must be specified when using unattended mode!"))
          (reimport-and-check-pool zpool
           #:without-zfs-native-encryption?
           without-zfs-native-encryption?
           #:passphrase passphrase)
          (init-zfsroot zpool zroot
           #:swap-size swap-size
           #:zdirs zdirs)
          (let* ((systemfs (utils:path zpool zroot))
                 (luks-dev (utils:path "/dev/mapper" luks-label))
                 (keyfile-stored (if keyfile (utils:path crypt-dir (basename keyfile)) #f)))
            (utils:println "Formatting LUKS device" luks-label "with ext4 to be used as root filesystem...")
            (when (not (zero? (system* "mkfs.ext4" "-q" "-m" "0" luks-dev)))
              (error "Failed to create EXT4 filesystem on:" luks-dev))
            (utils:println "Mounting LUKS root filesystem...")
            (when (not (zero? (system* "mount" luks-dev target)))
              (error "Failed to mount" luks-dev "as" target))
            (utils:println "Mounting all ZFS root directories...")
            (system* "zfs" "set" "canmount=off" systemfs)
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
             (utils:println (utils:path "/dev/zvol" zpool zroot "swap") "none" "swap" "sw" "0" "0")
             (newline)
             (utils:println "# systemd specific legacy mounts of ZFS datasets")
             (newline)
             (for-each
              (lambda (zdir)
                (utils:println "#" (utils:path zpool zroot zdir) (utils:path "" zdir) "zfs" "defaults,x-systemd.after=zfs.target" "0" "0"))
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
      (deps:install-deps-zfs
       accept-openzfs-license?)
      (load-zfs-kernel-module)
      (when (and unattended? (not passphrase)
                 (zfs-native-encryption-available?)
                 (not without-zfs-native-encryption?))
        (error "Encryption passphrase for ZFS pool must be specified when using unattended mode!"))
      (let* ((parts
              (init-boot-parts boot-dev
               #:uefiboot? uefiboot?
               #:force? force-format-ext4?))
             (uefi-partdev (hash-ref parts 'uefi))
             (boot-partdev (hash-ref parts 'boot))
             (systemfs (utils:path zpool zroot)))
        (reimport-and-check-pool zpool
         #:passphrase passphrase)
        (init-zfsroot zpool zroot
         #:swap-size swap-size
         #:zdirs zdirs)
        (utils:println "Mounting ZFS root...")
        (system* "zpool" "set" (string-append "bootfs=" systemfs) zpool)
        (system* "zfs" "set" (string-append "mountpoint=" target) systemfs)
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
         (utils:println (utils:path "/dev/zvol" zpool zroot "swap") "none" "swap" "sw" "0" "0")
         (print-fstab-entry-boot boot-partdev uefi-partdev))))
     (else
      (error "Either block device (when using LUKS encryption), or a ZFS pool (when using native ZFS encrption) must be specified for the root filesystem!")))))

(define options-spec
  `((target
     (single-char #\t)
     (description
      "Root mountpoint for installation.")
     (default "/mnt/instroot")
     (value-arg "path")
     (value #t))
    (zpool
     (single-char #\z)
     (description
      "ZFS pool to use for root filesystem and swap volume, if no root device for LUKS encryption is specified. A separate boot device must be specified when using ZFS for root filesystem! Alternatively, if used together with LUKS encrypted root device, the pool is used only for additional system directories and swap volume.")
     (value-arg "pool")
     (value #t))
    (zroot
     (single-char #\f)
     (description
      "Name of the system root dataset in the ZFS pool.")
     (default "system")
     (value-arg "name")
     (value #t))
    (zdirs
     (single-char #\d)
     (description
      "Coma separated list of root directories to mount as ZFS datasets.")
     (default "home,var,var/lib")
     (value-arg "dirs")
     (value #t))
    (bootdev
     (single-char #\b)
     (description
      "Use separate boot device for /boot filesystem.")
     (predicate ,utils:block-device?)
     (value-arg "device")
     (value #t))
    (rootdev
     (single-char #\r)
     (description
      "Device to use for LUKS encrypted root filesystem.")
     (predicate ,utils:block-device?)
     (value-arg "device")
     (value #t))
    (luks-label
     (single-char #\l)
     (description
      "Device name to use to open the LUKS encrypted root partition.
By default the label is generated based on the name of the root partition device.")
     (predicate
      ,(lambda (s) (regex:string-match "^[[:alnum:]_]+$" s)))
     (value-arg "name")
     (value #t))
    (luks-devs
     (single-char #\v)
     (description
      "Coma separated list of colon separated pairs of additional LUKS encrypted devices, and their respective LUKS labels.
(e.g. /dev/sda:foo,/dev/sdb:bar,/dev/sdc:baz) These mappings are then used to:
  1) unlock these devices (potentially to import a ZFS pool without native encryption on top of them)
  2) create crypttab entries for the devices to automatically unlock them during system boot
It is necessary to specify these mappings together with the keyfile option,
using a key which can unlock the devices in the list!")
     (value-arg "pairs")
     (value #t))
    (keyfile
     (single-char #\k)
     (description
      "Keyfile used to decrypt additional LUKS encrypted devices.")
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
    (passphrase
     (single-char #\p)
     (description "Passphrase used with LUKS or ZFS encryption for root filesystem and swap space.")
     (value-arg "text")
     (value #t))
    (swapsize
     (single-char #\s)
     (description
      "Size of the total swap space to use. (KMGTPEZY binary unit suffixes allowed)")
     (predicate
      ,(lambda (s) (regex:string-match "^[0-9]+[KMGTPEZY]?$" s)))
     (value-arg "size")
     (value #t))
    (swapfiles
     (single-char #\S)
     (description
      "Number of swapfiles to use to break total swap-space up into. Swapfiles are created in equally sized chunks.
COUNT zero means to use LVM volumes instead of swapfiles.")
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
    (luks-shred
     (description
      "Shred LUKS device with random data after formatting."))
    (init-zpool
     (description
      "Install and configure necessary ZFS dependencies only, then exit.")
     (single-char #\Z))
    (force-format-ext4
     (description
      "Force formatting devices (i.e. root and boot) with ext4 filesystems.
Normally the process would ask for confirmation before formatting, if it found existing filesystem headers on the device."))
    (force-format-luks
     (description
      "Force formatting root device with LUKS, and automatically confirm the prompt asking for confirmation. Also, by default this automatically skips shredding the LUKS device after formatting, unless used together with the --luks-shred flag, in which case it automatically shreds the LUKS device after formatting."))
    (without-zfs-native-encryption
     (description
      "Do not use ZFS native encryption, even when it is available."))
    (accept-openzfs-license
     (description "Confirm OpenZFS License (CDDL) automatically:
https://github.com/openzfs/zfs/blob/master/LICENSE"))
    (unattended
     (description
      "Runs script in unattended mode, toggling options force-format-ext4, force-format-luks, and accept-openzfs-license. This option guarantees that the script will not prompt for any user imput. Requires passphrase to be specified when using LUKS root device or ZFS pool with native encryption support enabled.")
     (single-char #\A))
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

(define (cryptroot-label root-dev boot-dev)
  (string-append
   (basename
    (if boot-dev
     (partdev root-dev "1")
     (partdev root-dev "3")))
   "_crypt"))

(define (main args)
  (let* ((options (utils:getopt-extra args options-spec))
         (target (hash-ref options 'target))
         (boot-dev (hash-ref options 'bootdev))
         (boot-dev (and boot-dev (canonicalize-path boot-dev)))
         (root-dev (hash-ref options 'rootdev))
         (root-dev (and root-dev (canonicalize-path root-dev)))
         (luks-label
          (hash-ref options 'luks-label
           (and root-dev (cryptroot-label root-dev boot-dev))))
         (zpool (hash-ref options 'zpool))
         (zroot (hash-ref options 'zroot))
         (zdirs (hash-ref options 'zdirs))
         (zdirs (and zdirs (string-split zdirs #\,)))
         (keyfile (hash-ref options 'keyfile))
         (new-keyfile (hash-ref options 'genkey))
         (dev-list (hash-ref options 'luks-devs))
         (dev-list
          (and dev-list
           (utils:parse-arg-alist dev-list
            #:list-separator #\,
            #:pair-separator #\:)))
         (passphrase (hash-ref options 'passphrase))
         (swap-size (hash-ref options 'swapsize))
         (swapfiles (hash-ref options 'swapfiles))
         (swapfiles (and swapfiles (string->number swapfiles)))
         (luks-v2? (hash-ref options 'luksv2))
         (luks-shred? (hash-ref options 'luks-shred))
         (uefiboot? (hash-ref options 'uefiboot))
         (init-zpool? (hash-ref options 'init-zpool))
         (without-zfs-native-encryption?
          (hash-ref options 'without-zfs-native-encryption))
         (without-zfs-native-encryption?
          (or without-zfs-native-encryption? root-dev))
         (unattended? (hash-ref options 'unattended))
         (accept-openzfs-license? (hash-ref options 'accept-openzfs-license))
         (accept-openzfs-license? (not (equal? accept-openzfs-license? unattended?)))
         (force-format-ext4? (hash-ref options 'force-format-ext4))
         (force-format-ext4? (not (equal? force-format-ext4? unattended?)))
         (force-format-luks? (hash-ref options 'force-format-luks))
         (force-format-luks? (not (equal? force-format-luks? unattended?)))
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
      (deps:install-deps-zfs
       accept-openzfs-license?)
      (load-zfs-kernel-module)
      (let ((args (hash-ref options '())))
        (if (not (null? args))
         (let ((name (car args))
               (vdevs (cdr args)))
           (when (and unattended? (not passphrase)
                      (zfs-native-encryption-available?)
                      (not without-zfs-native-encryption?))
             (error "Encryption passphrase for ZFS pool must be specified when using unattended mode!"))
           (if (init-zpool name vdevs
                #:without-zfs-native-encryption?
                without-zfs-native-encryption?
                #:passphrase passphrase)
               (utils:println "Finished creating ZFS pool:" name)
               (error "Failed to create ZFS pool:" name)))
         (utils:println "Finished installing and loading ZFS kernel modules!"))))
     ((not swap-size)
      (error "Swap size must be specified!"))
     ((and zpool (< 0 swapfiles))
      (error "Using swap files with ZFS is not supported!"))
     ((and zpool (not (or boot-dev root-dev)))
       (error "Separate boot device must be specified when using ZFS as root filesystem!"))
     ((and dev-list (not keyfile))
      (error "Keyfile must be specified to unlock additional LUKS encrypted devices!"))
     ((and luks-v2? (<= 10 (or (deps:read-debian-version) 0)))
      (error "LUKS format version 2 is only supported in Debian Buster or later!"))
     ((and luks-shred? (not force-format-luks?))
      (error "Shredding LUKS device option must be only used together with the --force-format-luks option."))
     ((and uefiboot? (not (modprobe? "efivars"))
           ;; efivars module might be built into the kernel directly
           (not (utils:directory? (utils:path "" "sys" "firmware" "efi"))))
      (error "Cannot use UEFI boot, when efivars module is not loaded!"))
     ((and unattended? (not passphrase))
      (cond
       (root-dev
        (error "Encryption passphrase for LUKS root device must be specified when using unattended mode!"))
       ((and zpool
         (zfs-native-encryption-available?)
         (not without-zfs-native-encryption?))
        (error "Encryption passphrase for ZFS pool must be specified when using unattended mode!"))))
     (else
      (hash-remove! options 'passphrase)
      (hash-set! options 'luks-label luks-label)
      (utils:write-config utils:config-filename options)
      (init-instroot target
       #:boot-dev boot-dev
       #:uefiboot? uefiboot?
       #:root-dev root-dev
       #:luks-label luks-label
       #:luks-shred? luks-shred?
       #:luks-v2? luks-v2?
       #:dev-list dev-list
       #:keyfile keyfile
       #:accept-openzfs-license?
       accept-openzfs-license?
       #:zpool zpool
       #:zroot zroot
       #:zdirs zdirs
       #:swap-size swap-size
       #:swapfiles swapfiles
       #:passphrase passphrase
       #:unattended? unattended?
       #:without-zfs-native-encryption? without-zfs-native-encryption?
       #:force-format-ext4? force-format-ext4?
       #:force-format-luks? force-format-luks?)
      (utils:move-file utils:config-filename (utils:path target utils:config-filename))
      (utils:println "Finished setting up installation root" target)))))
