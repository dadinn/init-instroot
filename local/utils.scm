(define-module (local utils)
  #:export (get-lastrun write-lastrun getopt-lastrun usage)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 popen))

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

(define (get-lastrun path)
  (if (file-exists? path)
      (let* ((lr-file (open-input-file path))
	     (lr-alist
	      (map
	       (lambda (kv) (cons (car kv) (cadr kv)))
	       (read lr-file))))
	(close lr-file)
	(alist->hash-table lr-alist))
      (make-hash-table 0)))

(define* (getopt-lastrun args options-spec #:optional lastrun-map)
  (let* ((options (getopt-long args (conform-spec options-spec)))
	 (result (make-hash-table (length options-spec))))
    (fold
     (lambda (spec noop!)
       (let* ((long-name (car spec))
	      (props (cdr spec))
	      (default (assoc-ref props 'default))
	      (default (and default (car default)))
	      (default (hash-ref lastrun-map long-name default))
	      (value (option-ref options long-name default)))
	 (if value (hash-set! result long-name value))))
     #nil options-spec)
    result))

(define (write-lastrun path options)
  (let ((lrfile (open-output-file ".lastrun")))
    (pretty-print (hash-map->list list options) lrfile)
    (close lrfile)))

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
		 (string-append " " (string-locale-upcase (car value-arg)) "\n")
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
