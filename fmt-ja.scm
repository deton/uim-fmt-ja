;;; IM to format selection or clipboard like fmt command.

(require-extension (srfi 1 2 8))
(require-custom "fmt-ja-custom.scm")

(define fmt-ja-context-rec-spec context-rec-spec)
(define-record 'fmt-ja-context fmt-ja-context-rec-spec)
(define fmt-ja-context-new-internal fmt-ja-context-new)

(define fmt-ja-context-new
  (lambda args
    (let ((pc (apply fmt-ja-context-new-internal args)))
      pc)))

(define fmt-ja-init-handler
  (lambda (id im arg)
    (let ((pc (fmt-ja-context-new id im)))
      pc)))

(define (fmt-ja-key-press-handler pc key key-state)
  (if (ichar-control? key)
    (im-commit-raw pc)
    (cond
      ((fmt-ja-selection-key? key key-state)
        (fmt-ja-on-selection pc))
      ((fmt-ja-clipboard-key? key key-state)
        (fmt-ja-on-clipboard pc))
      (else
        (im-commit-raw pc)))))

(define (fmt-ja-key-release-handler pc key state)
  (im-commit-raw pc))

(register-im
 'fmt-ja
 "ja"
 "EUC-JP"
 fmt-ja-im-name-label
 fmt-ja-im-short-desc
 #f
 fmt-ja-init-handler
 #f
 context-mode-handler
 fmt-ja-key-press-handler
 fmt-ja-key-release-handler
 #f
 #f
 #f
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )

(define (fmt-ja-acquire-text pc id)
  (and-let*
    ((ustr (im-acquire-text pc id 'beginning 0 'full))
     (latter (ustr-latter-seq ustr)))
    (and (pair? latter)
         (car latter))))

(define (fmt-ja-on-selection pc)
  (let ((str (fmt-ja-acquire-text pc 'selection)))
    (if (string? str)
      (im-commit pc (fmt-ja-str str)))))

(define (fmt-ja-on-clipboard pc)
  (let ((str (fmt-ja-acquire-text pc 'clipboard)))
    (if (string? str)
      (im-commit pc (fmt-ja-str str)))))

(define (fmt-ja-str str)
  (let* ((char-list (string-to-list str))
         (src-lines
          (fmt-ja-char-list->line-list '()
            (reverse char-list)))
         (res-lines
          (fmt-ja-line-list '() src-lines))
         (res-char-list
          (append-map
            (lambda (line)
              (append line '("\n")))
            (reverse res-lines))))
    (apply string-append
      (if (not (string=? (car char-list) "\n")) ; str is not terminated with \n
        (drop-right res-char-list 1)
        res-char-list))))

(define (fmt-ja-line-list res-lines src-lines)
  (define (get-indent line)
    (take-while fmt-ja-str1-whitespace? line))
  (define (join-and-fold-line line src-lines indent)
    (define (same-indent? i1 i2)
      (equal? i1 i2)) ; XXX: treat tab as spaces?
    (define (new-paragraph? line cur-indent)
      (or (null? line)
          (not (same-indent? (get-indent line) cur-indent))))
    (cond
      ((null? line) ; empty line?
        (fmt-ja-line-list (cons line res-lines) src-lines))
      ((> (fmt-ja-width line) fmt-ja-fold-width)
        (receive
          (line0 rest)
          (fmt-ja-fold-line line)
          (fmt-ja-line-list
            (cons line0 res-lines)
            (if (null? rest)
              src-lines ; avoid to add non-exist empty line
              (cons (append indent rest) src-lines)))))
      ((null? src-lines)
        (cons line res-lines))
      ((new-paragraph? (car src-lines) indent)
        (fmt-ja-line-list (cons line res-lines) src-lines))
      (else
        (join-and-fold-line
          (fmt-ja-join-lines line (car src-lines))
          (cdr src-lines)
          indent))))
  (if (null? src-lines)
    res-lines
    (join-and-fold-line
      (car src-lines)
      (cdr src-lines)
      (get-indent (car src-lines)))))

(define (fmt-ja-join-lines line1 line2)
  (let* ((l1rev (drop-while fmt-ja-str1-whitespace? (reverse line1)))
         (l2 (drop-while fmt-ja-str1-whitespace? line2)))
    (if (or (null? l1rev)
            (null? l2)
            (fmt-ja-str1-wide? (car l1rev))
            (fmt-ja-str1-wide? (car l2)))
      (append (reverse l1rev) l2)
      (append (reverse l1rev) '(" ") l2))))

(define (fmt-ja-fold-line line)
  (define (make-line line0 line)
    (define (fold-line line0 line)
      (define (kinsoku line0 line)
        (if (or
              (and (pair? line)
                   (string-contains fmt-ja-kinsoku-chars-on-start (car line) 0))
              (and (pair? line0)
                   (string-contains fmt-ja-kinsoku-chars-on-end (car line0) 0)))
          (fold-line line0 line)
          (values line0 line)))
      (define (fold-line-latin line0 line)
        (receive
          (last-word rest)
          (break
            (lambda (x)
              (or (fmt-ja-str1-whitespace? x)
                  (fmt-ja-str1-wide? x)))
            line0)
          (if (and (null? rest) ; no whitespace?
                   (pair? line))
            (make-line (cons (car line) line0) (cdr line)) ; do not fold line
            (kinsoku
              (drop-while fmt-ja-str1-whitespace? rest)
              (append (reverse last-word) line)))))
      (define (fold-line-ja line0 line)
        (kinsoku (cdr line0) (cons (car line0) line)))
      (cond
        ((null? line0) ; all chars are Kinsoku char?
          (values '() line))
        ((fmt-ja-str1-wide? (car line0))
          (fold-line-ja line0 line))
        (else
          (fold-line-latin line0 line))))
    (let ((line0n (reverse line0)))
      (cond
        ((> (fmt-ja-width line0n) fmt-ja-fold-width)
          (receive
            (l0 rest)
            (fold-line line0 line)
            (if (null? l0) ; got empty line? (all chars are Kinsoku char)
              (if (null? line)
                (values line0n '())
                (make-line (cons (car line) line0) (cdr line))) ; do not fold
              (values (reverse l0) rest))))
        ((null? line)
          (if (null? line0)
            (values line '())
            (values line0n line)))
        (else
          (make-line (cons (car line) line0) (cdr line))))))
  (make-line '() line))

(define (fmt-ja-width line)
  (fold
    (lambda (x col)
      (if (and (string=? x "\t") (not (zero? fmt-ja-tab-width)))
        (* (+ (quotient col fmt-ja-tab-width) 1) fmt-ja-tab-width)
        (+ col (if (fmt-ja-str1-wide? x) 2 1))))
    0
    line))

(define (fmt-ja-str1-wide? str1)
  (let ((char (fmt-ja-euc-jp-string->char str1)))
    (and char
         ;; TODO: support HALFWIDTH KATAKANA
         (>= (char->integer char) 128))))

(define (fmt-ja-str1-whitespace? str1)
  (let ((char (fmt-ja-euc-jp-string->char str1)))
    (and char
         (char-whitespace? char))))

(define (fmt-ja-euc-jp-string->char s)
  (let ((sl (with-char-codec "EUC-JP"
              (lambda ()
                (string->list s)))))
    (and (not (null? sl))
         (car sl))))

(define (fmt-ja-char-list->line-list res char-list)
  (if (null? char-list)
    (reverse res)
    (receive
      (line rest)
      (break (lambda (x) (string=? x "\n")) char-list)
      (fmt-ja-char-list->line-list (cons line res)
        (if (pair? rest)
          (cdr rest) ; drop first "\n"
          rest)))))
