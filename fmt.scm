;;; IM to format selection or clipboard like fmt command.

(require-extension (srfi 1 2 8))
(require-custom "fmt-custom.scm")

(define fmt-context-rec-spec context-rec-spec)
(define-record 'fmt-context fmt-context-rec-spec)
(define fmt-context-new-internal fmt-context-new)

(define fmt-context-new
  (lambda args
    (let ((pc (apply fmt-context-new-internal args)))
      pc)))

(define fmt-init-handler
  (lambda (id im arg)
    (let ((pc (fmt-context-new id im)))
      pc)))

(define (fmt-key-press-handler pc key key-state)
  (if (ichar-control? key)
    (im-commit-raw pc)
    (cond
      ((fmt-selection-key? key key-state)
        (fmt-on-selection pc))
      ((fmt-clipboard-key? key key-state)
        (fmt-on-clipboard pc))
      (else
        (im-commit-raw pc)))))

(define (fmt-key-release-handler pc key state)
  (im-commit-raw pc))

(register-im
 'fmt
 "ja"
 "EUC-JP"
 fmt-im-name-label
 fmt-im-short-desc
 #f
 fmt-init-handler
 #f
 context-mode-handler
 fmt-key-press-handler
 fmt-key-release-handler
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

(define (fmt-acquire-text pc id)
  (and-let*
    ((ustr (im-acquire-text pc id 'beginning 0 'full))
     (latter (ustr-latter-seq ustr)))
    (and (pair? latter)
         (car latter))))

(define (fmt-on-selection pc)
  (let ((str (fmt-acquire-text pc 'selection)))
    (if (string? str)
      (im-commit pc (fmt-str str)))))

(define (fmt-on-clipboard pc)
  (let ((str (fmt-acquire-text pc 'clipboard)))
    (if (string? str)
      (im-commit pc (fmt-str str)))))

(define (fmt-str str)
  (let* ((src-lines
          (fmt-char-list->line-list '()
            (reverse (string-to-list str))))
         (res-lines
          (fmt-line-list '() src-lines)))
    (apply string-append
      (append-map
        (lambda (line)
          (append line '("\n")))
        (reverse res-lines)))))

(define (fmt-line-list res-lines src-lines)
  (define (join-and-fold-line line src-lines)
    (cond
      ((null? line) ; empty line?
        (fmt-line-list (cons line res-lines) src-lines))
      ((>= (fmt-width line) fmt-fold-width)
        (receive
          (line0 rest)
          (fmt-fold-line line)
          (fmt-line-list
            (cons line0 res-lines)
            (if (null? rest)
              src-lines ; avoid to add non-exist empty line
              (cons rest src-lines)))))
      ((or (null? src-lines)
           (fmt-new-paragraph? (car src-lines)))
        (fmt-line-list (cons line res-lines) src-lines))
      (else
        (join-and-fold-line
          (fmt-join-lines line (car src-lines))
          (cdr src-lines)))))
  (if (null? src-lines)
    res-lines
    (join-and-fold-line (car src-lines) (cdr src-lines))))

(define (fmt-new-paragraph? line)
  (null? line)) ; empty line?
  ;; TODO: indentation change

(define (fmt-join-lines line1 line2)
  (let* ((l1rev (drop-while fmt-str1-whitespace? (reverse line1)))
         (l2 (drop-while fmt-str1-whitespace? line2)))
    (if (or (null? l1rev)
            (null? l2)
            (fmt-str1-wide? (car l1rev))
            (fmt-str1-wide? (car l2)))
      (append (reverse l1rev) l2)
      (append (reverse l1rev) '(" ") l2))))

(define (fmt-fold-line line)
  (define (make-line line0 line)
    (let ((width (fmt-width line0)))
      (if (>= width fmt-fold-width)
        (receive
          (last-word rest)
          (break fmt-str1-whitespace? line0)
          ;; TODO: line0 with no whitespace
          (values (reverse (drop-while fmt-str1-whitespace? rest))
                  (append (reverse last-word) line)))
        (make-line (cons (car line) line0) (cdr line)))))
  (make-line '() line))
;; TODO

(define (fmt-width line)
  ;; TODO: support tab char
  (fold
    (lambda (x sum) (+ sum (if (fmt-str1-wide? x) 2 1)))
    0
    line))

(define (fmt-str1-wide? str1)
  (let ((char (fmt-euc-jp-string->char str1)))
    (and char
         (>= (char->integer char) 128))))

(define (fmt-str1-whitespace? str1)
  (let ((char (fmt-euc-jp-string->char str1)))
    (and char
         (char-whitespace? char))))

(define (fmt-euc-jp-string->char s)
  (let ((sl (with-char-codec "EUC-JP"
              (lambda ()
                (string->list s)))))
    (and (not (null? sl))
         (car sl))))

(define (fmt-char-list->line-list res char-list)
  (if (null? char-list)
    (reverse res)
    (receive
      (line rest)
      (break (lambda (x) (string=? x "\n")) char-list)
      (fmt-char-list->line-list (cons line res)
        (if (pair? rest)
          (cdr rest) ; drop first "\n"
          rest)))))
