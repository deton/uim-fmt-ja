;;; IM to format selection or clipboard like fmt command.

(require-extension (srfi 1 2))
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
      (im-commit pc
        (string-list-concat
          (fmt-on-list () (reverse (string-to-list str))))))))

(define (fmt-on-clipboard pc)
  (let ((str (fmt-acquire-text pc 'clipboard)))
    (if (string? str)
      (im-commit pc
        (string-list-concat
          (fmt-on-list () (reverse (string-to-list str))))))))

(define (fmt-on-list res src)
  (define (make-line line src)
    (cond
      ((null? src)
        (fmt-on-list (append line res) src))
      ((>= (fmt-width line) fmt-fold-width)
        ;; TODO: inhibit ",." on top
        (fmt-on-list (append (cons "\n" line) res) src))
      ((string=? (car src) "\n")
        (make-line (cons " " line) (cdr src))) ; XXX: not add in Japanese
      (else
        (make-line (cons (car src) line) (cdr src)))))
  ;(writeln src)
  (if (null? src)
    res
    (make-line () src)))

(define (fmt-width line)
  ;; TODO: support tab char
  (fold
    (lambda (x sum) (+ sum (if (< (fmt-euc-jp-string->ichar x) 128) 1 2)))
    0
    line))

(define (fmt-euc-jp-string->ichar s)
  (let ((sl (with-char-codec "EUC-JP"
              (lambda ()
                (string->list s)))))
    (cond
      ((null? sl)
        0)
      (else
        (char->integer (car sl))))))
