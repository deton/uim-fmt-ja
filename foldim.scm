;;; IM to fold line on selection or clipboard.

(require-extension (srfi 1 2 8))
(require-custom "foldim-custom.scm")

(define foldim-context-rec-spec context-rec-spec)
(define-record 'foldim-context foldim-context-rec-spec)
(define foldim-context-new-internal foldim-context-new)

(define foldim-context-new
  (lambda args
    (let ((pc (apply foldim-context-new-internal args)))
      pc)))

(define foldim-init-handler
  (lambda (id im arg)
    (let ((pc (foldim-context-new id im)))
      pc)))

(define (foldim-key-press-handler pc key key-state)
  (if (ichar-control? key)
    (im-commit-raw pc)
    (cond
      ((foldim-selection-key? key key-state)
        (foldim-on-selection pc))
      ((foldim-clipboard-key? key key-state)
        (foldim-on-clipboard pc))
      (else
        (im-commit-raw pc)))))

(define (foldim-key-release-handler pc key state)
  (im-commit-raw pc))

(register-im
 'foldim
 ""
 "UTF-8"
 foldim-im-name-label
 foldim-im-short-desc
 #f
 foldim-init-handler
 #f
 context-mode-handler
 foldim-key-press-handler
 foldim-key-release-handler
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

(define (foldim-acquire-text pc id)
  (and-let*
    ((ustr (im-acquire-text pc id 'beginning 0 'full))
     (latter (ustr-latter-seq ustr)))
    (and (pair? latter)
         (car latter))))

(define (foldim-on-selection pc)
  (let ((str (foldim-acquire-text pc 'selection)))
    (if (string? str)
      (im-commit pc
        (list->string
          (reverse! (foldim-on-list () (foldim-string-as-utf8->list str))))))))

(define (foldim-on-clipboard pc)
  (let ((str (foldim-acquire-text pc 'clipboard)))
    (if (string? str)
      (im-commit pc
        (list->string
          (reverse! (foldim-on-list () (foldim-string-as-utf8->list str))))))))

(define (foldim-on-list res src)
  (define (make-line line src)
    (cond
      ((null? src)
        (values line src))
      ((>= (foldim-width line) foldim-fold-width)
        (values (cons #\newline line) src)) ; TODO: inhibit ",." on top
      ((char=? (car src) #\newline)
        (make-line (cons #\space line) (cdr src))) ; XXX: not add in Japanese
      (else
        (make-line (cons (car src) line) (cdr src)))))
  (if (null? src)
    res
    (receive (line src) (make-line () src)
      (foldim-on-list (append line res) src))))

(define (foldim-width line)
  ;; TODO: support tab char
  ;; TODO: support multibyte halfwidth char
  (fold (lambda (x sum) (+ sum (if (< (char->integer x) 128) 1 2)))
    0
    line))

(define (foldim-string-as-utf8->list str)
  (with-char-codec "UTF-8"
    (lambda ()
      (string->list str))))
