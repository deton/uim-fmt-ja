;;; IM to format selection or clipboard like fmt command.

(require-extension (srfi 1 2 8))
(require-custom "fmt-ja-custom.scm")

(set! fmt-ja-kinsoku-chars-on-start-internal
  (fmt-ja-utf8->eucjp fmt-ja-kinsoku-chars-on-start))
(set! fmt-ja-kinsoku-chars-on-end-internal
  (fmt-ja-utf8->eucjp fmt-ja-kinsoku-chars-on-end))

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
      (im-commit pc (fmt-ja-str str))
      (im-commit-raw pc))))

(define (fmt-ja-on-clipboard pc)
  (let ((str (fmt-ja-acquire-text pc 'clipboard)))
    (if (string? str)
      (im-commit pc (fmt-ja-str str))
      (im-commit-raw pc))))

(define (fmt-ja-str str)
  (let* ((char-list (string-to-list str))
         (src-lines (fmt-ja-char-list->line-list '() (reverse char-list)))
         (res-lines (fmt-ja-line-list src-lines))
         (res-char-list
          (append-map
            (lambda (line)
              (append line '("\n")))
            (reverse res-lines))))
    (apply string-append
      (if (not (string=? (car char-list) "\n")) ; str is not terminated with \n
        (drop-right res-char-list 1)
        res-char-list))))

(define (fmt-ja-line-list src-lines)
  (fmt-ja-fold-lines '()
    (reverse (fmt-ja-join-lines '() src-lines))
    #f))

(define (fmt-ja-get-indent line)
  (take-while fmt-ja-str1-whitespace? line))

;; lines->joined-lines (lines->paragraph-list)
(define (fmt-ja-join-lines joined-lines src-lines)
  ;; join lines in a paragraph to one line
  (define (join-paragraph paragraph src-lines indent)
    (define (paragraph-end? next-line cur-indent)
      (or (null? next-line) ; empty line?
          (and fmt-ja-new-paragraph-by-indent-change
               (not (equal? (fmt-ja-get-indent next-line) cur-indent)))))
    (cond
      ((null? src-lines)
        (cons paragraph joined-lines))
      ((paragraph-end? (car src-lines) indent)
        ;; next paragraph
        (fmt-ja-join-lines (cons paragraph joined-lines) src-lines))
      (else
        (join-paragraph
          (fmt-ja-join-two-lines paragraph (car src-lines))
          (cdr src-lines)
          indent))))
  (if (null? src-lines)
    joined-lines
    (let ((line (car src-lines)))
      (if (null? line) ; empty line?
        (fmt-ja-join-lines (cons line joined-lines) (cdr src-lines))
        (join-paragraph line (cdr src-lines) (fmt-ja-get-indent line))))))

(define (fmt-ja-join-two-lines line1 line2)
  (let ((l1rev (drop-while fmt-ja-str1-whitespace? (reverse line1)))
        (l2 (drop-while fmt-ja-str1-whitespace? line2)))
    (if (or (null? l1rev)
            (null? l2)
            (fmt-ja-str1-wide? (car l1rev))
            (fmt-ja-str1-wide? (car l2)))
      (append (reverse l1rev) l2)
      (append (reverse l1rev) '(" ") l2))))

(define (fmt-ja-fold-lines folded-lines src-lines indent)
  (if (null? src-lines)
    folded-lines
    (let ((line (car src-lines)))
      (cond
        ((null? line) ; empty line?
          (fmt-ja-fold-lines (cons line folded-lines) (cdr src-lines) #f))
        ((> (fmt-ja-width line) fmt-ja-goal-width)
          (let ((ind (or indent (fmt-ja-get-indent line))))
            (receive
              (line0 rest)
              (fmt-ja-fold-line line)
              (fmt-ja-fold-lines
                (cons line0 folded-lines)
                (if (null? rest) ; avoid to add non-exist empty line
                  (cdr src-lines)
                  (cons (append ind rest) (cdr src-lines)))
                ind))))
        (else
          (fmt-ja-fold-lines (cons line folded-lines) (cdr src-lines) #f))))))

(define (fmt-ja-fold-line line)
  ;; make line0 getting a character from line
  (define (make-line line0 line)
    (define (fold-line line0 line shrink?)
      (define (kinsoku line0 line shrink?)
        (if (or
              (and (pair? line)
                   (string-contains fmt-ja-kinsoku-chars-on-start-internal
                                    (car line) 0))
              (and (pair? line0)
                   (string-contains fmt-ja-kinsoku-chars-on-end-internal
                                    (car line0) 0)))
          (if shrink?
            (fold-line line0 line #t)
            (if (pair? line)
              (fold-line (cons (car line) line0) (cdr line) #f)
              (values line0 line)))
          (values line0 line)))
      (define (fold-line-latin line0 line shrink?)
        (define (word-limit? char)
          (or (fmt-ja-str1-whitespace? char)
              (fmt-ja-str1-wide? char)))
        (if shrink?
          (receive (last-word rest) (break word-limit? line0)
            (if (null? rest) ; no whitespace?
              (values '() line)
              (kinsoku
                (drop-while fmt-ja-str1-whitespace? rest)
                (append (reverse last-word) line)
                #t)))
          ;; expand
          (receive (word-tail rest) (break word-limit? line)
            (kinsoku
              (append (reverse word-tail) line0)
              (drop-while fmt-ja-str1-whitespace? rest)
              #f))))
      (define (fold-line-ja line0 line shrink?)
        (if shrink?
          (kinsoku (cdr line0) (cons (car line0) line) #t)
          (kinsoku line0 line #f)))
      (cond
        ((null? line0) ; all chars are Kinsoku char?
          (values '() line))
        ((fmt-ja-str1-wide? (car line0))
          (fold-line-ja line0 line shrink?))
        (else
          (fold-line-latin line0 line shrink?))))
    (let ((line0n (reverse line0)))
      (cond
        ;; width of line0 becomes larger than the goal by adding last char
        ((> (fmt-ja-width line0n) fmt-ja-goal-width)
          (receive
            (s-l0 s-rest)
            (fold-line line0 line #t) ; shrink
            (let ((s-width (fmt-ja-width (reverse s-l0))))
              (cond
                ((= s-width fmt-ja-goal-width)
                  (values (reverse s-l0) s-rest))
                ((> fmt-ja-max-width fmt-ja-goal-width)
                  (receive
                    (e-l0 e-rest)
                    (fold-line line0 line #f) ; expand
                    (let ((e-width (fmt-ja-width (reverse e-l0))))
                      (if (or (and (<= e-width fmt-ja-max-width)
                                   (<= (abs (- e-width fmt-ja-goal-width))
                                       (abs (- fmt-ja-goal-width s-width))))
                              (null? s-l0)) ; avoid infinite loop
                        (values (reverse e-l0) e-rest)
                        (values (reverse s-l0) s-rest)))))
                ((null? s-l0) ; got empty line? (all chars are Kinsoku char)
                  (if (null? line)
                    (values line0n '())
                    (make-line (cons (car line) line0) (cdr line)))) ;donot fold
                (else
                  (values (reverse s-l0) s-rest))))))
        ((null? line)
          (if (null? line0)
            (values line '())
            (values line0n line)))
        (else ; get a character to line0 from line
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
