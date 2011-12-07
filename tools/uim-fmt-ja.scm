(require "fmt-ja.scm")
(define (read-input res)
  (let ((ch (read-char)))
    (if (eof-object? ch)
      res
      (read-input (cons ch res)))))

(define (main args)
  (let ((str (list->string (reverse (read-input '())))))
    (display (fmt-ja-str str))))
