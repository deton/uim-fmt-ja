(require "fmt-ja.scm")
(define (read-input res)
  (let ((ch (read-char)))
    (if (eof-object? ch)
      res
      (read-input (cons ch res)))))

(define (main args)
  (define (parse-num arg)
    (and (> (string-length arg) 0)
         (string->number arg)))
  (let ((foldonly? (string=? (list-ref args 1) "t"))
        (indchange? (string=? (list-ref args 2) "t"))
        (goal (parse-num (list-ref args 3)))
        (max (parse-num (list-ref args 4)))
        (tab (parse-num (list-ref args 5))))
    (set! fmt-ja-fold-only foldonly?)
    (set! fmt-ja-new-paragraph-by-indent-change indchange?)
    (if tab (set! fmt-ja-tab-width tab))
    (if goal (set! fmt-ja-goal-width goal))
    (if max (set! fmt-ja-max-width max))
    (if (<= fmt-ja-goal-width fmt-ja-max-width)
      (let ((str (list->string (reverse (read-input '())))))
        (display (fmt-ja-str str)))
      (display
        (format "ERROR: max-width(~d) < goal-width(~d)\n"
          fmt-ja-max-width
          fmt-ja-goal-width)))))
