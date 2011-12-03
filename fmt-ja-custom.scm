(require "i18n.scm")

(define fmt-ja-im-name-label (N_ "fmt-ja"))
(define fmt-ja-im-short-desc (N_ "format selection or clipboard like fmt command"))

(define-custom-group 'fmt-ja
                     fmt-ja-im-name-label
                     fmt-ja-im-short-desc)

(define-custom 'fmt-ja-fold-width 70
  '(fmt-ja)
  '(integer 0 65535)
  (N_ "fold width")
  (N_ "long description will be here."))

(define-custom 'fmt-ja-selection-key '("s")
               '(fmt-ja)
	       '(key)
	       (N_ "[fmt-ja] format selection")
	       (N_ "long description will be here"))

(define-custom 'fmt-ja-clipboard-key '("v")
               '(fmt-ja)
	       '(key)
	       (N_ "[fmt-ja] format clipboard")
	       (N_ "long description will be here"))
