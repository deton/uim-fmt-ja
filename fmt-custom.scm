(require "i18n.scm")

(define fmt-im-name-label (N_ "fmt"))
(define fmt-im-short-desc (N_ "format selection or clipboard like fmt command"))

(define-custom-group 'fmt
                     fmt-im-name-label
                     fmt-im-short-desc)

(define-custom 'fmt-fold-width 70
  '(fmt)
  '(integer 0 65535)
  (N_ "fold width")
  (N_ "long description will be here."))

(define-custom 'fmt-selection-key '("s")
               '(fmt)
	       '(key)
	       (N_ "[fmt] format selection")
	       (N_ "long description will be here"))

(define-custom 'fmt-clipboard-key '("v")
               '(fmt)
	       '(key)
	       (N_ "[fmt] format clipboard")
	       (N_ "long description will be here"))
