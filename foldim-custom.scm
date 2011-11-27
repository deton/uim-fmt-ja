(require "i18n.scm")

(define foldim-im-name-label (N_ "foldim"))
(define foldim-im-short-desc (N_ "fold IM"))

(define-custom-group 'foldim
                     foldim-im-name-label
                     foldim-im-short-desc)

(define-custom 'foldim-fold-width 70
  '(foldim)
  '(integer 0 65535)
  (N_ "fold width")
  (N_ "long description will be here."))

(define-custom 'foldim-selection-key '("s")
               '(foldim)
	       '(key)
	       (N_ "[foldim] fold on selection")
	       (N_ "long description will be here"))

(define-custom 'foldim-clipboard-key '("v")
               '(foldim)
	       '(key)
	       (N_ "[foldim] fold on clipboard")
	       (N_ "long description will be here"))
