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

(define-custom 'fmt-ja-kinsoku-chars-on-start
  "¡¼¡Ý¡½¡Á¡¢¡£¡¤¡¥¡§¡¨¡©¡ª¡«¡¬¡ë¡ì¡í¡î¡ñ¢ó¡³¡´¡µ¡¶¡Ä¡Å¡¹¡¾¡¦­á¡Ç¡É¡Ë¡Í¡Ï¡Ñ¡Ó¡Õ¡×¡Ù¡Û¤¡¤£¤¥¤§¤©¤Ã¤ã¤å¤ç¤î¥¡¥£¥¥¥§¥©¥Ã¥ã¥å¥ç¥î¥õ¥ö.,:;!?>)}]-"
  '(fmt-ja)
  '(string ".*")
  (N_ "Kinsoku characters on start of line")
  (N_ "long description will be here."))

(define-custom 'fmt-ja-kinsoku-chars-on-end
  "­à¡Æ¡È¡Ê¡Ì¡Î¡Ð¡Ò¡Ô¡Ö¡Ø¡Ú<({["
  '(fmt-ja)
  '(string ".*")
  (N_ "Kinsoku characters on end of line")
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
