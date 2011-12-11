(require "i18n.scm")

(define fmt-ja-im-name-label (N_ "fmt-ja"))
(define fmt-ja-im-short-desc (N_ "format selection or clipboard like fmt command"))

(define-custom-group 'fmt-ja
                     fmt-ja-im-name-label
                     fmt-ja-im-short-desc)

(define-custom 'fmt-ja-goal-width 70
  '(fmt-ja)
  '(integer 1 65535)
  (N_ "goal width")
  (N_ "long description will be here."))

(define-custom 'fmt-ja-max-width 72
  '(fmt-ja)
  '(integer 1 65535)
  (N_ "max width")
  (N_ "long description will be here."))

(define-custom 'fmt-ja-fold-only #f
  '(fmt-ja)
  '(boolean)
  (N_ "fold long lines only, do not join")
  (N_ "long description will be here."))

(define (fmt-ja-iconv enc-to enc-from str)
  (let* ((ic (iconv-open enc-to enc-from))
         (converted-str (and ic (iconv-code-conv ic str))))
    (if ic (iconv-release ic))
    converted-str))

(define (fmt-ja-eucjp->utf8 eucjp-str)
  (fmt-ja-iconv "UTF-8" "EUC-JP" eucjp-str))

(define (fmt-ja-utf8->eucjp utf8-str)
  (fmt-ja-iconv "EUC-JP" "UTF-8" utf8-str))

(define fmt-ja-kinsoku-chars-on-start-internal
  "¡¼¡Ý¡Á¡¢¡£¡¤¡¥¡§¡¨¡©¡ª¡«¡¬¡ë¡ì¡í¡î¡ñ¢ó¡³¡´¡µ¡¶¡Ä¡Å¡¹¡¾¡¦­á¡Ç¡É¡Ë¡Í¡Ï¡Ñ¡Ó¡Õ¡×¡Ù¡Û¤¡¤£¤¥¤§¤©¤Ã¤ã¤å¤ç¤î¥¡¥£¥¥¥§¥©¥Ã¥ã¥å¥ç¥î¥õ¥ö.,:;!?>)}]-")

(define-custom 'fmt-ja-kinsoku-chars-on-start
  (fmt-ja-eucjp->utf8 fmt-ja-kinsoku-chars-on-start-internal)
  '(fmt-ja)
  '(string ".*")
  (N_ "Kinsoku characters on start of line")
  (N_ "long description will be here."))

(define fmt-ja-kinsoku-chars-on-end-internal
  "­à¡Æ¡È¡Ê¡Ì¡Î¡Ð¡Ò¡Ô¡Ö¡Ø¡Ú<({[")

(define-custom 'fmt-ja-kinsoku-chars-on-end
  (fmt-ja-eucjp->utf8 fmt-ja-kinsoku-chars-on-end-internal)
  '(fmt-ja)
  '(string ".*")
  (N_ "Kinsoku characters on end of line")
  (N_ "long description will be here."))

(define-custom 'fmt-ja-tab-width 8
  '(fmt-ja)
  '(integer 0 65535)
  (N_ "tab width")
  (N_ "long description will be here."))

(define-custom 'fmt-ja-new-paragraph-by-indent-change #f
  '(fmt-ja)
  '(boolean)
  (N_ "new paragraph by indent change")
  (N_ "long description will be here."))

(define-custom 'fmt-ja-selection-key '("s")
               '(fmt-ja)
	       '(key)
	       (N_ "[fmt-ja] format selection")
	       (N_ "long description will be here"))

(define-custom 'fmt-ja-clipboard-key '("<IgnoreCase><Shift>s")
               '(fmt-ja)
	       '(key)
	       (N_ "[fmt-ja] format clipboard")
	       (N_ "long description will be here"))

(custom-add-hook 'fmt-ja-kinsoku-chars-on-start
  'custom-set-hooks
  (lambda ()
    (set! fmt-ja-kinsoku-chars-on-start-internal
      (fmt-ja-utf8->eucjp fmt-ja-kinsoku-chars-on-start))))

(custom-add-hook 'fmt-ja-kinsoku-chars-on-end
  'custom-set-hooks
  (lambda ()
    (set! fmt-ja-kinsoku-chars-on-end-internal
      (fmt-ja-utf8->eucjp fmt-ja-kinsoku-chars-on-end))))

(custom-add-hook 'fmt-ja-max-width
  'custom-get-hooks
  (lambda ()
    (if (< fmt-ja-max-width fmt-ja-goal-width)
      (set! fmt-ja-max-width fmt-ja-goal-width))))

(custom-add-hook 'fmt-ja-goal-width
  'custom-set-hooks
  (lambda ()
    (if (< fmt-ja-max-width fmt-ja-goal-width)
      (custom-set-value! 'fmt-ja-max-width fmt-ja-goal-width))))
