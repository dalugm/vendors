;;; init-font.el --- Emacs font config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Config for font in Emacs
;;

;;; Code:

;;; First method
;; Default font
(set-face-attribute 'default nil :font "Droid Sans Mono")

;; Latin
(set-fontset-font t 'latin "Noto Sans")

;; East Asia: 你好, 早晨, こんにちは, 안녕하세요
;;
;; Make sure you use the right font. See
;; https://www.google.com/get/noto/help/cjk/.
;;
;; This font requires "Regular". Other Noto fonts dont.
;; ¯\_(ツ)_/¯
(set-fontset-font t 'han "Noto Sans CJK SC Regular")
(set-fontset-font t 'kana "Noto Sans CJK JP Regular")
(set-fontset-font t 'hangul "Noto Sans CJK KR Regular")
(set-fontset-font t 'cjk-misc "Noto Sans CJK KR Regular")

;; South East Asia: ជំរាបសួរ, ສະບາຍດີ, မင်္ဂလာပါ, สวัสดีครับ
(set-fontset-font t 'khmer "Noto Sans Khmer")
(set-fontset-font t 'lao "Noto Sans Lao")
(set-fontset-font t 'burmese "Noto Sans Myanmar")
(set-fontset-font t 'thai "Noto Sans Thai")

;; Africa: ሠላም
(set-fontset-font t 'ethiopic "Noto Sans Ethiopic")

;; Middle/Near East: שלום, السّلام عليكم
(set-fontset-font t 'hebrew "Noto Sans Hebrew")
(set-fontset-font t 'arabic "Noto Sans Arabic")

;;  South Asia: નમસ્તે, नमस्ते, ನಮಸ್ಕಾರ, നമസ്കാരം, ଶୁଣିବେ,
;;              ආයුබෝවන්, வணக்கம், నమస్కారం, བཀྲ་ཤིས་བདེ་ལེགས༎
(set-fontset-font t 'gujarati "Noto Sans Gujarati")
(set-fontset-font t 'devanagari "Noto Sans Devanagari")
(set-fontset-font t 'kannada "Noto Sans Kannada")
(set-fontset-font t 'malayalam "Noto Sans Malayalam")
(set-fontset-font t 'oriya "Noto Sans Oriya")
(set-fontset-font t 'sinhala "Noto Sans Sinhala")
(set-fontset-font t 'tamil "Noto Sans Tamil")
(set-fontset-font t 'telugu "Noto Sans Telugu")
(set-fontset-font t 'tibetan "Noto Sans Tibetan")

;; specify font for all unicode characters
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

;; ----------------------------------------------------------
;; 分別設置對應的中英文
;; NOTE 中文字符大小不能随英文字符缩放，可用`cnfonts'解决
;; 在不使用`cnfonts'的情况下，可通过注释`:size'解决
;; 使用除`华文'系列外中文字体会导致中英文不等高现象过于显著
;; ----------------------------------------------------------
(when (display-graphic-p)
  ;; English Font
  (set-face-attribute
   'default nil
   :font (font-spec
          ;; `:name' `:family' 两者只留其一即可
          :family "Hack Nerd Font"
          ;; :name "-*-Hack Nerd Font-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
          :weight 'normal
          :slant 'normal
          :size 14.0))
  ;; Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec
      ;; `:name' `:family' 两者只留其一即可
      :family "STKaiti"
      ;; :name "-*-STKaiti-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
      :weight 'normal
      :slant 'normal
      :size 18.0))))



;;; Second way
(defvar my-font-alist
  '(
    (lxgw-mono-16           . (:family "LXGW WenKai Mono" :size 16))
    (lxgw-mono-28           . (:family "LXGW WenKai Mono" :size 28))
    (gnu-unifont-16         . (:family "Unifont" :size 16))
    (gnu-unifont-32         . (:family "Unifont" :size 32))
    (firacode-14            . (:family "Fira Code" :size 14))
    (roboto-mono-14         . (:family "Roboto Mono" :size 14))
    (monaco-14              . (:family "Monaco" :size 14))
    (menlo-14               . (:family "Menlo" :size 14))
    (spot-mono-14           . (:family "Spot Mono" :size 14))
    (sf-mono-14             . (:family "SF Mono" :size 14))
    (hack-14                . (:family "Hack" :size 14))
    (sarasa-mono-sc-16      . (:family "Sarasa Mono SC" :size 16))
    (sarasa-mono-sc-28      . (:family "Sarasa Mono SC" :size 28))
    (sarasa-mono-slab-sc-16 . (:family "Sarasa Mono Slab SC" :size 16))
    (sarasa-mono-slab-sc-28 . (:family "Sarasa Mono Slab SC" :size 28))
    (wenquanyi-16           . (:family "WenQuanYi Zen Hei Mono" :size 16))
    (roboto-16              . (:family "Roboto" :size 16))
    (fangzheng-fangsong-16  . (:family "FZFangSong-Z02" :size 16))
    (fangzheng-gwfs-16      . (:family "FZDocFangSong" :size 16))
    (fangzheng-gwhw-16      . (:family "FZDocHei" :size 16))
    (fangzheng-gwkt-16      . (:family "FZDocKai" :size 16))
    (fangzheng-gwxbs-16     . (:family "FZDocXiaoBiaoSong" :size 16))
    (fangzheng-heiti-16     . (:family "FZHei-B01" :size 16))
    (fangzheng-kaiti-16     . (:family "FZKai-Z03" :size 16))
    (fangzheng-pxys-16      . (:family "FZPingXianYaSong-R-GBK" :size 16))
    (fangzheng-shusong-16   . (:family "FZShuSong-Z01" :size 16))
    (hiragino-sans-cns      . (:family "Hiragino Sans CNS" :size 16))
    (hiragino-sans-gb       . (:family "Hiragino Sans GB" :size 16))
    (huawen-kaiti-sc-16     . (:family "Kaiti SC" :size 16))
    (huawen-kaiti-tc-16     . (:family "Kaiti TC" :size 16))
    (lantinghei-sc-16       . (:family "Lantinghei SC" :size 16))
    (lantinghei-tc-16       . (:family "Lantinghei TC" :size 16))
    (lxgw-16                . (:family "LXGW WenKai" :size 16))
    (sarasa-gothic-sc-16    . (:family "Sarasa Gothic SC" :size 16))
    (sarasa-ui-sc-16        . (:family "Sarasa UI SC" :size 16))
    (source-han-sans-sc-16  . (:family "Source Han Sans SC" :size 16))
    (source-han-sans-tc-16  . (:family "Source Han Sans TC" :size 16))
    (source-han-serif-sc-16 . (:family "Source Han Serif SC" :size 16))
    (source-han-serif-tc-16 . (:family "Source Han Serif TC" :size 16))
    )
  "An alist of all the fonts you can switch between by `my-load-font'.
Key is a symbol as the name, value is a plist specifying the font spec.
More info about spec in `font-spec'.")

(defun my-load-buffer-font (&optional font-name)
  "Prompt for a FONT-NAME and set it for current buffer.
Fonts are specified in `my-font-alist'."
  (interactive (list
                (completing-read "Choose a font for current buffer: "
                                 (mapcar #'car my-font-alist))))
  (let* ((font-name (or font-name my-font))
         (font (apply #'font-spec
                      (if font-name
                          (alist-get (intern font-name) my-font-alist
                                     nil nil #'equal)
                        (cdar my-font-alist)))))
    ;; use `face-remapping-alist' instead of `buffer-face-mode-face'
    (set (make-local-variable 'face-remapping-alist)
         (copy-tree `((default
                       :font ,font
                       :height ,(* 10 (font-get `,font ':size))))))))

(defun my-load-font (&optional font-name)
  "Prompt for a FONT-NAME and set it.
Fonts are specified in `my-font-alist'.  If FONT-NAME non-nil,
use it instead."
  (interactive (list
                (completing-read "Choose a font: "
                                 (mapcar #'car my-font-alist))))
  (let* ((font-name (or font-name my-font))
         (font (apply #'font-spec
                      (if font-name
                          (alist-get (intern font-name) my-font-alist
                                     nil nil #'equal)
                        ;; If `font-name' is nil (loading from local
                        ;; file and don't have it saved), use the first
                        ;; font spec.
                        (cdar my-font-alist)))))
    (set-frame-font font nil t)
    ;; seems that there isn't a good way to get font-object directly
    (add-to-list 'default-frame-alist
                 `(font . ,(face-attribute 'default :font)))
    (when (or font-name (not (custom-variable-p my-font)))
      (customize-set-variable 'my-font font-name))))

(provide 'init-font)

;;; init-font.el ends here
