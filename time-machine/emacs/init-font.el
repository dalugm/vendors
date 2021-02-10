;;; init-font.el --- Emacs font config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Config for font in Emacs
;;

;;; Code:

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


(provide 'init-font)

;;; init-font.el ends here
