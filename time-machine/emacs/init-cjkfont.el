;;; init-cjkfont.el --- cjk ugly config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Config for cjk font in Emacs
;;

;;; Code:

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


(provide 'init-cjkfont)

;;; init-cjkfont.el ends here
