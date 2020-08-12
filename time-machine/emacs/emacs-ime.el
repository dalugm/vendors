;;; emacs-ime.el --- backup IME configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  IME configuration
;;

;;; Code:

;; make IME compatible with evil-mode
(defun my/evil-toggle-input-method ()
  "When input method is on, go to `evil-insert-state'.
Quit `evil-insert-state' when input method is off."
  (interactive)
  (cond
    ;; evil-mode
    ((and (boundp 'evil-mode) evil-mode)
      (cond
        ((eq evil-state 'insert)
          (toggle-input-method))
        (t
          (evil-insert-state)
          (unless current-input-method
            (toggle-input-method))))
      (cond
        (current-input-method
          (evil-insert-state)
          (message "IME on!"))
        (t
          (evil-normal-state)
          (message "IME off!"))))
    ;; NOT evil-mode
    (t
      (toggle-input-method))))

(defun my//evil-insert-state-hack (orig-func &rest args)
  "Notify user IME status by applying ORIG-FUNC with ARGS."
  (apply orig-func args)
  (if current-input-method (message "IME on!")))
(advice-add 'evil-insert-state :around #'my//evil-insert-state-hack)

;; ----- Emacs-rime ----------------------------------------
(use-package rime
  :bind (("C-\\" . my/evil-toggle-input-method)
          :map rime-active-mode-map
          ("M-j" . rime-inline-ascii)
          :map rime-mode-map
          ("C-`" . rime-send-keybinding))
  :init
  (setq default-input-method "rime")
  (setq rime-cursor "˰")
  (setq rime-show-candidate 'minibuffer)
  (setq rime-librime-root (expand-file-name "librime/dist" my-optional-d))
  (setq rime-user-data-dir (expand-file-name "rime" my-cache-d)))

;; ----- pyim ----------------------------------------------
(use-package pyim
  :bind ("C-\\" . (my/evil-toggle-input-method))
  :init
  (setq default-input-method "pyim")
  (defvar my-pyim-directory (expand-file-name "pyim/" my-cache-d)
    "The directory containing pyim dictionaries.")

  :config
  ;; -------------------------------------------------------
  ;; basic config
  ;; -------------------------------------------------------

  ;; 使用全拼
  (setq pyim-default-scheme 'quanpin)

  ;; 绘制选词框
  ;; compatible with terminal
  (setq pyim-page-tooltip 'minibuffer)
  ;; 调整 tooltip 选词框的显示样式
  (setq pyim-page-style 'two-lines)
  ;; 选词框显示 9 个候选词
  (setq pyim-page-length 9)
  (setq pyim-posframe-min-width 0)

  ;; ;; Rime config
  ;; (liberime-start
  ;;   (if (featurep 'cocoa)
  ;;     "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"
  ;;     "/usr/share/rime-data")
  ;;   (expand-file-name "rime/" my-cache-d))
  ;; (liberime-select-schema "luna_pinyin")

  ;; fuzzy config
  (setq pyim-fuzzy-pinyin-alist
    '(("en" "eng")
       ("in" "ing")))

  ;; use memory efficient pyim engine
  (setq pyim-dcache-backend 'pyim-dregcache)
  (setq pyim-dcache-directory (expand-file-name "pyim/dcache/" my-cache-d))
  ;; don't use shortcode2word
  (setq pyim-enable-shortcode nil)

  ;; -------------------------------------------------------
  ;; dict
  ;; -------------------------------------------------------

  ;;  pyim-bigdict is recommended (20M).
  ;; There are many useless words in pyim-greatdict
  ;; which also slows down pyim performance.
  ;; `curl -L http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz | zcat > path/to/pyim-bigdict.pyim`
  ;; automatically load all "*.pyim" under `my-cache-d'
  ;; `directory-files-recursively' requires Emacs 25
  (let ((files (and (file-exists-p my-pyim-directory)
                 (directory-files-recursively my-pyim-directory "\.pyim$")))
         disable-basedict)
    (when (and files (> (length files) 0))
      (setq pyim-dicts
        (mapcar (lambda (f)
                  (list :name (file-name-base f) :file f))
          files))
      ;; disable basedict if bigdict or greatdict is used
      (dolist (f files)
        (when (or (string= "pyim-bigdict" (file-name-base f))
                (string= "pyim-greatdict" (file-name-base f)))
          (setq disable-basedict t))))
    (unless disable-basedict (pyim-basedict-enable)))

  ;; -------------------------------------------------------
  ;; keybinding
  ;; -------------------------------------------------------

  (global-set-key (kbd "M-i")   'pyim-convert-string-at-point)
  (define-key pyim-mode-map "," 'pyim-page-previous-page)
  (define-key pyim-mode-map "." 'pyim-page-next-page))

(provide 'emacs-ime)

;;; emacs-ime.el ends here
