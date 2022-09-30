;;; misc-funcs.el --- functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Functions that may helpful in the future.
;;

;;; Code:

;;; Dired:

;; ---------------------------------------------------------
;;; Universal, non-nuclear escape
;; ---------------------------------------------------------

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar my-escape-hook nil
  "A hook run when \\<`C-g'> is pressed (or \\<`ESC'>> in normal mode, for evil users).
More specifically, when \\[my/escape] is pressed.  If any hook returns non-nil,
all hooks after it are ignored.")

(defun my/escape ()
  "Run `my-escape-hook'."
  (interactive)
  (cond
    ((minibuffer-window-active-p (minibuffer-window))
      ;; quit the minibuffer if open.
      (abort-recursive-edit))
    ;; Run all escape hooks. If any returns non-nil, then stop there.
    ((run-hook-with-args-until-success 'my-escape-hook))
    ;; don't abort macros
    ((or defining-kbd-macro executing-kbd-macro) nil)
    ;; save excursion and back to normal state when using `evil'
    ((or (evil-visual-state-p) (evil-replace-state-p)) (evil-normal-state))
    ;; Back to the default
    (t (keyboard-quit))))

(global-set-key [remap keyboard-quit] #'my/escape)

(defun convert-video-to-gif ()
  "Convert video file to gif using ffmpeg."
  (interactive)
  (let ((video-file (file-name-nondirectory (dired-file-name-at-point)))
        (rates (read-string "Please input frame rates you want (e.g, \"8\") or press [ENTER] to use default 8 frame rate: " nil nil "8"))
        (compress (read-string "Do you want to compress this video? (y or n): "))
        cmd)
    (cond
      ((string= compress "y")
        ;; DO compress video
        (setq cmd (format "ffmpeg -i \"%s\" -vf scale=iw/2:ih/2 -b:v 5000k -r \"%s\" -an \"%s\""
                    video-file
                    rates
                    (concat (file-name-base video-file) ".gif"))))
      ((string= compress "n")
        ;; do NOT compress video
        (setq cmd (format "ffmpeg -i \"%s\" -b:v 5000k -minrate 5000k -maxrate 5000k -bufsize 4200k -preset fast -ab 128k -r \"%s\" -an \"%s\""
                    video-file
                    rates
                    (format "%s.gif" (file-name-base video-file))))))
    (async-shell-command cmd)))

(defun my/cjk-current-buffer ()
  "Use cjk font in current buffer."
  (interactive)
  (make-face 'width-font-face)
  (set-face-attribute 'width-font-face nil :font "Sarasa Fixed SC 16")
  (setq buffer-face-mode-face 'width-font-face)
  (buffer-face-mode))

(defvar my-font nil
  "Used to cache configuration across sessions.")

(defvar my-font-alist
  '(
     (sarasa-mono-slab-sc-16 . (:family "Sarasa Mono Slab SC" :size 16))
     (sarasa-mono-sc-16      . (:family "Sarasa Mono SC" :size 16))
     (sarasa-gothic-sc-16    . (:family "Sarasa Gothic SC" :size 16))
     (sarasa-ui-sc-16        . (:family "Sarasa UI SC" :size 16))
     (gnu-unifont-16         . (:family "Unifont" :size 16))
     (sourcecodepro-nerd-14  . (:family "SauceCodePro Nerd Font" :size 14))
     (wenquanyi-16           . (:family "WenQuanYi Zen Hei Mono" :size 16))
     (firacode-nerd-14       . (:family "FiraCode Nerd Font" :size 14))
     (hack-nerd-14           . (:family "Hack Nerd Font" :size 14))
     (monaco-14              . (:family "Monaco" :size 14))
     (sf-mono-14             . (:family "SF Mono" :size 14))
     (source-han-sans-sc-16  . (:family "Source Han Sans SC" :size 16))
     (source-han-sans-tc-16  . (:family "Source Han Sans TC" :size 16))
     (source-han-serif-sc-16 . (:family "Source Han Serif SC" :size 16))
     (source-han-serif-tc-16 . (:family "Source Han Serif TC" :size 16))
     )
  "An alist of all the fonts you can switch between by `my/load-font'.
Key is a symbol as the name, value is a plist specifying the font spec.
More info about spec in `font-spec'.")

;; `buffer-face-mode' version
(defun my/load-buffer-font-1 (&optional font-name)
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
    (setq buffer-face-mode-face `(:font ,font :height ,(* 10 (font-get `,font ':size))))
    (buffer-face-mode +1)))

;; `face-remapping-alist' version
(defun my/load-buffer-font-2 (&optional font-name)
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
    (set (make-local-variable 'face-remapping-alist)
      (copy-tree `((default :font ,font :height ,(* 10 (font-get `,font ':size))))))))

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
          (message "IME off!"))))
    ;; NOT evil-mode
    (t
      (toggle-input-method))))

(defun my//evil-insert-state-hack (orig-func &rest args)
  "Notify user IME status by applying ORIG-FUNC with ARGS."
  (apply orig-func args)
  (if current-input-method (message "IME on!")))
(advice-add 'evil-insert-state :around #'my//evil-insert-state-hack)

(defun my--add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist'.
Use specified MODE for all given file PATTERNS."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun my--add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist'.
Use specified MODE for all given file PATTERNS."
  (dolist (pattern patterns)
    (add-to-list 'interpreter-mode-alist (cons pattern mode))))

(provide 'misc-funcs)

;;; misc-funcs.el ends here
