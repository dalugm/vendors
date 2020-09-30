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

(provide 'misc-funcs)

;;; misc-funcs.el ends here