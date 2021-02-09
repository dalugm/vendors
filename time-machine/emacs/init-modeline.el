;;; init-modeline.el --- mode-line -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  make mode-line's format suitable for reading
;;

;;; Code:

;; modeline indicator, bundle with `evil'
(defconst my-default-color (cons (face-background 'mode-line)
                             (face-foreground 'mode-line))
  "Default mode-line color.")
(defun my//show-current-state ()
  "Change mode line color to notify user evil current state."
  (let ((color (cond
                 ((minibufferp)          my-default-color)
                 ((evil-emacs-state-p)   '("#7e1671" . "#f8f4ed"))
                 ((evil-insert-state-p)  '("#20894d" . "#f8f4ed"))
                 ((evil-visual-state-p)  '("#ffd111" . "#f8f4ed"))
                 ((evil-replace-state-p) '("#de1c31" . "#f8f4ed"))
                 ((buffer-modified-p)    '("#1772b4" . "#f8f4ed"))
                 (t                      my-default-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))
(add-hook 'post-command-hook #'my//show-current-state)

;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use `setq-default' to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize " %b " 'face nil 'help-echo (buffer-file-name)))

    ;; line and column
    "("
    ;; '%01' to set to 1 chars at least; prevents flickering
    "%l" "," "%C"
    ") "

    ;; current file
    "<"
    ;; percent of buffer above top of window
    (propertize "%p" 'face 'nil)
    ;; judge between local and remote
    "%@"
    ;; the size of the buffer
    (propertize "%I" 'face 'nil)
    "> "

    ;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Echo.html
    "["

    ;; the current major mode for the buffer.
    '(:eval (propertize "%m" 'face nil 'help-echo buffer-file-coding-system))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat "/"  (propertize "Mod"
                             'face nil
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO" 'face nil 'help-echo "Buffer is read-only"))))
    "] "

    ;; global-mode-string, org-timer-set-timer in org-mode need this
    ;; display-time
    (propertize "%M" 'face nil)

    " -- "

    ;; buffer encoding system
    '(:eval (let ((sys (coding-system-plist buffer-file-coding-system)))
              (if (memq (plist-get sys :category)
                    '(coding-category-undecided coding-category-utf-8))
                "UTF-8"
                (upcase (symbol-name (plist-get sys :name))))))
    " "

    ;; file eol type
    '(:eval (let ((eol (coding-system-eol-type buffer-file-coding-system)))
              (propertize
                (pcase eol
                  (0 "LF ")
                  (1 "CRLF ")
                  (2 "CR ")
                  (_ "")))))

    ;; Don't show `minor-mode'
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
    ))

;; {{ time format
;; If you want to customize time format, read document of `format-time-string'
;; and customize `display-time-format'.
;; (setq display-time-format "%a %b %e")

;; from RobinH, Time management
(setq system-time-locale "C")
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil) ; do NOT display the load average
(display-time) ; show date in mode-line
;; }}

;; 执行速度较慢
;; ----- `my--mode-line-buffer-identification' -------------
;; more informative than `buffer-id'
(defvar my--mode-line-buffer-identification
  (list
    '(:eval (when (buffer-modified-p)
              (propertize "[" 'face nil 'help-echo nil)))
    '(:eval (propertize "%b"
              ;; was this buffer modified since the last save?
              'face (cond
                      ((buffer-modified-p)
                        '(error mode-line-buffer-id))
                      (t
                        'nil))
              'help-echo buffer-file-name))
    '(:eval (when (buffer-modified-p)
              (concat "]" (propertize "*"
                            'face nil
                            'help-echo "Bufer has been modified"))))
    '(:eval (buffer-read-only (:propertize " RO " face warning))))
  "More informative than `buffer-id'.")
(put 'my--mode-line-buffer-identification 'risky-local-variable t)

(provide 'init-modeline)

;;; init-modeline.el ends here
