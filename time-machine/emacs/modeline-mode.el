;;; modeline-mode.el --- simple modeline mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  My modeline mode
;;

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup my-modeline nil
  "A simple mode line."
  :prefix "my-modeline-"
  :group 'mode-line)

(defvar my-modeline--default-mode-line mode-line-format
  "The former value of `mode-line-format'.")

;;
;; Faces
;;

(defface my-modeline-unimportant
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements.")

(defface my-modeline-status-modified
  '((t (:inherit (font-lock-variable-name-face))))
  "Face used for the 'modified' indicator symbol in the mode-line.")

(defface my-modeline-status-info
  '((t (:inherit (font-lock-string-face))))
  "Face used for generic status indicators in the mode-line.")

(defface my-modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line.")

(defface my-modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line.")

(defface my-modeline-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line.")

;;
;; Helpers
;;

(defmacro my-modeline-create-segment (name doc &rest body)
  "Create a new segment with NAME, DOC and BODY function for `my-modeline-mode'."
  (let ((segment (intern (format "my-modeline-segment-%s" name)))
        (toggle (intern (format "my-modeline-toggle-%s" name)))
        (show (intern (format "my-modeline-show-%s" name))))
    `(progn
       (defcustom ,show t
         ,(format "Visibility of the %s segment of the mode-line." name)
         :group 'my-modeline
         :type 'boolean)
       (defun ,toggle ()
         ,(format "Toggle visibility of %s segment of the mode-line." name)
         (interactive)
         (customize-save-variable (quote ,show) (not ,show)))
       (defalias
         (quote ,segment)
         (lambda ()
           (when ,show
             ,@body))
         ,doc))))

(defun my-modeline--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and RIGHT-SEGMENTS, aligned respectively."
  (let* ((left (my-modeline--format-segments left-segments))
         (right (my-modeline--format-segments right-segments))
         (reserve (length right)))
    (concat
     left
     (propertize " " 'display `((space :align-to (- right ,reserve))))
     right)))

(defun my-modeline--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
                     segments)))

(defun my-modeline-make-mouse-map (mouse function)
  "Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

(my-modeline-create-segment
 "modified"
 "Displays a color-coded buffer modification/read-only indicator in the mode-line."
 (if (not (string-match-p "\\*.*\\*" (buffer-name)))
     (let* ((read-only (and buffer-read-only (buffer-file-name)))
            (modified (buffer-modified-p)))
       (propertize
        (if read-only " " (if modified " ●" " ○"))
        'face `(:inherit
                ,(if modified 'my-modeline-status-modified
                   (if read-only 'my-modeline-status-error
                     'my-modeline-unimportant)))
        'help-echo (format
                    "Buffer is %s and %smodified\nmouse-1: Toggle read-only status."
                    (if read-only "read-only" "writable")
                    (if modified "" "not "))
        'local-map (purecopy (my-modeline-make-mouse-map
                              'mouse-1
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (read-only-mode 'toggle)))))
        'mouse-face 'mode-line-highlight))))

(my-modeline-create-segment
 "buffer-name"
 "Displays the name of the current buffer in the mode-line."
 (propertize " %b" 'face 'mode-line-buffer-id))

(my-modeline-create-segment
 "position"
 "Displays the current cursor position in the mode-line."
 `((line-number-mode
    ((column-number-mode
      (column-number-indicator-zero-based
       (8 " %l:%c")
       (8 " %l:%C"))
      (5 " L%l")))
    ((column-number-mode
      (column-number-indicator-zero-based
       (5 " C%c")
       (5 " C%C")))))
   ,(if (region-active-p)
        (propertize (format "+%s"
                            (apply #'+ (mapcar
                                       (lambda (pos)
                                         (- (cdr pos)
                                            (car pos)))
                                       (region-bounds))))
                    'font-lock-face 'font-lock-variable-name-face))))

(my-modeline-create-segment
 "vc"
 "Displays color-coded version control information in the mode-line."
 '(vc-mode vc-mode))

(defvar my-modeline-segment-encoding-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (when (and enable-multibyte-characters
		     buffer-file-coding-system)
	    (describe-coding-system buffer-file-coding-system)))))
    (define-key map [mode-line mouse-3]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (call-interactively #'set-buffer-file-coding-system))))
    (purecopy map))
  "Local keymap for the coding-system part of the my-modeline.")

(my-modeline-create-segment
 "encoding"
 "Displays the encoding style of the buffer in the mode-line."
 `(" "
   ,(propertize
     "%z"
     'help-echo
     (lambda (window)
       (with-current-buffer (window-buffer window)
         (if buffer-file-coding-system
             (format "Buffer coding system (%s): %s\nmouse-1: Describe coding system\nmouse-3: Set coding system"
                     (if enable-multibyte-characters "multi-byte" "unibyte")
                     (symbol-name buffer-file-coding-system))
           "Buffer coding system: none specified")))
     'mouse-face 'mode-line-highlight
     'local-map my-modeline-segment-encoding-map)))

(my-modeline-create-segment
 "eol"
 "Displays the EOL style of the current buffer in the mode-line."
 (let* ((eol (coding-system-eol-type buffer-file-coding-system))
        (mnemonic (pcase eol
                    ('0 " LF")
                    ('1 " CRLF")
                    ('2 " CR")
                    (_ "")))
        (desc (pcase eol
                ('0 "Unix-style")
                ('1 "DOS-style")
                ('2 "Mac-style")
                (_ "Undecided"))))
   (propertize
    mnemonic
    'help-echo (format "End-of-line style: %s\nmouse-1: Cycle" desc)
    'local-map (purecopy
                (my-modeline-make-mouse-map
                 'mouse-1
                 (lambda (event)
                   (interactive "e")
                   (with-selected-window (posn-window (event-start event))
                     (let ((eol (coding-system-eol-type buffer-file-coding-system)))
                       (set-buffer-file-coding-system
                        (cond ((eq eol 0) 'dos) ((eq eol 1) 'mac) (t 'unix))))))))
    'mouse-face 'mode-line-highlight)))

(my-modeline-create-segment
 "misc-info"
 "Displays the current value of `mode-line-misc-info' in the mode-line."
 (let ((misc-info (string-trim (format-mode-line mode-line-misc-info 'my-modeline-unimportant))))
   (unless (string= misc-info "")
     (concat " " misc-info))))

(my-modeline-create-segment
 "input-method"
 "Displays the input-method of the buffer in the mode-line."
 `(""
   (current-input-method
    (:propertize (" " current-input-method-title)
                 help-echo (format
                            "Current input method: %s\nmouse-1: Describe current input method"
                            current-input-method)
                 local-map ,(purecopy
                             (my-modeline-make-mouse-map
                              'mouse-1
                              (lambda (e)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start e))
                                  (describe-current-input-method)))))
                 mouse-face 'mode-line-highlight))))

(my-modeline-create-segment
 "minor-modes"
 "Displays the current minor modes in the mode-line."
 (replace-regexp-in-string
   "%" "%%%%"
   (format-mode-line minor-mode-alist)
   t t))

(my-modeline-create-segment
 "process"
 "Displays the current value of `mode-line-process' in the mode-line."
 (when mode-line-process
   (concat " " (string-trim (format-mode-line mode-line-process)))))

(my-modeline-create-segment
 "major-mode"
 "Displays the current major mode in the mode-line."
 (propertize
  (concat " "
          (or (and (boundp 'delighted-modes)
                   (cadr (assq major-mode delighted-modes)))
              (format-mode-line mode-name)))
  'face 'bold))

(defvar my-modeline--mode-line
  '((:eval
     (my-modeline--format
      '(my-modeline-segment-modified
        my-modeline-segment-buffer-name
        my-modeline-segment-position)
      '(my-modeline-segment-minor-modes
        my-modeline-segment-input-method
        my-modeline-segment-eol
        my-modeline-segment-encoding
        my-modeline-segment-vc
        my-modeline-segment-misc-info
        my-modeline-segment-process
        my-modeline-segment-major-mode)))))

;;;###autoload
(define-minor-mode my-modeline-mode
  "Minor mode to get a my mode line.
When called interactively, toggle
`my-modeline-mode'.  With prefix ARG, enable
`my-modeline--mode' if ARG is positive, otherwise
disable it.
When called from Lisp, enable `my-modeline-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `my-modeline-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'my-modeline
  :global t
  (if my-modeline-mode
      (progn
        ;; Set the new mode-line-format
        (setq-default mode-line-format '(:eval my-modeline--mode-line)))
    (progn
      ;; Restore the original mode-line format
      (setq-default mode-line-format my-modeline--default-mode-line))))

(provide 'modeline-mode)

;;; modeline-mode.el ends here
