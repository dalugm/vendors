;;; doom-modeline-light.el --- doom-modeline format -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  modeline configuration, referring to hlissner's configuration
;;

;;; Code:

;; current structure use `doom-emacs' for reference
;; divide mode line segments
;; @see https://github.com/hlissner/doom-emacs-private/blob/master/lisp/modeline.el

;;
;;; Segments
;;

;; ----- `my--mode-line-modes' ---------------------------------
(defvar my--mode-line-modes ; remove minor modes
  '(:eval
    (propertize mode-name
      'face bold
      'mouse-face mode-line-highlight)
    mode-line-process
    "%n"
    "%]"
    " "))

;; ----- `my--mode-line-buffer-identification' -----------------
(defvar my--mode-line-buffer-identification ; slightly more informative buffer-id
  '(:eval
     (propertize "%b"
       'face (cond ((buffer-modified-p)
                     '(error mode-line-buffer-id))
               ((active)
                 'mode-line-buffer-id))
       'help-echo buffer-file-name))
  (buffer-read-only (:propertize " RO" face nil)))

;; ----- `my--mode-line-position' ------------------------------
(defvar my--mode-line-position '(" (%l,%C) "))

;; ----- `my--mode-line-current-file' ------------------------------
(defvar my--mode-line-current-file '(" <%p%@%I> "))

;; ----- `my--mode-line-encoding' ------------------------------
(defvar my--mode-line-encoding
  '(:eval
     (concat (let ((eol (coding-system-eol-type buffer-file-coding-system)))
               (pcase eol
                 (0 "LF ")
                 (1 "CRLF ")
                 (2 "CR ")
                 (_ "")))
       (let ((sys (coding-system-plist buffer-file-coding-system)))
         (if (memq (plist-get sys :category)
               '(coding-category-undecided coding-category-utf-8))
           "UTF-8"
           (upcase (symbol-name (plist-get sys :name)))))
       "  ")))

;;
;;; Functions
;;

(defun my//mode-line-fill-right (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun my//mode-line-fill-center (face reserve)
  "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))

(defconst RIGHT_PADDING 2)

(setq mode-line-align-left
      '(""
        "%1 "
        "%2 "
        (:propertize "%b" face mode-line-buffer-id)
        " "))

(setq mode-line-align-middle
      '(""
        (vc-mode vc-mode)
        "%1 "
        " "
        (:eval
         (when (eql buffer-read-only t)
           (propertize "Read-Only" 'face '(:foreground "#3a3a3a" :height .9))))
        ""))

(setq mode-line-align-right
      '(""
        ;; mode-line-misc-info
        "%1 "
        (:eval (format "%%l/%d:%%c " (line-number-at-pos (point-max))))))

(defun my//reserve-left-middle ()
  "Reserve space from `left' to `middle'."
  (/ (length (format-mode-line mode-line-align-middle)) 2))

(defun my//reserve-middle-right ()
  "Reserve space from `middle' to `right'."
  (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

;;
;;; Setup
;;

(defvar-local my--mode-line-format-left nil
  "Mode line left segments.")

(defvar-local my--mode-line-format-right nil
  "Mode line right segments.")

(setq-default my--mode-line-format-left
  '(" "
     my--mode-line-buffer-identification
     my--mode-line-position
     my--mode-line-current-file
     ))

(setq-default my--mode-line-format-right
  '(""
     "%M"
     mode-line-misc-info
     my--mode-line-modes
     (vc-mode vc-mode)
     " "
     my--mode-line-encoding
     ))

(setq-default mode-line-format
  '(""
     my--mode-line-format-left
     (:eval
       (propertize
         " "
         'display
         `((space :align-to (- (+ right right-fringe right-margin)
                              ,(string-width
                                 (format-mode-line '("" mode-line-format-right))))))))
     my--mode-line-format-right
     ))

;;
;;; Misc
;;

(with-current-buffer "*Messages*"
  (setq mode-line-format (default-value 'mode-line-format)))

(defun my/toggle-mode-line ()
  "Toggle `mode-line-format'."
  (interactive)
  (cond ((eq mode-line-format (default-value mode-line-format))
         (setq-default mode-line-format my/mode-line-fromat))
        (t (setq-default mode-line-format (default-value mode-line-format)))))

(defun my/default-mode-line ()
  "Use default `mode-line-format'."
  (interactive)
  (setq-default mode-line-format (default-value mode-line-format)))

(defun my/custom-mode-line ()
  "Use customized `mode-line-format'."
  (interactive)
  (setq-default mode-line-format my-mode-line-fromat))

(provide 'doom-modeline-light)

;;; doom-modeline-light.el ends here