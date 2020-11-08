;;; stupid-indent.el --- a minor indent mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  A simpile and stupid indent minor mode
;;

;;; Code:

(defvar stupid-indent-offset 4
  "Default indentation offset.")

(defun stupid-force-indent-line ()
  "Force indent line."
  (interactive)
  (let (col)
    (save-excursion
      (back-to-indentation)
      (setq col (+ (current-column) stupid-indent-offset))
      (indent-line-to col))
    (when (< (current-column) col)
      (back-to-indentation))))

(defun stupid-indent-line()
  "Indent line."
  (interactive)
  (let ((bt (save-excursion
              (back-to-indentation)
              (current-column))))
    (cond
      ((< (current-column) bt)
        (back-to-indentation))
      ((looking-at "\\s-*\n")
        (let ((col (save-excursion
                     (forward-line -1)
                     (back-to-indentation)
                     (current-column))))
          (if (< (current-column) col)
            (indent-line-to col)
            (stupid-force-indent-line))))
      (t
        (stupid-force-indent-line)))))

(defun stupid-outdent-line ()
  "Outdent line."
  (interactive)
  (let (col)
    (save-excursion
      (back-to-indentation)
      (setq col (- (current-column) stupid-indent-offset))
      (when (>= col 0)
        (indent-line-to col)))))

(defun stupid-indent-region (start stop)
  "Indent region from START to STOP."
  (interactive "r")
  (setq stop (copy-marker stop))
  (goto-char start)
  (while (< (point) stop)
    (unless (and (bolp) (eolp))
      (stupid-force-indent-line))
    (forward-line 1)))

(defun stupid-outdent-region (start stop)
  "Outdent region from START to STOP."
  (interactive "r")
  (setq stop (copy-marker stop))
  (goto-char start)
  (while (< (point) stop)
    (unless (and (bolp) (eolp))
      (stupid-outdent-line))
    (forward-line 1)))

(defun stupid-indent ()
  "Indent function."
  (interactive)
  (if (use-region-p)
    (save-excursion
      (stupid-indent-region (region-beginning) (region-end))
      (setq deactivate-mark nil))
    (stupid-indent-line)))

(defun stupid-outdent ()
  "Outdent function."
  (interactive)
  (if (use-region-p)
      (save-excursion
       (stupid-outdent-region (region-beginning) (region-end))
       (setq deactivate-mark nil))
      (stupid-outdent-line)))

(defun stupid-indent-newline ()
  "Indent with newline."
  (interactive)
  (when (< (point)
          (save-excursion
            (back-to-indentation)
            (point)))
    (back-to-indentation))
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (newline)
    (indent-to-column col)))

(define-minor-mode stupid-indent-mode
  "Stupid indent mode is just plain stupid."
  :init-value nil
  :lighter "/SI"
  :global nil
  :keymap `(
            (,(kbd "<tab>") . stupid-indent)
            (,(kbd "<backtab>") . stupid-outdent)
            (,(kbd "C-c <tab>") . stupid-indent-region)
            (,(kbd "C-c <backtab>") . stupid-outdent-region)
            (,(kbd "<return>") . stupid-indent-newline)
            (,(kbd "C-c C-<tab>") . indent-according-to-mode)
            ))

(provide 'stupid-indent)

;;; stupid-indent.el ends here
