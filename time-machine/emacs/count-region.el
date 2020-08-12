;;; count-region.el --- count selected region -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  count selected region's chars, words and lines.
;;

;;; Code:

(defun my/region-char ()
  "Compute the number of characters in the selected region."
  (interactive)
  (message (format "Total %d char(s)." (- (region-end) (region-beginning)))))

(defun my/region-line ()
  "Compute the number of lines in the selected region."
  (interactive)
  (message (format "Total %d line(s)."
             (count-lines (region-beginning) (min (region-end) (point-max))))))

(defun my/region-word ()
  "Compute the number of words in the selected region."
  (interactive)
  (message (format "Total %d word(s)."
             (count-words (region-beginning) (min (region-end) (point-max))))))

(provide 'count-region)

;;; count-region.el ends here
