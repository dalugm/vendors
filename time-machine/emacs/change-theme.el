;;; change-theme.el --- change theme -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  change theme according time.
;;

;;; Code:

(defvar dalu-themes-status "init")

(defun dalu-themes-load-light ()
  "Load light theme."
  (load-theme 'dalu-light t)
  (setq dalu-themes-status "light"))

(defun dalu-themes-load-dark ()
  "Load dark theme."
  (load-theme 'dalu-dark t)
  (setq dalu-themes-status "dark"))

(defun dalu-themes-toggle ()
  "Toggle between dark and light themes."
  (interactive)
  (let ((bg-mode (frame-parameter nil 'background-mode)))
    (if (eq bg-mode 'dark)
        (dalu-themes-load-light)
      (dalu-themes-load-dark))))

(defun dalu-themes--is-day ()
  "Confirm daytime."
  (let ((current-hour (string-to-number (format-time-string "%H"))))
    (and (> current-hour 7)
         (< current-hour 18))))

(defun dalu-themes-load ()
  "Load theme according current time."
  (if (dalu-themes--is-day)
      (when (or (string-equal dalu-themes-status "init")
                (string-equal dalu-themes-status "dark"))
        (dalu-themes-load-light))
    (when (or (string-equal dalu-themes-status "init")
              (string-equal dalu-themes-status "light"))
      (dalu-themes-load-dark))))

(defun dalu-themes-load-with-sunrise ()
  "Check time every hour then adjust theme with sunrise."
  (run-with-timer 0 (* 60 60) 'dalu-themes-load))

(provide 'change-theme)

;;; change-theme.el ends here
