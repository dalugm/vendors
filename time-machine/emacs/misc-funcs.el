;;; misc-funcs.el --- functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Functions that may helpful in the future.
;;

;;; Code:

;;; Dired:

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

(provide 'misc-funcs)

;;; misc-funcs.el ends here