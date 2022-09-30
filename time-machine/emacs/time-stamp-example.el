(add-hook 'org-mode-hook
          (lambda ()
            (setq-local time-stamp-active t
                        time-stamp-line-limit 18
                        time-stamp-start "^#\\+LAST_MODIFIED: [ \t]*"
                        time-stamp-end "$"
                        time-stamp-format "[%Y-%02m-%02dT%02H:%02M:%02S%5z]")
            (add-hook 'before-save-hook #'time-stamp nil 'local)))
