(defun my--show-current-evil-state ()
  "Change modeline's face according to different evil state."
  (let ((color (cond
                ((evil-emacs-state-p)   '("#7e1671" . "#f8f4ed"))
                ((evil-insert-state-p)  '("#20894d" . "#f8f4ed"))
                ((evil-visual-state-p)  '("#ffd111" . "#f8f4ed"))
                ((evil-replace-state-p) '("#de1c31" . "#f8f4ed"))
                ((buffer-modified-p)    '("#1772b4" . "#f8f4ed"))
                (t                      my-default-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(define-minor-mode my-modeline-evil-indicator-mode
  "Show current evil state by changing modeline's face."
  :global t
  :lighter ""
  (defconst my-default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))
    "Default modeline color.")
  (if my-modeline-evil-indicator-mode
      (add-hook 'post-command-hook #'my--show-current-evil-state)
    (remove-hook 'post-command-hook #'my--show-current-evil-state)))
