;; -*- lexical-binding: t; -*-

(with-eval-after-load 'eglot
  (setf (alist-get '(elixir-mode elixir-ts-mode heex-ts-mode) eglot-server-programs
                   nil nil #'equal)
        (if (and (fboundp 'w32-shell-dos-semantics)
                 (w32-shell-dos-semantics))
            '("language_server.bat")
          (eglot-alternatives '("language_server.sh" "start_lexical.sh")))))

(if-let ((mac-p (eq system-type 'darwin))
         (version-lt30 (< emacs-major-version 30))
         (gls (executable-find "gls")))
    (setq insert-directory-program gls)
  ;; Suppres Dired warning when not using GNU ls.
  (setq dired-use-ls-dired nil))
