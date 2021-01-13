# qsrc-mode

Major mode for editing qsrc files.

## INSTALL

Add the following code to your config files.

``` emacs-lisp
(setq load-path (cons "path/to/qsrc-mode" load-path))
(require 'qsrc-mode)
(add-to-list 'auto-mode-alist '("\\.qsrc\\'" . qsrc-mode))
```

If you're using `use-package`:

``` emacs-lisp
(use-package qsrc-mode
  :load-path "path/to/qsrc-mode"
  :mode ("\\.qsrc\\'" . qsrc-mode))
```

