;;; use-package-light.el --- lighter version of use-package -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  A lighter version of `use-package'
;;  @Inspired by https://emacs-china.org/t/use-package/13831
;;

;;; Code:

(require 'pcase)

(defun my//installed-p (package)
  "Return t if PACKAGE is installed."
  (and (locate-file (symbol-name package) load-path
         '(".el" ".el.gz" ".so" ".so.gz"))
    t))

(defun my//split-command-args (args)
  "Split args into commands and args.
If ARGS is (:command args args args :command args),
return: ((:command . (args args args)) (:command . (args)))."
  (let (ret-list arg-list command)
    (dolist (token (append args '(:finish)))
      (if (keywordp token)
        ;; Finish previous command
        (progn (if command (push (cons command (reverse arg-list))
                             ret-list))
          (setq arg-list nil)
          ;; Start new command
          (setq command token))
        (push token arg-list)))
    (reverse ret-list)))

(defun my//load-package--handle-hook (arg-list)
  "Handle hook arguments.
Each ARG in ARG-LIST is a cons (HOOK . FUNCTION).
HOOK can be either a single hook or a list of hooks.
FUNCTION can also be either a single function or a list of them."
  (let (ret-list hook-list func-list)
    (dolist (arg arg-list)
      (let ((hook (car arg))
             (func (cdr arg)))
        ;; Normalize to lists.
        (setq hook-list
          (if (symbolp hook) (list hook) hook))
        (setq func-list
          (if (symbolp func) (list func) func)))
      ;; Produce add-hook forms.
      (dolist (func func-list)
        (dolist (hook hook-list)
          (push `(add-hook ',hook #',func) ret-list))))
    (reverse ret-list)))

(defun my//load-package--collect-autoload (arg-list package)
  "Collect functions that needs autoload from ARG-LIST.
PACKAGE is the package we are loading.
Return a list of (autoload ...) forms."
  (let ((autoload
          (mapcan (lambda (arg)
                    (let ((command (car arg))
                           (arg-list (cdr arg)))
                      (pcase command
                        ;; ARG is either (hook . fn) or
                        ;;               ((hook ...) . fn) or
                        ;;               (hook . (fn ...))
                        (:hook (mapcan (lambda (arg)
                                         (let ((fn (cdr arg)))
                                           (if (symbolp fn)
                                             (list fn)
                                             fn)))
                                 arg-list))
                        ;; ARG is either ".xxx" or (".xxx" . mode)
                        (:mode (mapcar (lambda (arg)
                                         (if (stringp arg)
                                           package
                                           (cdr arg)))
                                 arg-list)))))
            arg-list)))
    (mapcar (lambda (fn)
              `(autoload #',fn ,(symbol-name package) nil t))
      autoload)))

(defmacro my|load-package (package &rest args)
  "Like ‘use-package’ but a lighter version.

PACKAGE is the package you are loading.
ARGS contains commands and arguments.
Available commands:

  :init         Run right away.
  :config       Run after package loads.
  :hook         Each arguments is (HOOK . FUNC)
                HOOK and FUNC can be a symbol or a list of symbols.
  :load-path    Add load paths.
  :mode         Add (ARG . PACKAGE) to ‘auto-mode-alist’.  If ARG is
                already a cons, add ARG to ‘auto-mode-alist’.
  :commands     Add autoload for this command.
  :when         Require when match condition
  :after        Require after this package loads.
  :defer        Don’t require the package.

Each command can take zero or more arguments."
  (declare (indent 1))
  ;; Group commands and arguments together.
  (let* ((arg-list (my//split-command-args args))
          ;; Translate commands & arguments to valid
          ;; config code.
          (body
            (mapcan
              (lambda (arg)
                (let ((command (car arg))
                       (arg-list (cdr arg)))
                  (pcase command
                    (:init arg-list)
                    (:config `((with-eval-after-load ',package
                                 ,@arg-list)))
                    (:hook (my//load-package--handle-hook arg-list))
                    (:mode
                      ;; ARG is either ".xxx" or (".xxx" . mode)
                      (mapcar
                        (lambda (arg)
                          (let ((pattern (if (consp arg) (car arg) arg))
                                 (mode-fn (if (consp arg) (cdr arg) package)))
                            `(add-to-list 'auto-mode-alist
                               ',(cons pattern mode-fn))))
                        arg-list))
                    (:when
                      (mapcar (lambda (pkg)
                                `(when ',pkg
                                   (require ',package)))
                        arg-list))
                    (:commands
                      (mapcar (lambda (cmd)
                                `(autoload ',cmd ,(symbol-name package) nil t))
                        arg-list))
                    (:after
                      (mapcar (lambda (pkg)
                                `(with-eval-after-load ',pkg
                                   (require ',package)))
                        arg-list)))))
              arg-list))
          (load-path-form (mapcar (lambda (path)
                                    `(add-to-list 'load-path ,path))
                            (alist-get :load-path arg-list)))
          (autoload-list (my//load-package--collect-autoload arg-list
                           package))
          ;; In which case we don’t require the package.
          (defer-p (let ((commands (mapcar #'car arg-list)))
                     (or (memq :defer commands)
                       (memq :commands commands)
                       (memq :after commands)
                       (memq :mode commands)
                       (memq :hook commands)))))
    `(condition-case err
       (progn
         ,@load-path-form
         ;; my--package-list跟配置有关
         (add-to-list 'my--package-list ',package)
         (when (not (my//installed-p ',package))
           (error "%s not installed" ',package))
         ,@autoload-list
         ,@body
         ,(unless defer-p `(require ',package)))
       ((debug error) (warn "Error when loading %s: %s" ',package
                        (error-message-string err))))))

(provide 'use-package-light)

;;; use-package-light.el ends here
