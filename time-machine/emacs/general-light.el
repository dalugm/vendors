;;; general-light.el --- a lighter version of general -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  A lighter version of general.el
;;  @Inspired by https://emacs-china.org/t/general-el/13862
;;

;;; Code:

(require 'pcase)

(defvar my--key-preset-alist nil
  "Stores defined presets.")

(defvar my--key-override-mode-map (make-sparse-keymap)
  "One map to rule them all.")

(defvar my--key-override-map-alist
  `(my--key-override-mode . ,my--key-override-mode-map)
  "Alist put into `emulation-mode-map-alists' to override all other maps.")

(defvar my--key-postponed-alist nil
  "An alist of (map . ((key def) (key def))).
When we define a key in a map that’s not defined yet, we push the
definition to this alist.  When a new file is loaded, we check for
each map and see if we can now define the bindings for that map.")

(defun my//key-define-postponed-binding (_)
  "Define postponed bindings in ‘my--key-postponed-alist’."
  (dolist (map (mapcar #'car my--key-postponed-alist))
    (when (boundp map)
      (pcase-dolist (`(,key ,def)
                      (alist-get map my--key-postponed-alist))
        (define-key (symbol-value map) key def))
      (setf (alist-get map my--key-postponed-alist) nil))))

(define-minor-mode my--key-override-mode
  "Minor mode used to activate our override keymap."
  :global t
  :lighter ""
  :keymap 'my--key-override-mode-map)

(defun my//key-def-preset (preset &rest args)
  "Define PRESET as ARGS.

ARGS can be anything valid in `my//def-key'.

If you define :leader as

    (my//key-def-preset :leader
     :keymaps 'override
     :prefix \"C-SPC\")

Then

    (my//def-key
     :leader
     KEY DEF)

is equivalent to

    (my//def-key
     :keymaps 'override
     :prefix \"C-SPC\"
     KEY DEF)"
  (declare (indent 1))
  (setf (alist-get preset my--key-preset-alist) args))

(defun my//key-normalize (prefix key)
  "Normalize KEY and PREFIX with `kbd' and combine them.
However, if KEY is [remap ...] or [t], don’t prepend PREFIX to it."
  ;; Normalize KEY and PREFIX with `kbd'.
  (if (stringp key) (setq key (kbd key)))
  (if (and prefix (stringp prefix)) (setq prefix (kbd prefix)))
  ;; Result of ‘kbd’ can be either string or vector,
  ;; now we normalize KEY and PREFIX to vectors.
  (if (stringp key) (setq key (string-to-vector key)))
  (if (and prefix (stringp prefix))
    (setq prefix (string-to-vector prefix)))
  ;; When do we simply return KEY without PREFIX: either PREFIX is
  ;; nil, or KEY has special meaning ([remap ...] or [t]).
  (if (or (not prefix) (and (vectorp key) (memq (aref key 0) '(t remap))))
    key
    (vconcat prefix key)))

(defun my//key-define (key def map-list prefix)
  "Define KEY to DEF.
Define KEY in all the maps in MAP-LIST, using PREFIX as prefix.
MAP-LIST and PREFIX can be nil.
If MAP-LIST is nil, only define in `global-map'."
  (let ((map-list (or map-list (list 'global-map)))
         (key (my//key-normalize prefix key))
         ;; If DEF is (STRING . DEFN), we use STRING as it’s description.
         (desc (car-safe def)))
    (when desc
      (with-eval-after-load 'which-key
        (which-key-add-key-based-replacements
          (key-description key) desc)))
    (dolist (map map-list)
      (let ((map (if (eq map 'override) 'my--key-override-mode-map map)))
        (if (boundp map)
          (define-key (symbol-value map) key def)
          (push (list key def)
            (alist-get map my--key-postponed-alist))
          (add-hook 'after-load-functions
            #'my//key-define-postponed-binding))))))

(defun my//def-key (&rest args)
  "Define key.

The :keymaps and :prefix modifiers specifies the keymaps and
prefix key for KEY DEF pairs below them.  Modifiers only affect
the definitions after them, and their effect lasts until another
modifier overrides them.  For example, to define KEY1 in MAP1 and
KEY2 in MAP2:

  (my//def-key
   :keymaps 'MAP1
   KEY1 DEF1
   :keymaps 'MAP2n
   KEY2 DEF2)

Unlike in `define-key', MAP is the symbol of a keymap, rather than
the keymap itself.  MAP can also be nil, which is interpreted as
`global-map', or 'override, which is interpreted as the override
keymap defined by my-key, or a list of these three forms.  KEY
and DEF can be anything that `define-key' accepts.  `kbd' is
automatically added to KEY but not DEF.

You can also use preset modifiers defined by `my//key-def-preset'.
They are basically macros that expand to other modifiers.

Use :clear to reset all modifier effects.  :--- is an alias for :clear.

ARGS.

\(fn [:keymaps MAPS] [:prefix PREFIX] [:clear] KEY DEF ...)"
  (my--key-override-mode)
  (condition-case nil
    (let (arg map-list prefix)
      (while args
        (setq arg (pop args))
        (pcase arg
          (:keymaps
            (let ((map (pop args)))
              (cond ((symbolp map) (setq map-list (list map)))
                ((proper-list-p map) (setq map-list map))
                (t (error "Invalid argument for :keymaps command: %s"
                     map)))))
          (:prefix
            (setq prefix (pop args)))
          ((or :clear :---) (setq prefix nil
                              map-list nil))
          ((pred keywordp)
            (when-let ((preset (alist-get arg my--key-preset-alist)))
              (setq args (append preset args))))
          (_ (let ((key arg)
                    (def (pop args)))
               (my//key-define key def map-list prefix))))))
    (setting-constant (error "Not enough arguments"))))

(provide 'general-light)

;;; general-light.el ends here
