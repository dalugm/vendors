;;; qsrc-mode.el --- qsrc edit mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 dalu

;; Filename: qsrc-mode.el
;; Author: dalu <mou.tong@qq.com>
;; Maintainer: dalu <mou.tong@qq.com>
;; Created: 2020-02-03 22:14
;; Version: 0.1
;; Last-Updated: 2020-02-17 14:37
;;           By: dalu <mou.tong@qq.com>
;; Keywords: language, qsrc

;; This file is NOT part of GNU Emacs.

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file is made to write qsrc language
;;

;;; Installation:
;;
;; To install it as your major mode for qsrc editing:
;;
;;   (add-to-list 'load-path "path/to/qsrc-mode")
;;   (require 'qsrc-mode)
;;   (add-to-list 'auto-mode-alist '("\\.qsrc\\'" . qsrc-mode))
;;

;;; Code:

;;; User definable variables

(require 'font-lock)

(defgroup qsrc-mode nil
  "Major mode for editing qsrc files."
  :group 'languages)

(defcustom qsrc-mode-hook nil
  "Normal hook run when entering qsrc-mode."
  :type 'hook
  :group 'qsrc-mode)

(defcustom qsrc-indent-offset 8
  "Amount of offset per level of indentation."
  :type 'integer
  :safe 'natnump
  :group 'qsrc-mode)

(defgroup qsrc-faces nil
  "Faces used for qsrc files."
  :group 'faces)

(defface qsrc-option
  '((default :inherit font-lock-variable-name-face))
  "Face used for qsp's configuration options.")

(defface qsrc-function
  '((default :inherit font-lock-function-name-face))
  "Face used for qsp's functions.")

(defface qsrc-command
  '((default :inherit font-lock-keyword-face))
  "Face used for qsp's Ex commands.")

(defface qsrc-number
  '((((class color) (background light)) (:foreground "steel blue"))
    (((class color) (background dark)) (:foreground "sky blue"))
    (t nil))
  "Face used for qsp's numbers.")

(defface qsrc-comment
  '((default :inherit font-lock-comment-face))
  "Face used for qsp's comments.")


;;; Constants

(defconst qsrc-mode-version "0.0.1"
  "Version of `qsrc-mode'.")

(defconst qsrc-blank-line-re "^ *$"
  "Regexp matching a line containing only (valid) whitespace.")


;;; Font-lock support

;; create the list for font-lock.
;; each category of keyword is given a particular face
(defvar qsrc-font-lock-keywords
  (let* (
          ;; define several category of keywords
          (x-keywords '("counter" "ongload" "ongsave" "onnewloc" "onactsel" "onobjsel" "onobjadd" "onobjdel" "usercom" "fname" "backimage" "args" "result"))
          (x-types '("args" "$args" "result" "$result" "disablescroll" "disablesubex" "nosave" "debug" "$counter" "$ongload" "$ongsave" "$onnewloc" "$onactsel" "$onobjsel" "$onobjadd" "$onobjdel" "$usercom" "usehtml" "bcolor" "fcolor" "lcolor" "fsize" "$fname" "$backimage"))
          (x-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
          (x-events '("at_rot_target" "at_target" "attach"))
          (x-functions '("act" "exit" "killvar" "cla" "nl" "clr" "gs" "xgt" "gt" "goto" "gosub" "xgoto" "if" "else" "elseif" "end" "jump" "dynamic" "input"))

          ;; generate regex string for each category of keywords
          (x-keywords-regexp (regexp-opt x-keywords 'words))
          (x-types-regexp (regexp-opt x-types 'words))
          (x-constants-regexp (regexp-opt x-constants 'words))
          (x-events-regexp (regexp-opt x-events 'words))
          (x-functions-regexp (regexp-opt x-functions 'words)))

    `(
       (,x-types-regexp . font-lock-type-face)
       (,x-constants-regexp . font-lock-constant-face)
       (,x-events-regexp . font-lock-builtin-face)
       (,x-functions-regexp . font-lock-function-name-face)
       (,x-keywords-regexp . font-lock-keyword-face)
       ;; note: order above matters, because once colored, that part won't change.
       ;; in general, put longer words first
       )))


;;; Mode setup

(defvar qsrc-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "C-c C-i") 'qsrc-if)
    ;; by convention, major mode's keys should begin with the form C-c C-‹key›
    ;; by convention, keys of the form C-c ‹letter› are reserved for user. don't define such keys in your major mode
    map)
  "Keymap for qsrc-mode.")

(defvar qsrc-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    syntax-table)
  "Syntax table for `qsrc-mode'.")


;;; Functions

(defun qsrc-if ()
  "Insert an if statement."
  (interactive)
  (message "[function] `qsrc-if' called."))

(defun qsrc-mode-version ()
  "Display version of `qsrc-mode'."
  (interactive)
  (message "qsrc-mode version: %s" qsrc-mode-version)
  qsrc-mode-version)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qsrc\\'" . qsrc-mode))

;;;###autoload
(define-derived-mode qsrc-mode js-mode "Qsrc"
  "Major mode to edit qsrc (QSP grammar) files."
  :group 'qsrc-mode
  ;; code for syntax highlighting
  (setq font-lock-defaults '((qsrc-font-lock-keywords)))
  (setq-local comment-start "! ")
  (setq-local comment-start-skip "!+ *")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  ;; actually no need because our keymap name is `qsrc-mode' follow by `-map'
  ;; so \\[define-derived-mode] will find it and set it for you automatically
  (use-local-map qsrc-mode-map)
  ;; actually no need, because our syntax table name is `qsrc-mode' + `-syntax-table'
  ;; so \\[define-derived-mode] will find it and set it automatically
  (set-syntax-table qsrc-mode-syntax-table)
  (run-hooks 'qsrc-mode-hook))

(provide 'qsrc-mode)

;;; qsrc-mode.el ends here
