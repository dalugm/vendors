;;; auto-insert.el --- auto insert/update file header -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Insert/Update file header
;;

;;; Code:

;;; Auto Insert
(require 'autoinsert)
(add-hook 'prog-mode auto-insert-mode)

(with-eval-after-load auto-insert-mode

  (defun my--insert-string(&optional prefix)
    "Basic information."
    (replace-regexp-in-string
     "^" (or prefix comment-start)
     (concat
      (make-string 80 ?*) "\n"
      "Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
      "File Name: " (file-name-nondirectory buffer-file-name) "\n"
      "Author: " (user-full-name)"\n"
      "Email: " user-mail-address "\n"
      "Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
      "Last Update: \n"
      "         By: \n"
      "Description: \n"
      (make-string 80 ?*))))

  (setq auto-insert-query nil           ; do NOT annoy
        auto-insert-alist
        '(((ruby-mode . "Ruby program") nil
           "#!/usr/bin/env ruby\n"
           "# -*- encoding: utf-8 -*-\n"
           (my--insert-string) "\n")
          ((python-mode . "Python program") nil
           "#!/usr/bin/env python\n"
           "# -*- coding: utf-8 -*-\n"
           (my--insert-string) "\n")
          ((c-mode . "C program") nil
           "/*"
           (string-trim-left (my--insert-string " ")) "*/\n"
           "#include<stdio.h>\n"
           "#include<string.h>\n")
          ((sh-mode . "Shell script") nil
           "#!/bin/bash\n"
           (my--insert-string) "\n")
          ((go-mode . "Go program") nil
           "/*"
           (string-trim-left (my--insert-string " ")) "*/\n"))))

;;; Auto Update

;; auto update timestamp
(setq time-stamp-active t)
(setq time-stamp-line-limit 11)
(setq time-stamp-start "[lL]ast[ -][uU]pdate[ \t]*:?")
(setq time-stamp-end "\n")
(setq time-stamp-format " %#A %Y-%02m-%02d %02H:%02M:%02S (%Z)")

;; Auto update when saving buffer
(add-hook 'before-save-hook 'time-stamp)

;; auto update file name
(defun my-header-update-action(name)
  "A."
  (let ((beg (match-beginning 2))
        (end (match-end 2)))
    (when (not (string= name (string-trim-left (match-string 2))))
      (goto-char beg)
      (delete-region beg end)
      (insert " " name))))

(defun my-header-update(regex default line-limit)
  "B."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((lines 0))
      (while (&lt; lines line-limit)
              (when (and (looking-at regex))
                (my-header-update-action default))
              (setq lines (1+ lines))
              (forward-line 1))))))

(defmacro my-header-update-engine (name regex default &optional line-limit)
  "C."
  `(defun ,(intern (format "my-header-update-%s" name)) ()
     ,(format "Update %s with regex." name)
     (interactive)
     (my-header-update ,regex ,default ,(or line-limit 7))))

;; Update FileName automatically
;; 生成一个\\[my-header-update-filename]的函数,只要调用它即可更新文件名信息
(my-header-update-engine "filename"
                            ".*\\(File Name:\\)\\(.*\\)"
                            (file-name-nondirectory (buffer-file-name)) 7)

;; Update Email automatically
(my-header-update-engine "email"
                            ".*\\(Email:\\)\\(.*\\)"
                            "myemail@gmail.com" 7)

(provide 'auto-insert)

;;; auto-insert.el ends here
