;; https://emacs-china.org/t/org-mode/22313
;; use `font-lock' to hide spaces
(font-lock-add-keywords
 'org-mode
 '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
   ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
    (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
 'append)

;; https://github.com/Elilif/.elemacs/blob/master/lisp/init-org.el#L90
;; use emphasis markers in Chinese lines, which do NOT need spaces
(setq org-emphasis-regexp-components
      '("-[:space:]('\"{[:nonascii:][:alpha:]"
        "-[:space:].,:!?;'\")}\\[[:nonascii:][:alpha:]"
        "[:space:]"
        "."
        1))
(org-set-emph-re 'org-emphasis-regexp-components
                 org-emphasis-regexp-components)
(org-element-update-syntax)

(setq org-match-substring-regexp
      (concat
       ;; Limit the matching range of superscript and subscript
       ;; https://orgmode.org/manual/Subscripts-and-Superscripts.html
       "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
       "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
       "\\|"
       "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
       "\\|"
       "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)"))

;; HACK: after we use the above changes
(defun my--org-do-emphasis-faces (limit)
  "Run through the buffer and emphasize Chinese strings."
  (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\).*?[~=*/_+]"
    		          (car org-emphasis-regexp-components))))
    (catch :exit
      (while (re-search-forward quick-re limit t)
        (let* ((marker (match-string 2))
               (verbatimp (member marker '("~" "="))))
          (when (save-excursion
    	          (goto-char (match-beginning 0))
    	          (and
    	           ;; Do not match table hlines.
    	           (not (and (equal marker "+")
    		             (org-match-line
    		              "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
    	           ;; Do not match headline stars.  Do not consider
    	           ;; stars of a headline as closing marker for bold
    	           ;; markup either.
    	           (not (and (equal marker "*")
    		             (save-excursion
    		               (forward-char)
    		               (skip-chars-backward "*")
    		               (looking-at-p org-outline-regexp-bol))))
    	           ;; Match full emphasis markup regexp.
    	           (looking-at (if verbatimp org-verbatim-re org-emph-re))
    	           ;; Do not span over paragraph boundaries.
    	           (not (string-match-p org-element-paragraph-separate
    				        (match-string 2)))
    	           ;; Do not span over cells in table rows.
    	           (not (and (save-match-data (org-match-line "[ \t]*|"))
    		             (string-match-p "|" (match-string 4))))))
            (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
    		        (m (if org-hide-emphasis-markers 4 2)))
              (font-lock-prepend-text-property
               (match-beginning m) (match-end m) 'face face)
              (when verbatimp
    	        (org-remove-flyspell-overlays-in
    	         (match-beginning 0) (match-end 0))
    	        (remove-text-properties (match-beginning 2) (match-end 2)
    				        '(display t invisible t intangible t)))
              (add-text-properties (match-beginning 2) (match-end 2)
    			           '(font-lock-multiline t org-emphasis t))
              (when (and org-hide-emphasis-markers
    		         (not (org-at-comment-p)))
    	        (add-text-properties (match-end 4) (match-beginning 5)
    			             '(invisible t))
    	        (add-text-properties (match-beginning 3) (match-end 3)
    			             '(invisible t)))
              (throw :exit t))))))))

(advice-add 'org-do-emphasis-faces :override #'my--org-do-emphasis-faces)

(defun my--strip-ws-maybe (text _backend _info)
  "Remove extra spaces when export."
  (let* (;; remove whitespace from line break
         (text (replace-regexp-in-string
                "\\(\\cc\\) *\n *\\(\\cc\\)"
                "\\1\\2"
                text))
         ;; remove whitespace from `org-emphasis-alist'
         (text (replace-regexp-in-string
                "\\(\\cc?\\) \\(.*?\\) \\(\\cc\\)"
                "\\1\\2\\3"
                text))
         ;; restore whitespace between English words and Chinese words
         (text (replace-regexp-in-string
                "\\(\\cc\\)\\(\\(?:<[^>]+>\\)?[a-z0-9A-Z-]+\\(?:<[^>]+>\\)?\\)\\(\\cc\\)"
                "\\1 \\2 \\3"
                text))
         (text (replace-regexp-in-string
                "\\(\\cc\\) ?\\(\\\\[^{}()]*?\\)\\(\\cc\\)"
                "\\1 \\2 \\3"
                text)))
    text))

(add-to-list 'org-export-filter-paragraph-functions #'my--strip-ws-maybe)

