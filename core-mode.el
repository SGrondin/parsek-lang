;;; core-mode-el -- Major mode for editing Kernel/CORE files
;; derived from:
;; http://two-wugs.net/emacs/mode-tutorial.html

;;; Code:
(defvar core-mode-hook nil)
(defvar core-mode-map
  (let ((core-mode-map (make-keymap)))
    (define-key core-mode-map "\C-j" 'newline-and-indent)
    core-mode-map)
  "Keymap for Kernel/CORE major mode")

(add-to-list 'auto-mode-alist '("\\.k\\'" . core-mode))

;;‘font-lock-builtin-face’ and ‘font-lock-variable-name-face’,

(defconst core-font-lock-keywords-1
  (list
   '("\\(\\w*\\)[ \t\n]*\\(=\\|<is>\\)[ \n\t\{\[\(]" 
     (1 font-lock-function-name-face))

   '("\\(<type>\\|<code>\\)[ \t\n]*\\(\\w*\\)[ \t\n]*<is>" 
     (2 font-lock-function-name-face)
    )

   '("\\([$@][ \t\n]*\\w*\\)" . font-lock-variable-name-face)
   '("\\(['.][ \t\n]*\\w*\\)" . font-lock-constant-face)
   '("\\([.][.][.]\\|<by>\\|<type>\\|<code>\\|<is>\\)" . font-lock-keyword-face)
   '("\\(::\\|[][{}()=,;|]\\|->\\)" . font-lock-keyword-face)

    '("\\<\\(CONCAT\\(?:ENATE\\)?\\|FIRST\\(?:_MATCH\\)?\\|INTER\\(?:SECT\\)?\\|LONGEST\\(?:_MATCH\\)?\\|SHORTEST\\(?:_MATCH\\)?\\)\\>" . font-lock-builtin-face)

   )
  "Highlighting expressions for Kernel/CORE mode.")

(defvar core-font-lock-keywords core-font-lock-keywords-1
  "Default highlighting expressions for Kernel/CORE mode.")


(defvar core-mode-syntax-table
  (let ((core-mode-syntax-table (make-syntax-table)))
	
    ; This is added so entity names with underscores can be more easily parsed
	(modify-syntax-entry ?0 "w" core-mode-syntax-table)
	(modify-syntax-entry ?1 "w" core-mode-syntax-table)
	(modify-syntax-entry ?2 "w" core-mode-syntax-table)
	(modify-syntax-entry ?3 "w" core-mode-syntax-table)
	(modify-syntax-entry ?4 "w" core-mode-syntax-table)
	(modify-syntax-entry ?5 "w" core-mode-syntax-table)
	(modify-syntax-entry ?6 "w" core-mode-syntax-table)
	(modify-syntax-entry ?7 "w" core-mode-syntax-table)
	(modify-syntax-entry ?8 "w" core-mode-syntax-table)
	(modify-syntax-entry ?9 "w" core-mode-syntax-table)
	(modify-syntax-entry ?_ "w" core-mode-syntax-table)
	(modify-syntax-entry ?+ "w" core-mode-syntax-table)
	(modify-syntax-entry ?= "w" core-mode-syntax-table)
	(modify-syntax-entry ?! "w" core-mode-syntax-table)
	(modify-syntax-entry ?< "w" core-mode-syntax-table)
	(modify-syntax-entry ?> "w" core-mode-syntax-table)
	(modify-syntax-entry ?? "w" core-mode-syntax-table)
	
	; Comment styles are same as C++
	(modify-syntax-entry ?/ "w 124b" core-mode-syntax-table)
	(modify-syntax-entry ?- "w 124b" core-mode-syntax-table)
	(modify-syntax-entry ?# "<    b" core-mode-syntax-table)
	(modify-syntax-entry ?% "<    b" core-mode-syntax-table)
	(modify-syntax-entry ?* "w   23" core-mode-syntax-table)
	(modify-syntax-entry ?\n ">    b" core-mode-syntax-table)
	core-mode-syntax-table)
  "Syntax table for core-mode")
  
(defun core-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map core-mode-map)
  (set-syntax-table core-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(core-font-lock-keywords))
  ;; Register our indentation function
;  (set (make-local-variable 'indent-line-function) 'core-indent-line)  
  (setq major-mode 'core-mode)
  (setq mode-name "Kernel/CORE")
  (run-hooks 'core-mode-hook))

(provide 'core-mode)

  ;; (defun core-indent-line ()
  ;; "Indent current line as Kernel/CORE code."
  ;; (interactive)
  ;; (beginning-of-line)
  ;; (if (bobp)
  ;;     (indent-line-to 0)		   ; First line is always non-indented
  ;;   (let ((not-indented t) cur-indent)
  ;;     (if (looking-at "^.*;") ; End of statement
  ;; 	  (progn
  ;; 	    (save-excursion
  ;; 	      (forward-line -1)
  ;; 	      (setq cur-indent (- (current-indentation) default-tab-width)))
  ;; 	    (if (< cur-indent 0) ; We can't indent past the left margin
  ;; 		(setq cur-indent 0)))
  ;; 	(save-excursion
  ;; 	  (while not-indented ; Iterate backwards until we find an indentation hint
  ;; 	    (forward-line -1)
  ;; 	    (if (looking-at "^.*;") 
  ;; 		;; This hint indicates that we need to indent at the
  ;; 		;; level of the END_ token
  ;; 		(progn
  ;; 		  (setq cur-indent (current-indentation))
  ;; 		  (setq not-indented nil))
  ;; 	      (if (looking-at "^.*\\([(=[{]\\)") 
  ;; 		  ;; This hint indicates that we need to indent an
  ;; 		  ;; extra level
  ;; 		  (progn
  ;; 		    (setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
  ;; 		    (setq not-indented nil))
  ;; 		(if (bobp)
  ;; 		    (setq not-indented nil)))))))
  ;;     (if cur-indent
  ;; 	  (indent-line-to cur-indent)
  ;; 	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

;;; core-mode.el ends here

