;;; Complete symbols at point using Pymacs.
;;; See pycomplete.py for the Python side of things and a short description
;;; of what to expect.

(require 'pymacs)
(require 'python)


(pymacs-load "pycomplete")


(defconst py-identifier 
  "[A-Za-z_][A-Za-z_0-9]*"
  "Regular expression matching a python identifier.")

(defvar py-mode-syntax-table nil
  "Syntax table used in `python-mode' buffers.")
(when (not py-mode-syntax-table)
  (setq py-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" py-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" py-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" py-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" py-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" py-mode-syntax-table)
  (modify-syntax-entry ?\} "){" py-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  py-mode-syntax-table)
  (modify-syntax-entry ?\% "."  py-mode-syntax-table)
  (modify-syntax-entry ?\& "."  py-mode-syntax-table)
  (modify-syntax-entry ?\* "."  py-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  py-mode-syntax-table)
  (modify-syntax-entry ?\- "."  py-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  py-mode-syntax-table)
  (modify-syntax-entry ?\< "."  py-mode-syntax-table)
  (modify-syntax-entry ?\= "."  py-mode-syntax-table)
  (modify-syntax-entry ?\> "."  py-mode-syntax-table)
  (modify-syntax-entry ?\| "."  py-mode-syntax-table)
  ;; For historical reasons, underscore is word class instead of
  ;; symbol class.  GNU conventions say it should be symbol class, but
  ;; there's a natural conflict between what major mode authors want
  ;; and what users expect from `forward-word' and `backward-word'.
  ;; Guido and I have hashed this out and have decided to keep
  ;; underscore in word class.  If you're tempted to change it, try
  ;; binding M-f and M-b to py-forward-into-nomenclature and
  ;; py-backward-into-nomenclature instead.  This doesn't help in all
  ;; situations where you'd want the different behavior
  ;; (e.g. backward-kill-word).
  (modify-syntax-entry ?\_ "w"  py-mode-syntax-table)
  ;; Both single quote and double quote are string delimiters
  (modify-syntax-entry ?\' "\"" py-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" py-mode-syntax-table)
  ;; backquote is open and close paren
  (modify-syntax-entry ?\` "$"  py-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?\# "<"  py-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  py-mode-syntax-table)
  )

;; An auxiliary syntax table which places underscore and dot in the
;; symbol class for simplicity
(defvar py-dotted-expression-syntax-table nil
  "Syntax table used to identify Python dotted expressions.")
(when (not py-dotted-expression-syntax-table)
  (setq py-dotted-expression-syntax-table
	(copy-syntax-table py-mode-syntax-table))
  (modify-syntax-entry ?_ "_" py-dotted-expression-syntax-table)
  (modify-syntax-entry ?. "_" py-dotted-expression-syntax-table))

(defun py-symbol-near-point ()
  "Return the first textual item to the nearest point."
  ;; alg stolen from etag.el
  (save-excursion
    (with-syntax-table py-dotted-expression-syntax-table
      (if (or (bobp) (not (memq (char-syntax (char-before)) '(?w ?_))))
	  (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
	    (forward-char 1)))
      (while (looking-at "\\sw\\|\\s_")
	(forward-char 1))
      (if (re-search-backward "\\sw\\|\\s_" nil t)
	  (progn (forward-char 1)
		 (buffer-substring (point)
				   (progn (forward-sexp -1)
					  (while (looking-at "\\s'")
					    (forward-char 1))
					  (point))))
	nil))))



(defconst py-blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "Regular expression matching a blank or comment line.")

;;; regular expressions regarding import statetment
;;; based on Python Grammar

(defconst py-dotted-name-re 
  (concat py-identifier "\\([.]" py-identifier "\\)*")
  "Regular expression matching a dotted_name production.")

(defconst py-dotted-as-name-re 
  (concat py-dotted-name-re "\\(\\s +as\\s +" py-identifier "\\)*")
  "Regular expression matching a dotted_as_name production.")

(defconst py-dotted-as-names-re 
  (concat py-dotted-as-name-re 
          "\\(\\s *,\\s *"  py-dotted-as-name-re "\\)*")
  "Regular expression matching a dotted_as_names production.")

(defconst py-import-as-name-re 
  (concat py-identifier "\\(\\s +as\\s +" py-identifier "\\)*" )
  "Regular expression matching a import_as_name production.")

(defconst py-import-as-names-re 
  (concat py-import-as-name-re "\\(\\s *,\\s *" py-import-as-name-re "\\)*" 
          "\\s *[,]?" )
  "Regular expression matching a import_as_names production.")

(defconst py-import-name-re 
  (concat "^\\s *\\<import\\>\\s +" py-dotted-as-names-re)
  "Regular expression matching a import_name production.")

(defconst py-import-from-re 
  (concat "^\\s *\\<from\\>\\s +" "\\([.]*" py-dotted-name-re "\\|[.]+\\)\\s +"
          "\\<import\\>\\s +" "\\([*]\\|(\\s *" py-import-as-names-re "[^)]*)"
          "\\|" py-import-as-names-re "\\)")
  "Regular expression matching a import_from production.")

(defconst py-imports-re
  (concat "\\(" 
          (mapconcat 'identity
                     (list py-import-name-re 
                           py-import-from-re)
                     "\\|")
          "\\)")
  "Regular expression matching imports.")


(defun blank-linep ()
  "check if current line is empty (only whitespaces and comments)"
  (save-excursion
    (beginning-of-line)
    (looking-at py-blank-or-comment-re)))


(defun char-before-blank ()
  "check if prev character is blank-type"
  (save-excursion
    (forward-char -1)
    (looking-at "[\n\t\r]")))


(defun py-complete ()
  "show possible completions for current statement"
  (interactive)
  (let ((pymacs-forget-mutability t))
    (if (and (eolp) (not (bolp)) 
             (not (char-before-blank))
             (not (blank-linep)))
        (insert (pycomplete-pycomplete 
                 (py-symbol-near-point)
                 (buffer-file-name)
                 (py-find-global-imports)))
      (indent-for-tab-command))))

(defconst py-stringlit-re
  (concat
   ;; These fail if backslash-quote ends the string (not worth
   ;; fixing?).  They precede the short versions so that the first two
   ;; quotes don't look like an empty short string.
   ;;
   ;; (maybe raw), long single quoted triple quoted strings (SQTQ),
   ;; with potential embedded single quotes
   "[rR]?'''[^']*\\(\\('[^']\\|''[^']\\)[^']*\\)*'''"
   "\\|"
   ;; (maybe raw), long double quoted triple quoted strings (DQTQ),
   ;; with potential embedded double quotes
   "[rR]?\"\"\"[^\"]*\\(\\(\"[^\"]\\|\"\"[^\"]\\)[^\"]*\\)*\"\"\""
   "\\|"
   "[rR]?'\\([^'\n\\]\\|\\\\.\\)*'"	; single-quoted
   "\\|"				; or
   "[rR]?\"\\([^\"\n\\]\\|\\\\.\\)*\""	; double-quoted
   )
  "Regular expression matching a Python string literal.")

(defconst py-continued-re
  ;; This is tricky because a trailing backslash does not mean
  ;; continuation if it's in a comment
  (concat
   "\\(" "[^#'\"\n\\]" "\\|" py-stringlit-re "\\)*"
   "\\\\$")
  "Regular expression matching Python backslash continuation lines.")

(defun py-backslash-continuation-line-p ()
  "Return t iff preceding line ends with backslash that is not in a comment."
  (save-excursion
    (beginning-of-line)
    (and
     ;; use a cheap test first to avoid the regexp if possible
     ;; use 'eq' because char-after may return nil
     (eq (char-after (- (point) 2)) ?\\ )
     ;; make sure; since eq test passed, there is a preceding line
     (forward-line -1)			; always true -- side effect
     (looking-at py-continued-re))))

(defun py-find-global-imports ()
  "find global import statements"
  (save-excursion
    (let ((imports nil))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (if (looking-at py-imports-re)
            ;; import statement found
            (progn
              (setq imports 
                    (append imports (list (buffer-substring
                                           (match-beginning 0) 
                                           (match-end 0)))))
              (forward-line 1)
              ;; handle continuation backslashes
              (while (and (py-backslash-continuation-line-p) (not (eobp)))
                (goto-char (line-beginning-position))
                (skip-chars-forward " \t")
                (setq begin (point))
                (goto-char (line-end-position))
                (skip-chars-backward " \t\\")
                (if (= (char-before) ?\\)
                    (setq end (- (point) 1))
                  (setq end (point)))
                (setcar (last imports)
                        (concat (car (last imports)) " " 
                                (buffer-substring begin end)))
                (forward-line 1)))
          (forward-line)))
      imports)))


(defun py-complete-python-dotexpr-begin nil
  (re-search-backward "[^a-zA-Z_0-9\\.]")
  (forward-char))

(defun py-complete-python-dotexpr-end nil
  (re-search-forward "[a-zA-Z_0-9\\.]*"))

(put 'python-dotexpr 'beginning-op 'py-complete-python-dotexpr-begin)
(put 'python-dotexpr 'end-op 'py-complete-python-dotexpr-end)


(defun py-complete-show (string)
  (display-message-or-buffer string "*PythonHelp*"))


(defun py-complete-help (string)
  "get help on a python expression"
  (interactive "sHelp: ")
  (let ((help-string 
         (pycomplete-pyhelp string (py-find-global-imports))))
    (if (and help-string (> (length help-string) 300))
        (with-output-to-temp-buffer "*Python Help*"
          (print help-string))
      (py-complete-show help-string))))


(defun py-complete-help-thing-at-point nil
  (interactive)
  (require 'thingatpt)
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (py-complete-help sym))))


(set 'py-complete-current-signature nil)

(defun py-complete-signature (function)
  "get signature of a python function or method"
  (set 'py-complete-current-signature
       (pycomplete-pysignature function)))


(defun py-complete-signature-show nil
  (require 'thingatpt) 
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (progn 
          (py-complete-show (py-complete-signature sym))))))


(defun py-complete-signature-expr nil
  (interactive)
  (require 'thingatpt)
  (let ((dotexpr (read-string "signature on: "
                              (thing-at-point 'python-dotexpr))))
    (if dotexpr
        (py-complete-show
         (py-complete-signature dotexpr)))))


(defun py-complete-electric-lparen nil
  "electricly insert '(', and try to get a signature for the stuff to the left"
  (interactive)
  (py-complete-signature-show)
  (self-insert-command 1))


(defun py-complete-electric-comma nil
  "electricly insert ',', and redisplay latest signature"
  (interactive)
  (self-insert-command 1)
  (if py-complete-current-signature
      (py-complete-show (format "%s" py-complete-current-signature))))


(define-key python-mode-map "\M-\C-i" 'py-complete)
(define-key python-mode-map "\t" 'py-complete)
(define-key python-mode-map [f1] 'py-complete-help-thing-at-point)
(define-key python-mode-map "(" 'py-complete-electric-lparen)
(define-key python-mode-map "," 'py-complete-electric-comma)
(define-key python-mode-map [f2] 'py-complete-signature-expr)
(define-key python-mode-map [f3] 'py-complete-help)

(provide 'pycomplete)
