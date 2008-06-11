;; ****** Added by emacsw32-setup-base at Thu May 01 10:45:12 2008
;; Add EmacsW32/lisp to load-path if found.
(let ((lisp-dir (expand-file-name (concat exec-directory "../../EmacsW32/lisp/"))))
  (unless (file-accessible-directory-p lisp-dir)
    (message "Can't find %s" lisp-dir)
    (sit-for 10))
  (when (file-accessible-directory-p lisp-dir)
    (message "Adding %s to load-path" lisp-dir)
    (add-to-list 'load-path lisp-dir)))


;; ****** Added by emacsw32-setup-base at Wed Jun 04 16:23:03 2008
;; Add EmacsW32/lisp to load-path if found.
(let ((lisp-dir (expand-file-name (concat exec-directory "../../EmacsW32/lisp/"))))
  (unless (file-accessible-directory-p lisp-dir)
    (lwarn "Can't find %s" lisp-dir)
    (sit-for 10))
  (when (file-accessible-directory-p lisp-dir)
    (message "Adding %s to load-path" lisp-dir)
    (add-to-list 'load-path lisp-dir))
  (require 'emacsw32 nil t)
  (unless (featurep 'emacsw32)
    (lwarn '(emacsw32) :error "Could not find emacsw32.el")))
