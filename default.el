
;; ****** Added by emacsw32-setup-base at Thu May 01 10:45:17 2008
;; Load emacsw32 if found.
(progn
  (require 'emacsw32 nil t)
  (unless (featurep 'emacsw32)
    (lwarn '(emacsw32) :error "Could not find emacsw32.el")))

; Font and Appearance
(set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
(setq-default truncate-partial-width-windows nil)

; Broken at the moment, fix
;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		       'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                          temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "epylint" (list local-file))))
    
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pylint-init)))

; Company mode
(require 'company-mode)
(require 'company-bundled-completions)
(company-install-bundled-completions-rules)

; Colour!
(require 'color-theme)

; Turn off shite
(tool-bar-mode -1) 
(menu-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-showhide-fringe-menu-customize-disable)
(blink-cursor-mode -1)

; hide-lines
(autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
(global-set-key "\C-ch" 'hide-lines)

; Org-Mode		
(require 'org-export-latex)
;; (setq org-log-done '(done))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

; Fix grep-find
(setq grep-find-command "find . -exec grep -nH -e '?' {} ; -print")

; Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

; Ropemacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")


; Windows
(require 'martin-darkroom)

; General
(global-font-lock-mode 1)                     ; for all buffers
(require 'pair-mode)
(winner-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)