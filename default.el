(server-start)

;load-path
(setq load-path (cons "C:/Program Files/Emacs/site-lisp/w3m" load-path))

;; Load emacsw32 if found.
(progn
  (require 'emacsw32 nil t)
  (unless (featurep 'emacsw32)
    (lwarn '(emacsw32) :error "Could not find emacsw32.el")))

(defvar rsys "d:/p4/depot/Development/7.0/ReleaseSystem_Stabilization/Build/ReleaseSystem")

(defvar todo "\\\\gin\\common\\forniall\\todo.org")

(defun insert-pwd ()
  (interactive)
  (insert (replace-regexp-in-string "Directory " "" (pwd))))

(defun python-insert-pdb-settrace ()
  (interactive)
  (insert "pdb.set_trace()"))

(global-set-key "\M-p" 'python-insert-pdb-settrace)

(defun dotemacs ()
  (interactive)
  (find-file "c:/Program Files/Emacs/site-lisp/default.el"))

(defun find-rsys (token)
  (interactive "SSearch RSYS for:")
  (let ((regex ".*\\.\\(py\\|txt\\|build\\)")
	(exe "C:\\cygwin\\bin\\find"))
    (grep-find
     (format "%s %S -regex %S -exec grep -nH -e '%s' {} ; -print" exe rsys regex token))))

(defun find-file-rsys (pattern)
  (interactive "SSearch RSYS for File Named:")
  (let ((exe "C:\\cygwin\\bin\\find"))
    (grep-find
      (format "%s %S -name \"*%S*.py\" -print " exe rsys pattern))))

(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun reload-dotemacs ()
  (interactive)
  (load-library "default"))

(global-set-key [(f11)] 'goto-next-locus)

(global-set-key [(f5)] 'revert-buffer)

(global-set-key (kbd "M-<f5>") 'reload-dotemacs)

(global-set-key "\C-c#" 'comment-region)

(global-set-key "\M-s" 'find-rsys)

(global-set-key "\C-c\M-s" 'find-file-rsys)

(global-set-key "\C-c\C-x\C-f" 'find-file-at-point)

(global-set-key "\C-c\C-x\C-f" 'find-file-at-point)

(global-set-key (kbd "M-SPC") 'imenu)

(global-set-key "\C-xp" 'python-mode)

(global-set-key "\C-xt" '(find-file todo))

(global-set-key [(f1)] '(find-file rsys))

;;;;;;;;;;;;;;;;


;; General and mode specific settings

(global-font-lock-mode 1)
(require 'pair-mode)
(winner-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(setq scroll-step 1)

; Font and Appearance
(set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
(setq-default truncate-partial-width-windows nil)
(require 'color-theme)
(require 'htmlfontify)

; Luddite Mode
(tool-bar-mode -1) 
(menu-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-showhide-fringe-menu-customize-disable)
(blink-cursor-mode -1)

; file types
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.build\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vcproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

; Org-Mode
(require 'org-export-latex)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

; Fix grep-find
(setq grep-find-command "find . -exec grep -nH -e '?' {} ; -print")

;Haskell-mode
(load "haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

; Perforce
(require 'p4)


; W3m
(require 'w3m-load)

; Windows
(require 'martin-darkroom)

; hide-lines
(autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
(global-set-key "\C-ch" 'hide-lines)


;; Steve Yegge Stuff

;; Enable backup files.
(setq make-backup-files t)
;; Enable versioning with default values
(setq version-control t)
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;;recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key [(meta f12)] 'recentf-open-files)

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
;; (setq desktop-save 'if-exists)
;; (desktop-save-mode 1)

;; ;; load the desktop on startup
;; (desktop-load-default)
;; ;; automatically save the desktop on exit.
;; (setq desktop-enable t) 


;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

(global-set-key [(f12)] 'ibuffer)

(when (require 'bubble-buffer nil t)
  (global-set-key [f11] 'bubble-buffer-next)
  (global-set-key [(shift f11)] 'bubble-buffer-previous))
(setq bubble-buffer-omit-regexp "\\(^ .+$\\|\\*Messages\\*\\|*compilation\\*\\|\\*.+output\\*$\\|\\*TeX Help\\*$\\|\\*vc-diff\\*\\|\\*Occur\\*\\|\\*grep\\*\\|\\*cvs-diff\\*\\)")

(defun xsteve-save-current-directory ()
  "Save the current directory to the file ~/.emacs.d/current-directory"
  (interactive)
  (let ((dir default-directory))
    (with-current-buffer (find-file-noselect "~/.emacs.d/current-directory")
      (delete-region (point-min) (point-max))
      (insert (concat dir "\n"))
      (save-buffer)
      (kill-buffer (current-buffer)))))
(global-set-key [(super f10)] 'xsteve-save-current-directory)
