(server-start)

;load-path
(setq load-path (cons "C:/Program Files/Emacs/site-lisp/w3m" load-path))

(add-hook 'ido-setup-hook
	  (lambda ()
	    (setq ido-enable-flex-matching t)))
(ido-mode)

;; Load emacsw32 if found.
(progn
  (require 'emacsw32 nil t)
  (unless (featurep 'emacsw32)
    (lwarn '(emacsw32) :error "Could not find emacsw32.el")))

(defvar rsys (getenv "RSYS"))

(defvar todo "\\\\gin\\common\\forniall\\todo.org")

(defun insert-pwd ()
  (interactive)
  (insert (replace-regexp-in-string "Directory " "" (pwd))))

(defun confluence-listify ()
  (interactive)
  (replace-regexp "^[ 	]*" "| (x) | " nil (region-beginning) (region-end))
  (replace-regexp "$" " | |" nil (region-beginning) (region-end)))

(defun confluence-linkify-jira-keys ()
  (interactive)
  (replace-regexp "[HTCER]..-[0-9]*" "[\\&|http://jira.dub.havok.com/browse/\\&]" nil (region-beginning) (region-end)))

(defun python-insert-pdb-settrace ()
  (interactive)
  (insert "import pdb")
  (indent-for-tab-command)
  (newline-and-indent)
  (insert "pdb.set_trace()"))

(global-set-key "\M-p" 'python-insert-pdb-settrace)

;; (defun pylint ()
;;   (interactive)
;;   (shell-command (format "pylint -e %s" (buffer-file-name))))

(require 'goto-last-change)


(setq python-check-command "pylint -e")
;; (add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-xc" 'pylint)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; ;;; Electric Pairs
;; (add-hook 'python-mode-hook
;;      (lambda ()
;;       (define-key python-mode-map "\"" 'electric-pair)
;;       (define-key python-mode-map "\'" 'electric-pair)
;;       (define-key python-mode-map "(" 'electric-pair)
;;       (define-key python-mode-map "[" 'electric-pair)
;;       (define-key python-mode-map "{" 'electric-pair)))

;; (defun electric-pair ()
;;   "Insert character pair without sournding spaces"
;;   (interactive)
;;   (let (parens-require-spaces)
;;     (insert-pair)))

(setq python-python-command "ipython")

(defvar ipython-completion-command-string
  "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(defadvice py-execute-buffer (around python-keep-focus activate)
  "return focus to python code buffer"
  (save-excursion ad-do-it))

;; (require 'pymacs)

;; (pymacs-load "ropemacs" "rope-")

;; (provide 'python-programming)


(defun dotemacs ()
  (interactive)
  (find-file "c:/Program Files/Emacs/site-lisp/default.el"))

(defun find-rsys (token)
  (interactive "SSearch RSYS for:")
  (let ((regex ".*\\.\\(py\\|txt\\|build\\)")
	(exe "C:\\cygwin\\bin\\find"))
    (grep-find
     (format "%s %S -regex %S -noleaf -type f -exec grep -nHiIs -e '%s' {} ; -print" exe rsys regex token))))

(defun find-file-rsys (pattern)
  (interactive "SSearch RSYS for File Named:")
  (let ((exe "C:\\cygwin\\bin\\find"))
    (grep-find
      (format "%s %S -iname \"*%S*.py\" -noleaf -type f -print " exe rsys pattern))))

(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun reload-dotemacs ()
  (interactive)
  (load-library "default"))

(defun open-rsys ()
  (interactive)
  (find-file rsys))

(defun open-todo ()
  (interactive)
  (find-file todo))

(defun explore-here ()
  (interactive)
  (shell-command "start .")
  (message (format "Exploring %s" (replace-regexp-in-string "Directory " "" (pwd)))))

(defun cmd-here ()
  (interactive)
  (shell-command "start cmd .")
  (message (format "Cmding %s" (replace-regexp-in-string "Directory " "" (pwd)))))

(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))

(defun uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((end (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
          (replace-match "\\1\n\\2")))))
  
(defun uniquify-all-lines-buffer ()
    "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (uniquify-all-lines-region (point-min) (point-max)))

; make file name and computer title
(set-default 'frame-title-format 
  (list "" "emacs" "@" (getenv "HOSTNAME") " : %f" ))

(setq ansi-color-for-comint-mode t)

(setq visible-bell t)

(global-set-key [(f1)] 'open-rsys)

(global-set-key [(f2)] 'explore-here)
(global-set-key [(meta f2)] 'cmd-here)

(global-set-key [(f5)] 'revert-buffer)

(global-set-key [(f6)] 'reload-dotemacs)

(global-set-key [(f7)] 'python-describe-symbol)

(global-set-key [(f11)] 'goto-next-locus)

(global-set-key [(f12)] 'ibuffer)

(global-set-key [(meta f12)] 'recentf-open-files)

(global-set-key "\C-c#" 'comment-region)

(global-set-key "\M-s" 'find-rsys)

(global-set-key "\C-c\M-s" 'find-file-rsys)

(global-set-key "\C-c\C-x\C-f" 'find-file-at-point)

(global-set-key "\C-c\C-x\C-f" 'find-file-at-point)

(global-set-key (kbd "M-SPC") 'imenu)

(global-set-key "\C-xp" 'python-mode)

(global-set-key "\C-xt" 'open-todo)

(global-set-key "\M-r" 'replace-string)

(global-set-key "\C-x\M-r" 'replace-regexp)


;;;;;;;;;;;;;;;;


(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))


(setq focus-follows-mouse t)
(setq mouse-autoselect-window t)

;; General and mode specific settings

(global-font-lock-mode 1)
(require 'pair-mode)
(winner-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(setq scroll-step 1)
(which-function-mode 1)
(show-paren-mode 1)

; Font and Appearance
(set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
(add-to-list 'default-frame-alist '(font . "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1"))
(setq-default truncate-partial-width-windows nil)
(require 'color-theme)
(require 'htmlfontify)

(defun change-color ()
  (progn (color-theme-select)
	 (color-theme-ld-dark)
	 (kill-this-buffer)))

(change-color)

(setq inhibit-startup-echo-area-message t)                                      
(setq initial-scratch-message nil)                                              
(setq inhibit-splash-screen t)                                                  
(setq inhibit-startup-message t) 

; Luddite Mode
(tool-bar-mode -1) 
(menu-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-showhide-fringe-menu-customize-disable)
(blink-cursor-mode -1)

(defadvice kill-new (before kill-new-push-xselection-on-kill-ring activate)
  "Before putting new kill onto the kill-ring, add the clipboard/external selection to the kill ring"
  (let ((have-paste (and interprogram-paste-function
                         (funcall interprogram-paste-function))))
    (when have-paste (push have-paste kill-ring))))


; file types
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.build\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.mapping\\'" . python-mode))
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
(org-remember-insinuate)
(setq org-directory "C:\\Users\\niall\\Notes")
(setq org-default-notes-file (concat org-directory "\\notes.org"))
(define-key global-map "\C-cr" 'org-remember)

(require 'org-export-latex)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

; Fix grep-find
(setq grep-find-command "find . -noleaf -type f -exec grep -nHiIs -e '?' {} ; -print")

; Fix Python startup
(setq python-python-command-args '())



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

(setq delete-old-versions t)

;; Enable backup files.
(setq make-backup-files t)
;; Enable versioning with default values
(setq version-control t)
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; ;;recentf
;; (require 'recentf)
;; (recentf-mode 1)
;; (setq recentf-max-saved-items 500)
;; (setq recentf-max-menu-items 60)


;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(desktop-save-mode 1)
;; ;; ;; load the desktop on startup
;;(desktop-load-default)
;; ;; ;; automatically save the desktop on exit.
(setq desktop-enable t) 


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

(when (require 'bubble-buffer nil t)
  (global-set-key [f11] 'bubble-buffer-next)
  (global-set-key [(shift f11)] 'bubble-buffer-previous))
(setq bubble-buffer-omit-regexp "\\(^ .+$\\|\\*Messages\\*\\|*compilation\\*\\|\\*.+output\\*$\\|\\*TeX Help\\*$\\|\\*vc-diff\\*\\|\\*Occur\\*\\|\\*grep\\*\\|\\*cvs-diff\\*\\)")

;; (defun xsteve-save-current-directory ()
;;   "Save the current directory to the file ~/.emacs.d/current-directory"
;;   (interactive)
;;   (let ((dir default-directory))
;;     (with-current-buffer (find-file-noselect "~/.emacs.d/current-directory")
;;       (delete-region (point-min) (point-max))
;;       (insert (concat dir "\n"))
;;       (save-buffer)
;;       (kill-buffer (current-buffer)))))
;; (global-set-key [(super f10)] 'xsteve-save-current-directory)

;; macros

(setq confluence-list
   [?\M-x ?r ?p ?l ?e ?\C-? ?\C-? ?\C-? ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p ?\C-m ?^ ?  ?* ?\C-m ?\C-m ?\M-< ?\M-x ?p ?l ?r ?e ?- ?\C-? ?\C-? ?\C-? ?\C-? ?\C-? ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p ?\C-m ?^ ?\C-m ?| ?  ?\( ?x ?\) ?  ?| ?  ?\C-m ?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p ?\C-m ?$ ?\C-m ?\S-  ?| ?  ?|])

(require 'ansi-color)
(require 'eshell)
(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
			      eshell-last-output-end))
(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)