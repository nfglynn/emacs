(server-start)

(setenv "EMACS" "True")

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;load-path
(setq load-path (cons "." load-path))
;;(setq load-path (cons "w3m" load-path))

;; (add-hook 'ido-setup-hook
;; 	  (lambda ()
;; 	    (setq ido-enable-lex-matching t)))

(require 'tramp)

;; ;; Load emacsw32 if found.
;; (progn
;;   (require 'emacsw32 nil t)
;;   (unless (featurep 'emacsw32)
;;     (lwarn '(emacsw32) :error "Could not find emacsw32.el")))

(defvar rsys (getenv "RSYS"))

(defvar todo "\\\\gin\\common\\forniall\\todo.org")

(defun insert-pwd ()
  (interactive)
  (insert (replace-regexp-in-string "Directory " "" (pwd))))

(defun confluence-listify ()
  (interactive)
  (replace-regexp "^[ 	]*" "| (x) | " nil (region-beginning) (region-end))
  (replace-regexp "$" " | |" nil (region-beginning) (region-end)))

(defun jira-pp (key)
  (interactive "sJira Key: ")
  (insert
   (shell-command-to-string
    (concat
     "d:/p4/Other/Personal/Niall.Glynn/utils/pretty_print_jira_issue.py "
     key))))

(defun python-insert-pdb-settrace ()
  (interactive)
  (insert "import ipdb;ipdb.set_trace()"))

(defun python-insert-todo ()
  (interactive)
  (insert "# todo <nfg>: "))

(defun python-find-todos ()
  (interactive)
  (projectile-ag "# todo <nfg>: "))

(global-set-key "\M-p" 'python-insert-pdb-settrace)
(global-set-key "\M-t" 'python-insert-todo)
(global-set-key "\C-c\M-t" 'python-find-todos)


;; (defun pylint ()
;;   (interactive)
;;   (shell-command (format "pylint -e %s" (buffer-file-name))))

(require 'goto-last-change)


(setq python-check-command "c:/users/niall.glynn/bin/pycheck.py")
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

(setq py-shell-name "ipython")

;;(setq python-python-command "ipython")

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
  (find-file "c:/emacs/site-lisp/emacs/default.el"))

(defun notes ()
  (interactive)
  (find-file "c:/users/niall.glynn/Desktop/notes/notes.org"))

(defun perforce ()
  (interactive)
  (find-file "d:/p4"))

(defun head ()
  (interactive)
  (find-file "d:/p4/Development/HEAD/Build/ReleaseSystem"))

(defun branch ()
  (interactive)
  (find-file "d:/p4/Release/2015_1/2015_1_Branch/Build/ReleaseSystem"))

(defun devops ()
  (interactive)
  (find-file "d:/p4/Development/2016_1/DevOps"))


(defun im-logs ()
  (interactive)
  (find-file "C:/Users/niall.glynn/AppData/Roaming/.purple/logs/jabber/niall.glynn@jabber.havok.com"))

;; (defun compilation ()
;;   (interactive)
;;   (find-file "d:/p4/Development/2015_1/Compilation/Build/ReleaseSystem"))

(defvar cc-templates-path "d:/p4/Other/Infrastructure/CruiseControl/ccConfig/Templates")

(defun templates ()
  (interactive)
  (find-file cc-templates-path))

(defun infrastructure ()
  (interactive)
  (find-file "d:/p4/Other/Infrastructure"))

(defun search-cctemplates (pattern)
  (interactive "sPattern: ")
  (ag pattern 'cc-templates-path))


(defun personal ()
  (interactive)
  (find-file "d:/p4/Other/Personal/Niall.Glynn"))

(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun reload-dotemacs ()
  (interactive)
  (load-library "default"))

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

(global-set-key [(f8)] 'helm-projectile)

(global-set-key [(f9)] 'p4-edit)

(global-set-key [(f10)] 'p4v-show)

(global-set-key [(f12)] 'goto-next-locus)

(global-set-key [(meta f12)] 'compile)

(global-set-key "\C-c#" 'comment-region)

(global-set-key "\M-s" 'projectile-ag)

(global-set-key "\C-c\M-s" 'helm-projectile)

(global-set-key "\C-x\M-s" 'search-cctemplates)

(global-set-key "\C-c\C-x\C-f" 'find-file-at-point)

(global-set-key "\C-c\C-x\C-f" 'find-file-at-point)

(global-set-key (kbd "M-SPC") 'imenu)

(global-set-key "\C-xp" 'python-mode)

(global-set-key "\C-xj" 'jira-pp)

(global-set-key "\C-xt" 'open-todo)

(global-set-key "\M-r" 'replace-string)


(global-set-key "\C-x\M-r" 'replace-regexp)

(global-set-key "\C-c!" 'flymake-goto-next-error)

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

;; (setq focus-follows-mouse t)
;; (setq mouse-autoselect-window t)

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

;; (defun change-color ()
;;   (progn (color-theme-select)
;; 	 (color-theme-ld-dark)
;; 	 (kill-this-buffer)))

;; (change-color)

(color-theme-solarized-dark)

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
(add-to-list 'auto-mode-alist '("\\.dict\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.mapping\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vcproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

; Org-Mode
;; (org-remember-insinuate)
(setq org-directory "C:\\Users\\niall.glynn\\Desktop\\notes")
(setq org-default-notes-file (concat org-directory "\\captured.org"))
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
;;(require 'w3m-load)

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

;;(require 'darkroom-mode)

(setq ansi-color-for-comint-mode t)




(put 'upcase-region 'disabled nil)

(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))

(defun load-tags ()
  "Load TAGS file searching upwards"
  (interactive "")
  (let ((my-tags-file (find-file-upwards "TAGS")))
    (when my-tags-file
      (message "Loading tags file: %s" my-tags-file)
      (visit-tags-table my-tags-file))))

(setq path-to-ctags "c:\\users\\niall.glynn\\bin\\ctags.exe")

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s --verbose=yes -e -R -h *.py  %s" path-to-ctags dir-name (directory-file-name dir-name))))


(setq next-line-add-newlines nil)

(defun toggle-full-screen () (interactive) (shell-command "emacs_fullscreen.exe"))
(global-set-key [f11] 'toggle-full-screen)


(defun invoke-p4v-cmd (cmd)
  (let ((file (if (equal major-mode 'dired-mode)
                  (dired-get-file-for-visit)
                (buffer-file-name))))
    (when file
      (shell-command (concat "\"c:/Program Files/Perforce/p4v.exe\" -cmd \"" cmd " " file "\" &")))))


(defun p4v-timelapse ()
  "show revision tree"
  (interactive)
  (invoke-p4v-cmd "annotate"))

(define-key p4-prefix-map "T" 'p4v-timelapse)

(defun p4v-tree ()
  "show revision tree"
  (interactive)
  (invoke-p4v-cmd "tree"))

(defun p4v-show ()
  "show revision tree"
  (interactive)
  (invoke-p4v-cmd "open"))

(define-key p4-prefix-map "g" 'p4v-tree)

(defun p4v-history ()
  "Show history"
  (interactive)
  (invoke-p4v-cmd "history"))

(define-key p4-prefix-map "x" 'p4v-history)


(autoload 'mingus "mingus")

;; (add-to-list 'load-path "./workgroups.el")
;; (require 'workgroups)
;; (setq wg-prefix-key (kbd "C-z"))
;; (workgroups-mode 1)
;; (wg-load "./workgroups")


(add-to-list 'load-path "./emacs-color-theme-solarized")

 (defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))
(transparency 99)


;; (ido-mode t)
;; (require 'ido-ubiquitous)
;; (ido-ubiquitous)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)


(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(global-set-key [(f3)] 'prelude-copy-file-name-to-clipboard)


;; (require 'compile)
;; (setq compilation-ask-about-save nil)
;; (setq compilation-save-buffers-predicate '(lambda () nil))
;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (set (make-local-variable 'compile-command)
;; 		 (let ((file (file-name-nondirectory buffer-file-name)))
;; 		   (format "python c:/users/niall.glynn/bin/pylint.py %s"
;; 			   (file-name-sans-extension file))))))



(setq column-number-mode t)

;; Configure flymake for Python
;; (setq pylint "c:/users/niall.glynn/bin/pycheck.bat")
;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list (expand-file-name pylint "") (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pylint-init)))
;; ;; Set as a minor mode for Python
;; (add-hook 'python-mode-hook '(lambda () (flymake-mode)))

(require 'uniquify)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.bmk")
 '(org-agenda-files (quote ("~/Desktop/notes/notes.org")))
 '(python-python-command "bpython")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)


(setq backup-projects
   [?\C-a ?\C-  ?\C-s ?  ?- right ?\C-w ?c ?p ?  ?\C-e ?\C-r ?\\ right ?\C-  ?\C-e ?\M-w ?  ?B ?a ?k ?\\ ?\C-y ?\C-n ?\C-a])

(helm-mode)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)



(defun grep-enum (token)
  (interactive "SSearch for enum value:")
  (let ((regex ".*\\.\\(h\\)")
	(exe "C:\\cygwin\\bin\\find"))
    (grep-find
     (format "%s %S -regex %S -noleaf -type f -exec grep -nHiIs -e '%s' {} ; -print" exe "D:\\clean\\depot\\Development\\HEAD\\Source" regex token))))



(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
	ad-do-it))

(ad-activate 'grep-compute-defaults)


;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)
(custom-set-variables
 '(help-at-pt-timer-delay 0.9)
 '(help-at-pt-display-when-idle '(flymake-overlay)))

(grep-a-lot-setup-keys)
(grep-a-lot-advise igrep)
(add-hook 'python-mode-hook 'elpy-enable)

;; (setq elpy-rpc-backend "jedi")

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(projectile-mode)
(persp-mode)
(require 'persp-projectile)
(global-set-key ["<apps>"] 'projectile-persp-switch-project)

(custom-set-faces
 '(diff-added ((t (:foreground "Green"))) 'now)
 '(diff-removed ((t (:foreground "Red"))) 'now)
 )


;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error t)    ; now you should get a backtrace


(setq set-mark-command-repeat-pop t)


(global-set-key (kbd "C-c <SPC>") 'helm-all-mark-rings)


;;;
;; Move to beginning of word before yanking word in isearch-mode.
;; Make C-s C-w and C-r C-w act like Vim's g* and g#, keeping Emacs'
;; C-s C-w [C-w] [C-w]... behaviour.

(require 'thingatpt)

(defun my-isearch-yank-word-or-char-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'my-isearch-yank-word-or-char-from-beginning 
			     'isearch-yank-word-or-char
			     isearch-mode-map))

(add-hook 'isearch-mode-hook
 (lambda ()
   "Activate my customized Isearch word yank command."
   (substitute-key-definition 'isearch-yank-word-or-char 
			      'my-isearch-yank-word-or-char-from-beginning
			      isearch-mode-map)))
(setq-default indent-tabs-mode nil)

(setq org-todo-keywords '((type "TODO" "WAITING" "DOING" "|" "DONE")))

(require 'ein)

 ;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(set-frame-parameter (selected-frame) 'alpha '(85 75))

(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
