;; Configuration, variables, global modes


;; # Startup
(setq-default inhibit-startup-message t)
(setq warning-minimum-level :emergency)  ;; Don't bombard me with plugin warnings and hijack focus.
(add-to-list 'default-frame-alist '(fullscreen . maximized))  ;; Default to full-screen.
(setq diary-file "~/personal/diary")


;; # Enable commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; # Save screen space
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)


;; # Screen, buffers, viewport
(column-number-mode t)
(global-hi-lock-mode t)
(global-display-line-numbers-mode t)


;; # Indentation
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)  ;; Keep an eye on this, might cause problems.
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default sgml-basic-offset 2)


;; # Text, formatting, editing
(setq kill-whole-line t)
(setq-default truncate-lines t)
(setq case-fold-search t)  ;; Search ignores case by default.
(setq-default case-fold-search t)


;; # Parens
(setq show-paren-delay 0)


;; # Visuals
(highlight-indent-guides-mode t)


;; # Search
(setq-default isearch-lazy-highlight-initial-delay 0)
;; Makes deadgrep start the search form the current directory instead of the project base.
(defun my/get-current-dir () default-directory)
(setq deadgrep-project-root-function #'my/get-current-dir)


;; # Safety, backups
(setq confirm-kill-emacs 'yes-or-no-p)
(setq backup-directory-alist `((".*" . "~/.tmp")))
(setq auto-save-file-name-transforms `((".*" "~/.tmp" t)))
(setq auto-save-list-file-prefix (concat "~/.tmp" ".auto-saves-"))
;; (setq make-backup-files nil)  ;; Temporary. Using this for work. Causing issues with watchers picking up backup files and crashing.
;; (setq auto-save-default nil)  ;; ^^
;; (setq create-lockfiles nil)   ;; ^^


;; # Dired
(setq wdired-allow-to-change-permissions t)  ;; Allow editing file modes.
(setq dired-listing-switches "-laXGh --group-directories-first")  ;; Order directories first.


;; # Describe, help
(setq help-window-select t)  ;; Focus describe buffers on load.


;; # System clipboard
(setq x-select-enable-clipboard t)  ;; Allows you to copy into the system clipboard.
(setq save-interprogram-paste-before-kill t)  ;; Save system clipboard to kill ring.


;; # Filename patterns
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\rc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))


;; # Packages, modes
(setq uniquify-buffer-name-style 'forward)
(ido-mode t)
(show-paren-mode t)
(setq show-paren-delay 0)
(electric-pair-mode 0)
(autopair-global-mode t)
(setq highlight-indent-guides-method 'bitmap)
(yas-global-mode t)


;; # org-mode
(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))


;; # TRAMP
;; Not exactly sure what this is, but it enables me to actually use TRAMP.
(setq tramp-terminal-type "tramp")
;; This line also needs to be in the REMOTE .zshrc file.
;; [[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return


;; # Variables
(custom-set-variables
 '(fill-column 80))


;; # Hardware
(setq printer-name "HLL2350DW")
