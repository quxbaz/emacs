;; Configuration, variables, global modes


;; # Startup
(setq-default inhibit-startup-message t)
(setq warning-minimum-level :emergency)  ;; Don't bombard me with plugin warnings and hijack focus.
(add-to-list 'default-frame-alist '(fullscreen . maximized))  ;; Default to full-screen.
(setq diary-file "~/personal/diary")


;; Data
(setq my/init-files (list user-init-file
                          (concat user-emacs-directory "bugs.el")
                          (concat user-emacs-directory "scratch.el")
                          (concat user-emacs-directory "sql-notebook.el")
                          "~/personal/automotive.org"
                          "~/personal/bookmarks.org"
                          "~/personal/cad.org"
                          "~/personal/diary"
                          "~/personal/dogs.org"
                          "~/personal/food.org"
                          "~/personal/main.org"
                          "~/personal/media-queue.org"
                          "~/personal/programming.org"
                          "~/personal/promise.org"
                          "~/personal/rem.org"
                          "~/personal/rem-structured.org"
                          "~/personal/archive/buy.org"
                          "~/personal/archive/people.org"
                          "~/personal/archive/projects.org"
                          "~/personal/automotive/subaru-outback-2008/Notes.org"
                          "~/personal/automotive/subaru-outback-2008/log.org"
                          "~/personal/exercise/exercises.org"
                          "~/personal/exercise/misc.org"
                          "~/personal/exercise/movement.org"
                          "~/personal/exercise/records.org"
                          "~/personal/exercise/routines.org"
                          "~/personal/linux/emacs.org"
                          "~/personal/linux/linux.org"
                          "~/conf/sync.org"
                          "~/conf/i3/config"
                          "~/conf/zsh/.zshrc"
                          "~/work/wnmu/Todo.org"
                          "~/work/wnmu/huddle-notes.org"
                          "~/work/wnmu/projects.org"
                          "~/work/wnmu/records/creds.org"
                          "~/work/wnmu/records/qr-code.org"
                          "~/work/wnmu/refactor.org"))


;; Logging
(setq message-log-max 10000)


;; # Enable commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; # Save screen space
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


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
(setq-default sentence-end-double-space nil)  ;; Make single-space end a sentence. Affects filling.


;; # Parens
(setq show-paren-delay 0)
(show-paren-mode t)


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
(setq dired-dwim-target t)  ;; Use other dired window as default copy/move path.
(setq wdired-allow-to-change-permissions t)  ;; Allow editing file modes.
(setq dired-listing-switches "-laXGh --group-directories-first")  ;; Order directories first.
(setq dired-omit-files "^\\.$\\|^\\.\\.$")  ;; Hide . and .. directories.
(setq dired-omit-lines nil)
(setq dired-omit-extensions nil)


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
(add-to-list 'auto-mode-alist '("\\.FCMacro$" . python-mode))
(add-to-list 'auto-mode-alist '("\\rc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))


;; # Packages, modes
(setq uniquify-buffer-name-style 'forward)
(setq ivy-do-completion-in-region nil)
(ivy-mode t)
(electric-pair-mode 0)
(autopair-global-mode t)
(setq highlight-indent-guides-method 'bitmap)
(yas-global-mode t)
(global-corfu-mode t)
(setq blink-matching-paren nil) ;; Disable paredit delay when closing round.


;; # org-mode
(setq org-adapt-indentation t)  ;; Indent after headings.
;; Add org-agenda files.
(custom-set-variables
 '(org-agenda-files '("~/work/wnmu/Todo.org")))
(custom-set-faces
 '(org-ellipsis ((t (:foreground "gray50")))))
(setq org-ellipsis " [...]")
(setq org-todo-keywords '((sequence "TODO"              "NEXT"                 "IN-PROGRESS"             "WAITING"              "FAILED"                "QUESTION"              "REVIEW"              "LIMBO"              "BACKLOG"      "|"     "NOTE"              "DONE-INT"                  "DONE")))
(setq org-todo-keyword-faces     '(("TODO" . "yellow") ("NEXT" . "OrangeRed") ("IN-PROGRESS" . "cyan1") ("WAITING" . "orange") ("FAILED" . "DeepPink") ("QUESTION" . "grey50") ("REVIEW" . "orchid") ("LIMBO" . "grey50") ("BACKLOG" . "grey50") ("NOTE" . "grey50") ("DONE-INT" . "PaleGreen2") ("DONE" . "green")))


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


;; # SQL
(setq sql-connection-alist '((wnmu-edu-db (sql-product 'mysql)
                                          (sql-user "david")
                                          (sql-password "")
                                          (sql-server "localhost")
                                          (sql-database "wnmu_edu_db"))))
