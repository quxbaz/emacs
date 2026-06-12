;; Configuration, variables, global modes


;; # Startup
(setq-default inhibit-startup-message t)
(setq initial-buffer-choice  ;; Start in dired on the invocation directory (e.g. emacs --chdir ~/foo).
      (lambda () (dired default-directory)))
(setq warning-minimum-level :emergency)  ;; Don't bombard me with plugin warnings and hijack focus.
(add-to-list 'default-frame-alist '(fullscreen . maximized))  ;; Default to full-screen.
(setq diary-file "~/personal/diary")


;; Logging
(setq message-log-max 5000)
(setq list-command-history-max 1000)  ;; Max commands shown by list-command-history.
(lossage-size 1000)  ;; Max number of keystrokes to save.


;; # Enable commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;; # Save screen space
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; # Screen, buffers, viewport
(column-number-mode t)
(global-hi-lock-mode t)
(global-display-line-numbers-mode t)


;; # Whitespace, indentation
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
;; highlight-indent-guides derives its guide colors from the `default' face.
;; Under the daemon there's no frame at init time, so `default' has no usable
;; color and the mode errors when it auto-sets faces. Defer enabling until a
;; graphical frame exists.
(setq highlight-indent-guides-method 'bitmap)
(defun my/enable-highlight-indent-guides (&optional frame)
  (when (display-graphic-p frame)
    (remove-hook 'after-make-frame-functions #'my/enable-highlight-indent-guides)
    (with-selected-frame frame
      (highlight-indent-guides-mode t))))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/enable-highlight-indent-guides)
  (highlight-indent-guides-mode t))


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
(with-eval-after-load 'dired (add-to-list 'dired-no-confirm 'load))  ;; L (dired-do-load) without confirmation.
(advice-add 'dired-find-buffer-nocreate :override #'ignore)  ;; Always create a fresh dired buffer instead of reusing an existing one.
(with-eval-after-load 'dired
  (define-key dired-mode-map "q" (lambda () (interactive) (quit-window t))))  ;; Kill the dired buffer on 'q' instead of burying it.


;; # magit
(setq magit-section-initial-visibility-alist
      '((recent . show)
        (unpushed . show)
        (untracked . show)
        (unstaged . show)        ;; Expand the Unstaged section (show the file list)...
        ([file unstaged] . hide)))  ;; ...but keep each file's diff collapsed.


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


;; # Shell, terminal
;; Don't show  *Async Shell Command* buffer.
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))


;; # org-mode
(setq org-adapt-indentation t)  ;; Indent after headings.
(setq org-confirm-babel-evaluate nil)  ;; Evaluate code without confirmation.
;; Add org-agenda files.
(custom-set-variables
 '(org-agenda-files '("~/work/wnmu/Todo.org")))
(custom-set-faces
 '(org-ellipsis ((t (:foreground "gray50")))))
(setq org-ellipsis " [...]")
(setq org-todo-keywords '((sequence "TODO"              "NEXT"                 "IN-PROGRESS"             "WAITING"              "FAILED"                "QUESTION"              "REVIEW"              "LIMBO"              "BACKLOG"      "|"     "NOTE"              "DONE-INT"                  "DONE")))
(setq org-todo-keyword-faces     '(("TODO" . "yellow") ("NEXT" . "OrangeRed") ("IN-PROGRESS" . "cyan1") ("WAITING" . "orange") ("FAILED" . "DeepPink") ("QUESTION" . "grey50") ("REVIEW" . "orchid") ("LIMBO" . "grey50") ("BACKLOG" . "grey50") ("NOTE" . "grey50") ("DONE-INT" . "PaleGreen2") ("DONE" . "green")))
(font-lock-add-keywords 'org-mode '(("`[^`\n]+`" 0 'org-code t)))  ;; Highlight `backtick` spans like =code=.
(with-eval-after-load 'org
  (set-face-attribute 'org-block-begin-line nil :foreground (face-foreground 'org-verbatim nil t))
  (set-face-attribute 'org-block-end-line nil :foreground (face-foreground 'org-verbatim nil t)))


;; # Packages, modes, misc
(setq uniquify-buffer-name-style 'forward)
(setq ivy-do-completion-in-region nil)
(ivy-mode t)
(electric-pair-mode 0)
(autopair-global-mode t)
(yas-global-mode t)
(setq yas-triggers-in-field t)  ;; Enable nested snippet expansions.
(global-corfu-mode t)
(setq blink-matching-paren nil) ;; Disable paredit delay when closing round.
(setq bookmark-save-flag 1)     ;; Save ~/.emacs.d/bookmarks on every change
;; Control hippie-expand order of expansion.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


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


;; Lua
(setq lua-indent-level 2)


;; # Modes
(add-hook 'calc-mode-hook #'maf-mode)

(define-minor-mode mathjax-mode
  "Mode for writing MathJax snippets."
  ;; If t, the minor mode is enabled by default.
  :init-value nil
  ;; Mode Line text when the minor mode is active.
  :lighter " Mathjax"
  ;; Custom keybindings when the minor mode is active.
  :keymap (let ((map (make-sparse-keymap)))
            ;; (define-key map (kbd "KEY-SEQUENCE") 'COMMAND)
            map)
  ;; Specifies whether the mode is global (t) or buffer local (nil).
  :global nil
  ;;The customization group under which the mode settings are categorized.
  :group 'mathjax-mode)
