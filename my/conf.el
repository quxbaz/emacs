;; Configuration, variables, global modes


;; emacs 31
;; (ibuffer-human-readable-size t)  ;; KB/MB instead of raw byte counts


;; # Startup
(setq-default inhibit-startup-message t)
(setq initial-buffer-choice  ;; Start in dired on the invocation directory (e.g. emacs --chdir ~/foo).
      (lambda () (dired default-directory)))
(setq warning-minimum-level :emergency)  ;; Don't bombard me with plugin warnings and hijack focus.
;; Initial-frame settings (fullscreen, GUI bars, font, colors) live in
;; early-init.el so they apply before the frame appears (no startup flash).
(setq diary-file "~/personal/diary")


;; Logging
(setq message-log-max 5000)  ;; Max number of lines to keep in the message log buffer.
(setq list-command-history-max 1000)  ;; Max commands shown by list-command-history.
(lossage-size 1000)  ;; Max number of keystrokes to save.


;; # Enable commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;; # Save screen space
;; menu-bar, tool-bar, and scroll-bar are disabled in early-init.el (via
;; default-frame-alist) so they never flash on at startup.


;; # Screen, buffers, viewport
(column-number-mode t)
(global-hi-lock-mode t)
(global-display-line-numbers-mode t)
(global-so-long-mode 1)  ;; Don't choke on files with very long lines.


;; # Whitespace, indentation
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)  ;; Keep an eye on this, might cause problems.
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default sgml-basic-offset 2)
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;; Trim trailing whitespace on save.


;; # Text, formatting, editing
(setq kill-whole-line t)
(setq-default truncate-lines t)
(setq case-fold-search t)  ;; Search ignores case by default.
(setq-default case-fold-search t)
(setq-default sentence-end-double-space nil)  ;; Make single-space end a sentence. Affects filling.
(setq-default cycle-spacing-actions '(delete-all-space just-one-space restore))
(setq duplicate-line-final-position -1)    ;; duplicate-dwim leaves point on the last new line.
(setq duplicate-region-final-position -1)  ;; duplicate-dwim leaves the region around the last copy.


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
(setq vc-follow-symlinks t)  ;; Auto-follow symlinks without prompting.
(setq confirm-kill-emacs 'yes-or-no-p)
(setq backup-directory-alist `((".*" . "~/.tmp")))
(setq auto-save-file-name-transforms `((".*" "~/.tmp" t)))
(setq auto-save-list-file-prefix (concat "~/.tmp" ".auto-saves-"))
;; (setq make-backup-files nil)  ;; Temporary. Using this for work. Causing issues with watchers picking up backup files and crashing.
;; (setq auto-save-default nil)  ;; ^^
;; (setq create-lockfiles nil)   ;; ^^


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
