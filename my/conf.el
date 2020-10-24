;; Basics
(setq-default inhibit-startup-message t)


;; Enable commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; Save screen space
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Screen, buffers, viewport
(global-linum-mode t)
(column-number-mode t)
(global-hi-lock-mode t)
(set-face-attribute 'default nil :font "dejavu sans mono-10")


;; Indentation
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)  ;; keep an eye on this, might cause problems
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default sgml-basic-offset 2)


;; Text, formatting, editing
(setq kill-whole-line t)
(setq show-paren-delay 0)
(setq-default fill-column 70)
(setq-default truncate-lines t)
(setq case-fold-search t)  ;; Search ignores case by default.
(setq-default case-fold-search t)


;; Search
(setq-default isearch-lazy-highlight-initial-delay 0)


;; Safety
(setq confirm-kill-emacs 'yes-or-no-p)
(setq backup-directory-alist `(("." . "~/.emacs.backups")))


;; Misc
(setq uniquify-buffer-name-style 'forward)
(setq x-select-enable-clipboard t)  ;; Allows you to copy into the system clipboard.


;; Modes
(ido-mode t)
(iswitchb-mode t)
(show-paren-mode t)
(yas-global-mode t)
(autopair-global-mode)


;; Modes / auto-complete-mode
(setq ac-auto-show-menu 0)


;; Theme
(load-theme 'wombat t)


;; Filename patterns
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . javascript-mode))
