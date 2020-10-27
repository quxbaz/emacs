;; Configuration, variables, global modes


;; Startup
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


;; Indentation
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)  ;; Keep an eye on this, might cause problems.
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default sgml-basic-offset 2)


;; Text, formatting, editing
(setq kill-whole-line t)
(setq-default fill-column 70)
(setq-default truncate-lines t)
(setq case-fold-search t)  ;; Search ignores case by default.
(setq-default case-fold-search t)


;; Search
(setq-default isearch-lazy-highlight-initial-delay 0)


;; Safety
(setq confirm-kill-emacs 'yes-or-no-p)
(setq backup-directory-alist `(("." . "~/.emacs.backups")))


;; Describe, help
(setq help-window-select t)  ;; Focus describe buffers on load.


;; Misc
(setq x-select-enable-clipboard t)  ;; Allows you to copy into the system clipboard.


;; Theme
;; (load-theme 'doom-monokai-pro t)
(load-theme 'doom-dracula t)
;; (load-theme 'doom-material t)
;; (load-theme 'doom-oceanic-next t)
;; (set-face-attribute 'default nil :font "dejavu sans mono-10")
;; (set-face-attribute 'default nil :font "SFMono-10")
(set-face-attribute 'default nil :font "sourcecodepro-10")
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#999")))e))


;; Filename patterns
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . javascript-mode))


;; Packages, modes
(setq uniquify-buffer-name-style 'forward)
(dired-async-mode t)
(ido-mode t)
(show-paren-mode t)
(setq show-paren-delay 0)
(electric-pair-mode 0)
(autopair-global-mode t)
(setq highlight-indent-guides-method 'bitmap)
(yas-global-mode t)

;; Package / auto-complete
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0)

;; Package / prism
(custom-set-variables
 '(prism-colors '("#1fceff" "#ff6bc4" "#ffc738" "#f95624"))
 '(prism-desaturations '(10 15 20))
 '(prism-lightens '(0 5 10))
 '(prism-comments nil)
 '(prism-strings nil)
 '(prism-level-1-strings ((t (:foreground "#999")))))
