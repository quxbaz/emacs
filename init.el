;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Dependencies
(require 's)
(require 'dash)

;; Extensions
(load-file (concat user-emacs-directory "ext/show-point-mode.el"))

;; Local packages
;; Load maf package if it exists.
;; (let ((maf-path (concat user-emacs-directory "site-lisp/maf")))
;;   (when (file-exists-p (concat maf-path "/maf.el"))
;;     (add-to-list 'load-path maf-path)
;;     (require 'maf)))

;; Load wire package if it exists.
(let ((wire-path (concat user-emacs-directory "site-lisp/wire")))
  (when (file-exists-p (concat wire-path "/wire.el"))
    (add-to-list 'load-path wire-path)
    (autoload 'wire-mode "wire" "Wire annotated regions to a running Claude instance." t)
    (autoload 'wire-dispatch "wire" nil t)
    (autoload 'wire-select-target "wire" nil t)
    (autoload 'wire-list-instances "wire" nil t)
    (autoload 'wire-doctor "wire" nil t)
    (autoload 'global-wire-mode "wire" nil t)
    (global-wire-mode 1)))

;; Load tutor package if it exists.
(let ((tutor-path (concat user-emacs-directory "site-lisp/tutor")))
  (when (file-exists-p (concat tutor-path "/tutor.el"))
    (add-to-list 'load-path tutor-path)
    (autoload 'tutor-load "tutor" "Load and play a tutorial script." t)
    (autoload 'tutor-start "tutor" nil t)))

;; Custom config
(load-file (concat user-emacs-directory "my/data.el"))
(load-file (concat user-emacs-directory "my/conf.el"))
(load-file (concat user-emacs-directory "my/theme.el"))
;; Libraries, helpers, utilities
(load-file (concat user-emacs-directory "my/lib-string.el"))
(load-file (concat user-emacs-directory "my/lib-text.el"))
(load-file (concat user-emacs-directory "my/lib-js.el"))
(load-file (concat user-emacs-directory "my/util.el"))
;; Commands
(load-file (concat user-emacs-directory "my/commands.el"))
;; Lisp
(load-file (concat user-emacs-directory "my/lisp/lib.el"))
(load-file (concat user-emacs-directory "my/lisp/commands.el"))
(load-file (concat user-emacs-directory "my/lisp/conf.el"))
(load-file (concat user-emacs-directory "my/lisp/bindings.el"))
(load-file (concat user-emacs-directory "my/lisp/hooks.el"))
;; Bindings
(load-file (concat user-emacs-directory "my/bindings.el"))
(load-file (concat user-emacs-directory "my/mode-bindings.el"))
(load-file (concat user-emacs-directory "my/kmacros.el"))
;; Hooks
(load-file (concat user-emacs-directory "my/hooks.el"))
;; Calc
(load-file (concat user-emacs-directory "my/calc/index.el"))

;; Autoloads
(autoload 'sql-lisp-mode (concat user-emacs-directory "my/lib-sql.el") "A mode for SQL interaction through evaluation of Emacs Lisp forms." t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(package-selected-packages
   '(
     aggressive-indent
     autopair
     corfu
     deadgrep
     diff-hl
     doom-themes
     highlight-indent-guides
     ivy
     lua-mode
     magit
     markdown-mode
     modus-themes
     paredit
     posframe
     rainbow-blocks
     slime
     web-mode
     yasnippet
     )))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-section-highlight ((t (:inherit hl-line :background "blue"))))
 '(org-ellipsis ((t (:foreground "gray50")))))
