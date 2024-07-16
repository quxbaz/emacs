(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Extensions
(load-file (concat user-emacs-directory "ext/show-point-mode.el"))

;; Custom config
(load-file (concat user-emacs-directory "my/conf.el"))
(load-file (concat user-emacs-directory "my/theme.el"))
(load-file (concat user-emacs-directory "my/macros.el"))
(load-file (concat user-emacs-directory "my/util-defuns.el"))
(load-file (concat user-emacs-directory "my/defuns.el"))
(load-file (concat user-emacs-directory "my/lisp-defuns.el"))
(load-file (concat user-emacs-directory "my/bindings.el"))
(load-file (concat user-emacs-directory "my/mode-bindings.el"))
(load-file (concat user-emacs-directory "my/kmacros.el"))
(load-file (concat user-emacs-directory "my/hooks.el"))
(load-file (concat user-emacs-directory "my/macos.el"))

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
     autothemer
     corfu
     deadgrep
     doom-themes
     git-gutter
     highlight-indent-guides
     ivy
     lsp-mode
     magit
     modus-themes
     paredit
     php-mode
     rainbow-blocks
     web-mode
     yasnippet
     )))
