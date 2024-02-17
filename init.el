(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(load-file "~/.emacs.d/my/conf.el")
(load-file "~/.emacs.d/my/theme.el")
(load-file "~/.emacs.d/my/macros.el")
(load-file "~/.emacs.d/my/util-defuns.el")
(load-file "~/.emacs.d/my/defuns.el")
(load-file "~/.emacs.d/my/lisp-defuns.el")
(load-file "~/.emacs.d/my/bindings.el")
(load-file "~/.emacs.d/my/kmacros.el")
(load-file "~/.emacs.d/my/hooks.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(package-selected-packages
   '(
     auto-complete
     autopair
     autothemer
     deadgrep
     doom-themes
     eros
     git-gutter
     highlight-indent-guides
     lsp-mode
     magit
     modus-themes
     paredit
     php-mode
     prism
     rainbow-blocks
     web-mode
     yasnippet
     )))
