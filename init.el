(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(load-file "~/.emacs.d/my/conf.el")
(load-file "~/.emacs.d/my/defuns.el")
(load-file "~/.emacs.d/my/bindings.el")
(load-file "~/.emacs.d/my/hooks.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(key-chord prism auto-complete lsp-mode autopair paredit highlight-indent-guides yasnippet magit doom-themes autothemer)))
