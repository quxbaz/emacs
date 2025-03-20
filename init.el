;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Dependencies
(require 's)

;; Extensions
(load-file (concat user-emacs-directory "ext/show-point-mode.el"))

;; Custom config
(load-file (concat user-emacs-directory "my/data.el"))
(load-file (concat user-emacs-directory "my/conf.el"))
(load-file (concat user-emacs-directory "my/theme.el"))
(load-file (concat user-emacs-directory "my/lib-text.el"))
(load-file (concat user-emacs-directory "my/lib-lisp.el"))
(load-file (concat user-emacs-directory "my/lib-js.el"))
(load-file (concat user-emacs-directory "my/util.el"))
(load-file (concat user-emacs-directory "my/commands.el"))
(load-file (concat user-emacs-directory "my/lisp-commands.el"))
(load-file (concat user-emacs-directory "my/lisp-conf.el"))
(load-file (concat user-emacs-directory "my/bindings.el"))
(load-file (concat user-emacs-directory "my/mode-bindings.el"))
(load-file (concat user-emacs-directory "my/kmacros.el"))
(load-file (concat user-emacs-directory "my/hooks.el"))
(load-file (concat user-emacs-directory "my/lisp-conf.el"))

;; Autoloads
(autoload 'sql-lisp-mode (concat user-emacs-directory "my/lib-sql.el") "A mode for SQL interaction through evaluation of Emacs Lisp forms." t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(ibuffer-saved-filter-groups
   '(("filters"
      ("Todos" (filename . "todo.org"))
      ("resist" (not starred-name) (not name . "magit") (not used-mode . dired-mode) (directory . "/resist/")))))
 '(ibuffer-saved-filters
   '(("programming" (or (derived-mode . prog-mode) (mode . ess-mode) (mode . compilation-mode)))
     ("text document" (and (derived-mode . text-mode) (not (starred-name))))
     ("TeX" (or (derived-mode . tex-mode) (mode . latex-mode) (mode . context-mode)
                (mode . ams-tex-mode) (mode . bibtex-mode)))
     ("web" (or (derived-mode . sgml-mode) (derived-mode . css-base-mode)
                (derived-mode . js-base-mode) (derived-mode . typescript-ts-base-mode)
                (mode . js2-mode) (derived-mode . haml-mode) (mode . sass-mode)))
     ("gnus" (or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode)
                 (mode . gnus-summary-mode) (mode . gnus-article-mode)))))
 '(package-selected-packages
   '(
     aggressive-indent
     autopair
     corfu
     deadgrep
     doom-themes
     git-gutter
     highlight-indent-guides
     ivy
     magit
     modus-themes
     paredit
     rainbow-blocks
     slime
     web-mode
     yasnippet
     )))
