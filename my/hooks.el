;; Mode hooks


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'eval-expression-minibuffer-setup-hook
          (lambda () (paredit-mode t)))

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 0)
            (highlight-indent-guides-mode t)))

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)
            (lsp)
            (lsp-mode t)
            (local-set-key (kbd "M-.") 'lsp-find-definition)
            (local-set-key (kbd "M-,") 'lsp-find-implementation)
            (local-set-key (kbd "M-'") 'lsp-rename)))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (setq cssm-indent-level 2)
            (rainbow-mode t)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (autopair-mode 0)
            (paredit-mode t)
            (prism-mode t)
            (local-set-key (kbd "M-k") 'my-kill-sexp)
            (local-set-key (kbd "M-0") 'my-next-sexp)
            (local-set-key (kbd "M-9") 'my-prev-sexp)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-,") 'my-switch-to-other-buffer)
            (local-set-key (kbd "C-o") 'open-line)
            (local-set-key (kbd "M-<return>") 'org-meta-return)
            (local-set-key (kbd "M-S-<return>") 'org-insert-todo-heading)))
