;; Mode hooks


(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Insert parentheses automatically when going into eval buffer.
(add-hook 'eval-expression-minibuffer-setup-hook
          (lambda () (insert-parentheses)))

(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-indent-guides-mode t)))

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)
            (auto-complete-mode t)))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (setq cssm-indent-level 2)
            (rainbow-mode t)))

;; (add-hook 'emacs-lisp-mode-hook (lambda ()))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-,") 'my-switch-to-other-buffer)
            (local-set-key (kbd "C-o") 'open-line)))
