(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Insert parentheses automatically when going into eval buffer.
(add-hook 'eval-expression-minibuffer-setup-hook
  (lambda ()
    (insert-parentheses)))

(add-hook 'css-mode-hook
  (lambda ()
    (setq cssm-indent-level 2)
    (setq css-indent-offset 2)
    (rainbow-mode t)))

(add-hook 'js-mode-hook
  (lambda ()
    ;; (electric-indent-mode -1)
    (setq js-indent-level 2)
    (auto-complete-mode t)))

(add-hook 'org-mode-hook
  (lambda ()
    (local-set-key (kbd "C-,") 'my-switch-to-other-buffer)
    (local-set-key (kbd "C-o") 'open-line)))

;; (add-hook 'autopair-mode-hook
;;   #'(lambda ()
;;       (push '(?{ . ?})
;;             (getf autopair-extra-pairs :code))))
