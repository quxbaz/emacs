(add-hook 'eval-expression-minibuffer-setup-hook
  (lambda ()
    (insert-parentheses)))

(add-hook 'less-css-mode-hook
  (lambda ()
    (setq css-indent-offset 2)))

(add-hook 'css-mode-hook
  (lambda ()
    ;; (local-set-key (kbd "C-j")
    ;;   (lambda ()
    ;;     (interactive)
    ;;     (setq cssm-indent-level 2)
    ;;     (newline-and-indent)))
    (setq cssm-indent-level 2)
    ;; (wrap-region-mode t)
    (rainbow-mode t)
    (setq css-indent-offset 2)))

(add-hook 'html-mode-hook
  (lambda ()
    ;; (rainbow-mode t)
    ;; (tidy-build-menu html-mode-map)
    (local-set-key (kbd "C-c C-c") 'tidy-buffer)
    (local-set-key (kbd "M-n") 'sgml-skip-tag-forward)
    (local-set-key (kbd "M-p") 'sgml-skip-tag-backward)
    (setq sgml-validate-command "tidy")
    ;; (sgml-guess-indent)
    (setq sgml-basic-offset 2)
    ;; (wrap-region-mode t)
    ))

; (add-hook 'cua-selection-mode-hook
          ; (lambda ()
            ; (local-set-key (kbd "C-g")
                           ; (lambda ()
                             ; (keyboard-quit)
                             ; (cua-selection-mode nil)))))

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(add-hook 'js-mode-hook
  (lambda ()
    ;; (local-set-key (kbd "C-j")
    ;;   (lambda () (interactive) (newline)))
    (local-set-key (kbd "C-j")
      (lambda () (interactive) (newline-and-indent)))
    (auto-complete-mode t)
    (setq js-indent-level 2)
    (yas-minor-mode t)
    (electric-indent-mode -1)
    ))

(add-hook 'org-mode-hook
  (lambda ()
    (local-set-key (kbd "C-,") 'my-switch-to-other-buffer)
    (local-set-key (kbd "C-o") 'open-line)))

(add-hook 'paredit-mode-hook
  (lambda () (local-set-key (kbd "C-w") 'paredit-backward-kill-word)))

(add-hook 'python-mode-hook
  (lambda ()
    (setq python-indent 2)
    ;; (pysmell-mode 1)
    ;; (wrap-region-mode t)
    ))

(add-hook 'fundamental-mode-hook
  (lambda () (local-set-key (kbd "C-o") 'open-line)))

(add-hook 'shell-mode-hook
  (ansi-color-for-comint-mode-on))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
