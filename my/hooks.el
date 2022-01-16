;; Mode hooks


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'eval-expression-minibuffer-setup-hook
          (lambda () (paredit-mode t)))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            (put 'dired-find-alternate-file 'disabled nil)
            ;; (local-set-key (kbd "P") 'dired-up-directory)  ;; This spawns a new buffer each time.
            (define-key dired-mode-map (kbd "P")
              (lambda () (interactive) (find-alternate-file "..")))
            (local-set-key (kbd "/") 'dired-mark-files-regexp)
            (local-set-key (kbd "C-c C-c") 'dired-toggle-read-only)
            (local-set-key (kbd "C-+") 'dired-create-empty-file)
            (local-set-key (kbd "<return>") 'dired-find-alternate-file)
            (local-set-key (kbd "F") 'find-name-dired)
            (local-set-key (kbd "I") 'dired-do-isearch-regexp)
            (local-set-key (kbd "M-r") 'dired-do-query-replace-regexp)))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (local-set-key (kbd "I") 'ibuffer-do-isearch-regexp)
            (local-set-key (kbd "M-r") 'ibuffer-do-query-replace-regexp)))

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 0)
            (highlight-indent-guides-mode t)
            (git-gutter-mode t)
            (key-chord-define-local "kt" 'my-transpose-lines)
            (key-chord-define-local "qq" 'my-indent-block)
            (key-chord-define-local "zz" 'zap-up-to-char)))

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)
            ;; (lsp)
            ;; (lsp-mode t)
            ;; (local-set-key (kbd "M-.") 'lsp-find-definition)
            ;; (local-set-key (kbd "M-,") 'lsp-find-implementation)
            ;; (local-set-key (kbd "M-'") 'lsp-rename)
            ))

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
            (local-set-key (kbd "M-9") 'my-prev-sexp)
            (local-set-key (kbd "C-c C-s") 'paredit-splice-sexp)
            (local-set-key (kbd "C-c C-o") 'paredit-raise-sexp)))

;; Paredit binds keys in an unconventional way which necessitates this workaround.
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-s") 'save-buffer)
     (define-key paredit-mode-map (kbd "M-r") 'query-replace-regexp)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-,") 'my-switch-to-other-buffer)
            (local-set-key (kbd "C-o") 'open-line)
            (local-set-key (kbd "M--") 'org-meta-return)
            (local-set-key (kbd "C-M--") 'org-insert-todo-heading)
            (local-set-key (kbd "M-p") 'org-previous-visible-heading)
            (local-set-key (kbd "M-n") 'org-next-visible-heading)))

(add-hook 'magit-mode-hook
          (lambda ()
            (add-to-list 'magit-no-confirm 'stage-all-changes)
            (add-to-list 'magit-no-confirm 'unstage-all-changes)))
