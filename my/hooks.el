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
            (local-set-key (kbd "/") 'isearch-forward-regexp)
            (local-set-key (kbd "M") 'dired-mark-files-regexp)
            (local-set-key (kbd "C-c C-c") 'dired-toggle-read-only)
            (local-set-key (kbd "C-+") 'dired-create-empty-file)
            (local-set-key (kbd "<return>") 'dired-find-alternate-file)
            (local-set-key (kbd "F") 'my-find-dired)
            (local-set-key (kbd "V") 'my-find-string-dired)
            (local-set-key (kbd "J") 'my-find-jsx)
            (local-set-key (kbd "I") 'dired-do-isearch-regexp)
            (local-set-key (kbd "M-r") 'dired-do-query-replace-regexp)
            (when (file-remote-p default-directory)
              (setq dired-listing-switches "-l"))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (local-set-key (kbd "I") 'ibuffer-do-isearch-regexp)
            (local-set-key (kbd "M-r") 'ibuffer-do-query-replace-regexp)))

(add-hook 'occur-mode-hook
          (lambda ()
            (local-set-key (kbd "n") 'occur-next)
            (local-set-key (kbd "p") 'occur-prev)))

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 0)
            (highlight-indent-guides-mode t)
            (git-gutter-mode t)
            (key-chord-define-local "kt" 'my-transpose-lines)
            (key-chord-define-local "qq" 'fill-paragraph)
            (key-chord-define-local "zz" 'zap-up-to-char)))

(add-hook 'js-mode-hook
          (lambda ()
            (setq-local require-final-newline nil)
            (setq js-indent-level 2)
            (local-set-key (kbd "C-c C-/") 'my-toggle-jsx-comment)
            (local-set-key (kbd "C-c /") 'my-close-html-tag)
            (lsp)
            (lsp-mode t)
            (local-set-key (kbd "M-.") 'lsp-find-definition)
            (local-set-key (kbd "M-,") 'lsp-find-implementation)
            (local-set-key (kbd "M-'") 'lsp-rename)))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (setq cssm-indent-level 2)))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)))

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
            (local-set-key (kbd "M-n") 'org-next-visible-heading)
            (outline-show-all)))

(add-hook 'magit-mode-hook
          (lambda ()
            (setq magit-save-repository-buffers nil)
            (add-to-list 'magit-no-confirm 'stage-all-changes)
            (add-to-list 'magit-no-confirm 'unstage-all-changes)))
