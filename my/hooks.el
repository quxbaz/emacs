;; Mode hooks


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'eval-expression-minibuffer-setup-hook
          (lambda () (paredit-mode t)))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            (put 'dired-find-alternate-file 'disabled nil)
            (when (file-remote-p default-directory)
              (setq dired-listing-switches "-l"))
            ;; (local-set-key (kbd "P") 'dired-up-directory)  ;; This spawns a new buffer each time.
            (define-key dired-mode-map (kbd "P") (lambda () (interactive) (find-alternate-file "..")))
            (local-set-key (kbd "/") 'isearch-forward-regexp)
            (local-set-key (kbd "M") 'dired-mark-files-regexp)
            (local-set-key (kbd "C-c C-c") 'dired-toggle-read-only)
            (local-set-key (kbd "+") 'dired-create-empty-file)
            (local-set-key (kbd "C-+") 'dired-create-directory)
            (local-set-key (kbd "<return>") 'dired-find-alternate-file)
            (local-set-key (kbd "F") 'my/find-dired)
            (local-set-key (kbd "V") 'my/find-string-dired)
            (local-set-key (kbd "J") 'my/find-jsx)
            (local-set-key (kbd "I") 'dired-do-isearch-regexp)
            (local-set-key (kbd "M-r") 'dired-do-query-replace-regexp)))

(add-hook 'wdired-mode-hook
          (lambda ()
            (local-set-key (kbd "M-m") (lambda () (interactive) (move-to-column 2)))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (local-set-key (kbd "I") 'ibuffer-do-isearch-regexp)
            (local-set-key (kbd "M-r") 'ibuffer-do-query-replace-regexp)))

(add-hook 'occur-mode-hook
          (lambda ()
            (local-set-key (kbd "p") (lambda () (interactive) (occur-prev) (occur-mode-display-occurrence)))
            (local-set-key (kbd "n") (lambda () (interactive) (occur-next) (occur-mode-display-occurrence)))))

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 0)
            (git-gutter-mode t)))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local my/local-map (make-sparse-keymap))
            (define-key my/local-map (kbd "M-a") 'backward-sentence)
            (define-key my/local-map (kbd "M-e") 'forward-sentence)
            (use-local-map my/local-map)))

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)
            (setq-local require-final-newline nil)
            (local-set-key (kbd "M-.") 'my/mark-current-word)
            (local-set-key (kbd "C-c C-/") 'my/toggle-jsx-comment)
            (local-set-key (kbd "C-c /") 'my/close-html-tag)
            ;; (lsp)
            ;; (lsp-mode t)
            ;; (local-set-key (kbd "M-.") 'lsp-find-definition)
            ;; (local-set-key (kbd "M-,") 'lsp-find-implementation)
            ;; (local-set-key (kbd "M-'") 'lsp-rename)
            ))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

(add-hook 'html-mode-hook
          (lambda ()
            (local-set-key (kbd "M-o") 'other-window)))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-enable-auto-indentation nil)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-css-indent-offset 2)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (autopair-mode 0)
            (paredit-mode t)
            ;; (prism-mode t)
            (rainbow-blocks-mode t)
            (local-set-key (kbd "M-/") 'dabbrev-expand)
            (local-set-key (kbd "C-c C-c") 'my/eval-dwim)
            (local-set-key (kbd "M-9") 'my/prev-sexp)
            (local-set-key (kbd "M-0") 'my/next-sexp)
            (local-set-key (kbd "M-k") 'my/kill-sexp)
            (local-set-key (kbd "C-c C-s") 'paredit-splice-sexp)
            (local-set-key (kbd "C-c C-o") 'paredit-raise-sexp)))

;; Paredit binds keys in an unconventional way which necessitates the use of define-key.
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-s") 'save-buffer)
     (define-key paredit-mode-map (kbd "M-r") 'query-replace-regexp)
     (define-key paredit-mode-map (kbd "M-<up>") 'my/transpose-line)
     (define-key paredit-mode-map (kbd "M-<down>") (lambda () (interactive) (my/transpose-line t)))
     (define-key paredit-mode-map (kbd "M-)") 'my/open-new-round)))

(add-hook 'org-mode-hook
          (lambda ()
            (git-gutter-mode t)
            (local-set-key (kbd "M--") 'org-meta-return)
            (local-set-key (kbd "C-j") 'org-newline-and-indent)
            (local-set-key (kbd "C-o") 'open-line)
            (local-set-key (kbd "M-<return>") 'my/duplicate-line)
            (local-set-key (kbd "M-q") 'fill-paragraph)
            (local-set-key (kbd "C-,") 'my/switch-to-other-buffer)
            (local-set-key (kbd "C-c C-v") 'my/revert-buffer)
            (local-set-key (kbd "M-p") 'org-previous-visible-heading)
            (local-set-key (kbd "M-n") 'org-next-visible-heading)
            (local-set-key (kbd "C-M--") 'org-insert-todo-heading)
            (local-set-key (kbd "C-M-.") 'my/org-table-mark-field)
            (outline-show-all)))

(add-hook 'magit-mode-hook
          (lambda ()
            (setq magit-save-repository-buffers nil)
            (add-to-list 'magit-no-confirm 'stage-all-changes)
            (add-to-list 'magit-no-confirm 'unstage-all-changes)
            (local-set-key (kbd "C-o") 'magit-diff-visit-file-other-window)))

(add-hook 'calendar-mode-hook
          (lambda ()
            (diary-mark-entries)
            (local-set-key (kbd "s-d") 'calendar-exit)
            (local-set-key (kbd "l") 'calendar-forward-day)
            (local-set-key (kbd "h") 'calendar-backward-day)
            (local-set-key (kbd "j") 'calendar-forward-week)
            (local-set-key (kbd "k") 'calendar-backward-week)))
