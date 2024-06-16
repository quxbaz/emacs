;; Mode bindings


(keymap-set help-mode-map "<mouse-8>" 'help-go-back)
(keymap-set help-mode-map "<mouse-9>" 'help-go-forward)
(add-hook 'html-mode-hook (lambda () (local-set-key (kbd "M-o") 'other-window)))
(eval-after-load 'ibuffer '(keymap-set ibuffer-mode-map "M-r" 'ibuffer-do-query-replace-regexp))
(keymap-set minibuffer-mode-map "M-/" 'dabbrev-expand)
(keymap-set occur-mode-map "n" (lambda () (interactive) (occur-next) (occur-mode-display-occurrence)))
(keymap-set occur-mode-map "p" (lambda () (interactive) (occur-prev) (occur-mode-display-occurrence)))
(keymap-set text-mode-map "M-/" 'dabbrev-expand)

(eval-after-load 'calendar
  '(progn
     (keymap-set calendar-mode-map "s-d" 'calendar-exit)
     (keymap-set calendar-mode-map "<f12>" 'calendar-exit)
     (keymap-set calendar-mode-map "l" 'calendar-forward-day)
     (keymap-set calendar-mode-map "h" 'calendar-backward-day)
     (keymap-set calendar-mode-map "j" 'calendar-forward-week)
     (keymap-set calendar-mode-map "k" 'calendar-backward-week)))

(eval-after-load 'dired
  '(progn
     (keymap-set dired-mode-map "M-`" 'my/dired-to-emacs-d)
     (keymap-set dired-mode-map "P" 'my/dired-up-directory)
     (keymap-set dired-mode-map "/" 'isearch-forward-regexp)
     (keymap-set dired-mode-map "M" 'dired-mark-files-regexp)
     (keymap-set dired-mode-map "C-c C-c" 'dired-toggle-read-only)
     (keymap-set dired-mode-map "+" 'dired-create-empty-file)
     (keymap-set dired-mode-map "C-+" 'dired-create-directory)
     (keymap-set dired-mode-map "<return>" 'dired-find-alternate-file)
     (keymap-set dired-mode-map "I" 'dired-do-isearch-regexp)
     (keymap-set dired-mode-map "M-r" 'dired-do-query-replace-regexp)
     (keymap-set dired-mode-map "F" 'my/find-regex-dired)
     (keymap-set dired-mode-map "J" 'my/find-jsx)))

(add-hook 'wdired-mode-hook
          (lambda ()
            (local-set-key (kbd "C-a") (lambda () (interactive) (move-to-column 2)))
            (local-set-key (kbd "M-m") (lambda () (interactive) (move-to-column 2)))))

(keymap-set emacs-lisp-mode-map "M-<return>" 'my/duplicate-list)
(keymap-set emacs-lisp-mode-map "M-/" 'completion-at-point)
(keymap-set emacs-lisp-mode-map "C-M-i" 'dabbrev-expand)
(keymap-set emacs-lisp-mode-map "M-n" 'my/lisp-forward-sexp)
(keymap-set emacs-lisp-mode-map "M-p" 'backward-sexp)
(keymap-set emacs-lisp-mode-map "C-c C-c" 'my/eval-dwim)
(keymap-set emacs-lisp-mode-map "C-c C-." 'my/eval-here)
(keymap-set emacs-lisp-mode-map "C-c C-y" 'my/eval-kill-ring)
(keymap-set emacs-lisp-mode-map "M-w" 'my/lisp-kill-ring-save-dwim)
(keymap-set emacs-lisp-mode-map "C-k" 'my/lisp-kill-dwim)
(keymap-set emacs-lisp-mode-map "M-k" 'my/kill-list)
(keymap-set emacs-lisp-mode-map "M-0" 'my/append-new-round)
(keymap-set emacs-lisp-mode-map "M-9"  'my/insert-new-round)
(keymap-set emacs-lisp-mode-map "M-("  'my/wrap-sexp)
(keymap-set emacs-lisp-mode-map "C-c C-s" 'paredit-splice-sexp)
(keymap-set emacs-lisp-mode-map "C-c C-o" 'paredit-raise-sexp)

(eval-after-load 'js
  '(progn
     ;; (local-set-key (kbd "M-.") 'lsp-find-definition)
     ;; (local-set-key (kbd "M-,") 'lsp-find-implementation)
     ;; (local-set-key (kbd "M-'") 'lsp-rename)
     (keymap-set js-mode-map "M-." 'my/mark-context)
     (keymap-set js-mode-map "C-c C-/" 'my/toggle-jsx-comment)
     (keymap-set js-mode-map "C-c /" 'my/close-html-tag)))

(eval-after-load 'magit
  '(progn
     (keymap-set magit-mode-map "w" 'my/key-w)
     (keymap-set magit-mode-map "C-o" 'magit-diff-visit-file-other-window)))

(eval-after-load 'org
  '(progn
     (keymap-set org-mode-map "M--" 'org-meta-return)
     (keymap-set org-mode-map "M--" 'org-meta-return)
     (keymap-set org-mode-map "C-j" 'org-newline-and-indent)
     (keymap-set org-mode-map "C-o" 'open-line)
     (keymap-set org-mode-map "M-<return>" 'my/duplicate-dwim)
     (keymap-set org-mode-map "M-<up>" 'my/transpose-line)
     (keymap-set org-mode-map "M-<down>" (lambda () (interactive) (my/transpose-line t)))
     (keymap-set org-mode-map "M-q" 'fill-paragraph)
     (keymap-set org-mode-map "C-," 'my/switch-to-other-buffer)
     (keymap-set org-mode-map "C-c C-v" 'my/revert-buffer)
     (keymap-set org-mode-map "M-p" 'org-previous-visible-heading)
     (keymap-set org-mode-map "M-n" 'org-next-visible-heading)
     (keymap-set org-mode-map "C-M--" 'org-insert-todo-heading)
     (keymap-set org-mode-map "C-M-." 'my/org-table-mark-field)))

(eval-after-load 'paredit
  '(progn
     (keymap-set paredit-mode-map "M-s" 'save-buffer)
     (keymap-set paredit-mode-map "M-r" 'my/query-replace-dwim)
     (keymap-set paredit-mode-map "M-;" 'my/comment-block)
     (keymap-set paredit-mode-map "M-<up>" 'my/transpose-line)
     (keymap-set paredit-mode-map "M-<down>" 'my/transpose-line-down)
     (keymap-set paredit-mode-map "C-k" 'my/lisp-kill-dwim)
     (keymap-set paredit-mode-map "M-)" 'my/open-new-round)
     (keymap-set paredit-mode-map "M-(" 'my/wrap-sexp)))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local my/sh-mode-map (make-sparse-keymap))
            (define-key my/sh-mode-map (kbd "M-a") 'backward-sentence)
            (define-key my/sh-mode-map (kbd "M-e") 'forward-sentence)
            (use-local-map my/sh-mode-map)))

(add-hook 'web-mode-hook
          (lambda ()
            (keymap-set web-mode-map "M-/" 'hippie-expand)
            (keymap-set web-mode-map "C-M-i" 'completion-at-point)))
