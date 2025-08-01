;; Mode bindings


(keymap-set help-mode-map "<mouse-8>" 'help-go-back)
(keymap-set help-mode-map "<mouse-9>" 'help-go-forward)
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

(add-hook 'css-mode-hook
          (lambda ()
            (keymap-set css-mode-map "M-q" 'my/indent-block)))

(eval-after-load 'deadgrep
  '(progn
     (keymap-set deadgrep-mode-map "C-o" 'my/deadgrep-display-result)
     (keymap-set deadgrep-mode-map "N" 'my/deadgrep-display-next-result)
     (keymap-set deadgrep-mode-map "P" 'my/deadgrep-display-prev-result)))

(add-hook 'dired-mode-hook
          (lambda ()
            (keymap-set dired-mode-map "P" 'my/dired-up-directory)
            (keymap-set dired-mode-map "/" 'isearch-forward-regexp)
            (keymap-set dired-mode-map "M" 'dired-mark-files-regexp)
            (keymap-set dired-mode-map "C-c C-c" 'dired-toggle-read-only)
            (keymap-set dired-mode-map "r" 'my/dired-do-rename-here)
            (keymap-set dired-mode-map "+" 'dired-create-empty-file)
            (keymap-set dired-mode-map "C-+" 'dired-create-directory)
            (keymap-set dired-mode-map "<return>" 'dired-find-alternate-file)
            (keymap-set dired-mode-map "I" 'dired-do-isearch-regexp)
            (keymap-set dired-mode-map "M-r" 'dired-do-query-replace-regexp)
            (keymap-set dired-mode-map "F" 'my/find-regex-dired)
            (keymap-set dired-mode-map "N" 'my/find-name-dired)
            (keymap-set dired-mode-map "J" 'my/find-jsx)
            (keymap-set dired-mode-map "E" 'my/dired-context-edit-file)
            (keymap-set dired-mode-map "G" 'my/dired-open-gallery)
            (keymap-set dired-mode-map "C-u C-r" 'my/dired-resize-image)))

(add-hook 'wdired-mode-hook
          (lambda ()
            (local-set-key (kbd "C-a") (lambda () (interactive) (move-to-column 2)))
            (local-set-key (kbd "M-m") (lambda () (interactive) (move-to-column 2)))))

(eval-after-load 'ibuffer
  '(progn
     (keymap-set ibuffer-mode-map "M-r" 'ibuffer-do-query-replace-regexp)
     (keymap-set ibuffer-mode-map "/ ?" 'ibuffer-filter-by-predicate)
     (keymap-set ibuffer-mode-map "/ e" 'ibuffer-filter-by-file-extension)
     (keymap-set ibuffer-mode-map "/ q" 'ibuffer-pop-filter)))

(eval-after-load 'js
  '(progn
     ;; (local-set-key (kbd "M-.") 'lsp-find-definition)
     ;; (local-set-key (kbd "M-,") 'lsp-find-implementation)
     ;; (local-set-key (kbd "M-'") 'lsp-rename)
     (keymap-set js-mode-map "M-q" 'my/indent-block)
     (keymap-set js-mode-map "M-." 'my/mark-context)
     (keymap-set js-mode-map "C-c C-/" 'my/toggle-jsx-comment)
     (keymap-set js-mode-map "C-c /" 'my/close-html-tag)))

(eval-after-load 'magit
  '(progn
     (keymap-set magit-mode-map "w" 'my/key-w)
     (keymap-set magit-mode-map "C-o" 'magit-diff-visit-file-other-window)))

(eval-after-load 'org
  '(progn
     (keymap-set org-mode-map "M-a" 'org-backward-element)
     (keymap-set org-mode-map "M-e" 'org-forward-element)
     (keymap-set org-mode-map "M-p" 'org-previous-visible-heading)
     (keymap-set org-mode-map "M-n" 'org-next-visible-heading)
     ;; (keymap-set org-mode-map "M--" 'org-meta-return)
     (keymap-set org-mode-map "M-j" 'org-meta-return)
     (keymap-set org-mode-map "C-j" 'org-newline-and-indent)
     (keymap-set org-mode-map "|" 'my/key-pipe)
     (keymap-set org-mode-map "C-o" 'my/open-line)
     (keymap-set org-mode-map "M-<return>" 'my/duplicate-dwim)
     (keymap-set org-mode-map "M-q" 'fill-paragraph)
     (keymap-set org-mode-map "C-," 'my/switch-to-other-buffer)
     (keymap-set org-mode-map "C-c C-v" 'my/revert-buffer)
     (keymap-set org-mode-map "C-M--" 'org-insert-todo-heading)
     (keymap-set org-mode-map "C-M-." 'my/org-table-mark-field)
     (keymap-set org-mode-map "C-c C-y" 'my/org-evaluate-time-range)
     (keymap-set org-mode-map "C-C C-," 'org-agenda)))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local my/sh-mode-map (make-sparse-keymap))
            (define-key my/sh-mode-map (kbd "M-a") 'backward-sentence)
            (define-key my/sh-mode-map (kbd "M-e") 'forward-sentence)
            (use-local-map my/sh-mode-map)))

(add-hook 'web-mode-hook
          (lambda ()
            (keymap-set web-mode-map "M-/" 'hippie-expand)
            (keymap-set web-mode-map "C-M-i" 'completion-at-point)
            (keymap-set web-mode-map "M-q" 'my/indent-block)
            (keymap-set web-mode-map "C-c /" (kmacro "< > C-b / C-f"))))  ;; Close tag.
