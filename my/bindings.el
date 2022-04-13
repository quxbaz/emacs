;; KEYBINDINGS


;; Text navigation, selection
(global-set-key "\C-\\" 'my-match-paren)
(global-set-key (kbd "M-h") 'my-mark-paragraph)
(global-set-key (kbd "M-o") 'my-swap-points)
(global-set-key (kbd "<C-iso-lefttab>") 'my-outline-toggle-all)
(global-set-key (kbd "C-<tab>") 'outline-toggle-children)
(global-set-key (kbd "C-c C-p") 'find-file-at-point)


;; Appearance, themes
(global-set-key (kbd "s-s") 'my-swap-theme-background)


;; Editing
(global-set-key [\S-insert] 'clipboard-yank)
(global-set-key (kbd "C-d") 'my-delete-char)
(global-set-key (kbd "C-k") 'my-kill-line)
(global-set-key (kbd "M-k") 'my-kill-block)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "M-i") 'indent-rigidly-right)
(global-set-key (kbd "M-I") 'indent-rigidly-left)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C--") 'delete-indentation)
(global-set-key (kbd "C-o") 'my-open-line)
(global-set-key (kbd "M-<return>") 'my-duplicate-line)
(global-set-key (kbd "M-S-<return>") 'my-duplicate-block)
(global-set-key (kbd "C-;") 'my-comment-line)
(global-set-key (kbd "M-;") 'my-comment-block)
(global-set-key (kbd "C-<backspace>") 'my-copy-line)
(global-set-key (kbd "s-a") 'align-regexp)
(global-set-key (kbd "s-k") 'my-clear-buffer)
(global-set-key (kbd "C-M-/") 'undo-only)
(global-set-key (kbd "C-x r \\") 'delete-whitespace-rectangle)
(global-set-key (kbd "C-q") 'view-mode)


;; Commands
(global-set-key (kbd "C-.") 'repeat)


;; Auto-complete
(global-set-key (kbd "M-/") 'hippie-expand)


;; Search, replace, regexp, occur
(global-set-key (kbd "C-c C-.") 'search-forward-regexp)
(global-set-key (kbd "C-c C-,") 'search-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-r") 'query-replace-regexp)
(global-set-key (kbd "M-R") 'query-replace-regexp)
(global-set-key (kbd "C-S-s") 'rgrep)
(global-set-key (kbd "C-S-d") 'find-name-dired)
(global-set-key (kbd "C-c o") 'occur)


;; Files
(global-set-key (kbd "M-F") 'find-file-other-window)


;; Buffers
(global-set-key (kbd "M-SPC") 'ido-switch-buffer)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "C-c C-v") 'my-revert-buffer)
(global-set-key (kbd "C-c C-SPC") 'ido-dired)
(global-set-key (kbd "M-`") 'my-dired)
(global-set-key (kbd "M-~") 'my-dired-other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-,") 'my-switch-to-other-buffer)
(global-set-key (kbd "C-x C-o") 'other-window)


;; Windows
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") (lambda () (interactive) (jump-to-register (string-to-char "1"))))
(global-set-key (kbd "M-2") (lambda () (interactive) (jump-to-register (string-to-char "2"))))
(global-set-key (kbd "s-x") 'window-swap-states)


;; Macros
(global-set-key (kbd "C-c C-0") 'kmacro-cycle-ring-next)
(global-set-key (kbd "C-c C-9") 'kmacro-cycle-ring-previous)
(global-set-key (kbd "C-c C-e") 'kmacro-edit-macro)
(global-set-key (kbd "C-c C-a") 'kmacro-step-edit-macro)
(global-set-key (kbd "C-c C-n") 'kmacro-name-last-macro)
(global-set-key (kbd "C-c C-i") 'insert-kbd-macro)


;; git, magit
(global-set-key (kbd "C-M-SPC") 'magit-status)
;; (global-set-key (kbd "<home>") 'magit-status)
;; (global-set-key (kbd "<delete>") 'magit-status)
(global-set-key (kbd "<f5>") 'magit-file-checkout)
(global-set-key (kbd "C-c C-l") 'magit-file-dispatch)
(global-set-key (kbd "C-c C-b") 'magit-blob-mode)


;; Emacs, elisp
(global-set-key (kbd "C-=") 'my-eval)
;; (global-set-key (kbd "S-SPC") 'mark-sexp)


;; Misc
(global-set-key [mouse-2] 'nil) ; Disable middle mouse button.
