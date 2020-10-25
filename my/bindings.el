;; KEYBINDINGS


;; Text navigation, selection
(global-set-key "\C-\\" 'my-match-paren)
(global-set-key (kbd "M-.") 'my-mark-current-word)


;; Editing
(global-set-key (kbd "M-i") (lambda () (interactive) (my-indent-region 1)))
(global-set-key (kbd "M-I") (lambda () (interactive) (my-indent-region -1)))
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-_") 'delete-indentation)
(global-set-key (kbd "C-o") 'my-open-line)
(global-set-key (kbd "M-D") 'my-duplicate-line)
(global-set-key (kbd "M-T") 'transpose-lines)
(global-set-key (kbd "C-;") 'my-comment-line)
(global-set-key [\S-insert] 'clipboard-yank)


;;; Commands
(global-set-key (kbd "C-.") 'repeat)


;; Auto-complete
(global-set-key (kbd "M-/") 'hippie-expand)


;; Search, replace, regexp
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-r") 'query-replace-regexp)
(global-set-key (kbd "M-S") 'rgrep)


;;; Buffers
(global-set-key (kbd "M-SPC") 'ido-switch-buffer)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-`") 'dired-jump)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-,") 'my-switch-to-other-buffer)
(global-set-key (kbd "C-x C-o") 'other-window)


;;; Files
(global-set-key (kbd "M-F") 'find-file-other-window)


;; Windows
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)


;; git, magit
(global-set-key (kbd "<home>") 'magit-status)
(global-set-key (kbd "<f5>") 'magit-file-checkout)


;; Emacs, elisp
(global-set-key (kbd "C-=") 'my-eval)


;; Misc
(global-set-key [mouse-2] 'nil) ; Disable middle mouse button.
