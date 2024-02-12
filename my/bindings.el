;; Global keybindings


;; Mouse config
(global-set-key (kbd "M-<down-mouse-1>") 'nil)
(global-set-key (kbd "M-<drag-mouse-1>") 'nil)
(global-set-key (kbd "<mouse-3>") 'nil)
(global-set-key (kbd "M-<down-mouse-3>") 'nil)
(global-set-key (kbd "M-<drag-mouse-3>") 'nil)
(global-set-key [mouse-2] 'nil) ; Disable middle mouse button.


;; Help, info
(global-set-key (kbd "C-h C-a") 'apropos)
(global-set-key (kbd "C-h C-f") 'find-library)


;; Mini-apps
(global-set-key (kbd "s-d") 'calendar)


;; Appearance, themes
;;


;; Text navigation, marking
(global-set-key "\C-\\" 'my/match-paren)
(global-set-key (kbd "M-.") 'my/mark-context)
(global-set-key (kbd "M-h") 'my/mark-paragraph)


;; Jumping
(global-set-key (kbd "M-1") (lambda () (interactive) (jump-to-register (string-to-char "1"))))
(global-set-key (kbd "M-2") (lambda () (interactive) (jump-to-register (string-to-char "2"))))
(global-set-key (kbd "M-3") (lambda () (interactive) (jump-to-register (string-to-char "3"))))
(global-set-key (kbd "M-4") (lambda () (interactive) (jump-to-register (string-to-char "4"))))
(global-set-key (kbd "M-'") 'xref-find-definitions)


;; Editing
;; Kill ring, clipboard, undo
(global-set-key [\S-insert] 'clipboard-yank)
(global-set-key (kbd "C-M-/") 'undo-only)
;; Creation, duplication, opening
(global-set-key (kbd "C-o") 'my/open-line)
(global-set-key (kbd "M-<return>") 'my/duplicate-line)
(global-set-key (kbd "M-S-<return>") 'my/duplicate-block)
;; Deletion, clearing
(global-set-key (kbd "C-d") 'my/delete-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-k") 'my/kill-block)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-M-k") 'my/clear-buffer)
(global-set-key (kbd "C-x r \\") 'delete-whitespace-rectangle)
;; Commenting
(global-set-key (kbd "C-;") 'my/comment-line)
(global-set-key (kbd "M-;") 'my/comment-block)
;; Transposing
(global-set-key (kbd "M-<up>") 'my/transpose-line)
(global-set-key (kbd "M-<down>") (lambda () (interactive) (my/transpose-line t)))
(global-set-key (kbd "M-T") 'transpose-regions)
;; Sorting, alignment
(global-set-key (kbd "C-c C-\\") 'align-regexp)


;; Indentation
(global-set-key (kbd "M-q") 'my/indent-block)
(global-set-key (kbd "M-I") 'indent-rigidly-left)
(global-set-key (kbd "M-i") 'indent-rigidly-right)
(global-set-key (kbd "C--") 'delete-indentation)
(global-set-key (kbd "C-j") 'newline-and-indent)


;; Commands
(global-set-key (kbd "C-.") 'repeat)


;; Auto-complete
(global-set-key (kbd "M-/") 'hippie-expand)


;; Search, replace, regexp, occur
;; Search
(global-set-key (kbd "C-s") 'my/isearch-forward-dwim)
(global-set-key (kbd "C-r") 'my/isearch-backward-dwim)
;; Replace
(global-set-key (kbd "M-r") 'my/query-replace-dwim)
(global-set-key (kbd "M-R") 'my/query-replace-buffer-dwim)
;; Grep, find, occur
(global-set-key (kbd "C-S-s") 'deadgrep)
(global-set-key (kbd "C-c o") 'my/occur-dwim)


;; Files
(global-set-key (kbd "M-F") 'find-file-other-window)
(global-set-key (kbd "C-c p") 'find-file-at-point)


;; Buffers, dired
;; Buffers
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "C-,") 'my/switch-to-other-buffer)
(global-set-key (kbd "M-SPC") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-v") 'my/revert-buffer)
;; Dired
(global-set-key (kbd "C-c C-SPC") 'ido-dired)
(global-set-key (kbd "M-`") 'dired-jump)
(global-set-key (kbd "M-~") 'dired-jump-other-window)


;; Modes, viewing, narrowing, filtering
(global-set-key (kbd "C-q") 'view-mode)
(global-set-key (kbd "C-<tab>") 'outline-toggle-children)
(global-set-key (kbd "<C-iso-lefttab>") 'my/outline-toggle-all)


;; Windows
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "s-x") 'window-swap-states)


;; Macros
(global-set-key (kbd "C-c C-9") 'kmacro-cycle-ring-previous)
(global-set-key (kbd "C-c C-0") 'kmacro-cycle-ring-next)
(global-set-key (kbd "C-c C-e") 'kmacro-edit-macro)
(global-set-key (kbd "C-c C-a") 'kmacro-step-edit-macro)
;; (global-set-key (kbd "C-c C-n") 'kmacro-name-last-macro)
(global-set-key (kbd "C-c C-i") 'insert-kbd-macro)


;; git, magit
(global-set-key (kbd "C-M-SPC") 'magit-status)
(global-set-key (kbd "<f5>") 'magit-file-checkout)
(global-set-key (kbd "C-c C-l") 'magit-file-dispatch)
(global-set-key (kbd "C-c C-b") 'magit-blob-mode)
(global-set-key (kbd "C-c C-p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c C-n") 'git-gutter:next-hunk)
