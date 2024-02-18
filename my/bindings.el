;; Global keybindings


;; Guidelines:
;; * Prefer `M` bindings for non-destructive actions.
;; * Prefer `M` bindings for frequently-used actions.
;; * Prefer `C` bindings for destructive commands.
;; * Generally, Emacs convention suggests `M` for larger actions and `C` for smaller ones.
;;
;; Notes:
;; - Available prefixes: C-q, C-z


;; # Disabled keys
(global-set-key (kbd "C-q") nil)
(global-set-key (kbd "C-z") nil)
;; Disable these to force yourself to use your preferred bindings.
(global-unset-key (kbd "C-x ("))
(global-unset-key (kbd "C-x )"))
(global-unset-key (kbd "C-x e"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x r d"))


;; # Mouse config
(global-set-key (kbd "M-<down-mouse-1>") nil)
(global-set-key (kbd "M-<drag-mouse-1>") nil)
(global-set-key (kbd "<mouse-3>") nil)
(global-set-key (kbd "M-<down-mouse-3>") nil)
(global-set-key (kbd "M-<drag-mouse-3>") nil)
(global-set-key [mouse-2] nil)  ;; Disable middle mouse button.
(global-set-key [mouse-8] 'switch-to-prev-buffer)  ;; Bind to back button.
(global-set-key [mouse-9] 'switch-to-next-buffer)  ;; Bind to next button.


;; # Help, info
(global-set-key (kbd "C-h C-a") 'apropos)
(global-set-key (kbd "C-h C-f") 'find-library)
(global-set-key (kbd "C-h q") 'shortdoc-display-group)


;; # Mini-apps
(global-set-key (kbd "<f12>") 'calendar)


;; # Appearance, themes
;;


;; # Text navigation, marking
(global-set-key (kbd "C-\\") 'my/match-paren)
(global-set-key (kbd "M-.") 'my/mark-context)
(global-set-key (kbd "M-h") 'my/mark-paragraph)


;; # Jumping
(global-set-key (kbd "M-'") 'xref-find-definitions)


;; # Editing
;; ## Kill ring, clipboard, undo
(global-set-key [\S-insert] 'clipboard-yank)
(global-set-key (kbd "C-M-/") 'undo-only)
;; ## Creation, duplication, opening
(global-set-key (kbd "C-o") 'my/open-line)
(global-set-key (kbd "M-<return>") 'my/duplicate-line)
(global-set-key (kbd "M-S-<return>") 'my/duplicate-block)
;; ## Deletion, clearing
(global-set-key (kbd "C-d") 'my/delete-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-k") 'my/kill-block)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-M-k") 'my/clear-buffer)
;; ## Whitespace
(global-set-key (kbd "M-\\") 'delete-horizontal-space)
(global-set-key (kbd "C-x r \\") 'delete-whitespace-rectangle)
;; ## Commenting
(global-set-key (kbd "C-;") 'my/comment-line)
(global-set-key (kbd "M-;") 'my/comment-block)
;; ## Transposing
(global-set-key (kbd "M-<up>") 'my/transpose-line)
(global-set-key (kbd "M-<down>") (lambda () (interactive) (my/transpose-line t)))
(global-set-key (kbd "M-T") 'transpose-regions)
;; ## Sorting, alignment
(global-set-key (kbd "C-c C-\\") 'align-regexp)


;; # Indentation
(global-set-key (kbd "M-q") 'my/indent-block)
(global-set-key (kbd "M-I") 'indent-rigidly-left)
(global-set-key (kbd "M-i") 'indent-rigidly-right)
(global-set-key (kbd "C--") 'delete-indentation)
(global-set-key (kbd "C-j") 'newline-and-indent)


;; # Commands
(global-set-key (kbd "C-.") 'repeat)


;; # Auto-complete
(global-set-key (kbd "M-/") 'hippie-expand)


;; # Search, replace, regexp, occur
;; ## Search
(global-set-key (kbd "C-s") 'my/isearch-forward-dwim)
(global-set-key (kbd "C-r") 'my/isearch-backward-dwim)
;; ## Replace
(global-set-key (kbd "M-r") 'my/query-replace-dwim)
(global-set-key (kbd "M-R") 'my/query-replace-buffer-dwim)
;; ## Grep, find, occur
(global-set-key (kbd "C-S-s") 'deadgrep)
(global-set-key (kbd "C-c o") 'my/occur-dwim)


;; # Files
(global-set-key (kbd "M-F") 'find-file-other-window)
(global-set-key (kbd "C-c p") 'find-file-at-point)


;; # Buffers, dired
;; ## Buffers
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "C-,") 'my/switch-to-other-buffer)
(global-set-key (kbd "M-SPC") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-v") 'my/revert-buffer)
;; ## Dired
(global-set-key (kbd "C-c C-SPC") 'ido-dired)
(global-set-key (kbd "M-`") 'dired-jump)
(global-set-key (kbd "M-~") 'dired-jump-other-window)


;; # Registers
(global-set-key (kbd "M-1") (lambda () (interactive) (jump-to-register ?1)))
(global-set-key (kbd "M-2") (lambda () (interactive) (jump-to-register ?2)))
(global-set-key (kbd "M-3") (lambda () (interactive) (jump-to-register ?3)))
(global-set-key (kbd "M-4") (lambda () (interactive) (jump-to-register ?4)))
(global-set-key (kbd "C-M-m") 'my/move-line-to-register)


;; # Modes, viewing, narrowing, filtering
(global-set-key (kbd "C-<tab>") 'outline-toggle-children)
(global-set-key (kbd "<C-iso-lefttab>") 'my/outline-toggle-all)


;; # Windows
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "<escape> s") 'window-swap-states)
(global-set-key (kbd "<escape> 9") 'delete-other-windows)
(global-set-key (kbd "<escape> 0") 'delete-window)


;; # Keyboard Macros
(global-set-key (kbd "<escape> [") 'kmacro-start-macro)
(global-set-key (kbd "<escape> ]") 'kmacro-end-macro)
(global-set-key (kbd "M-,") 'kmacro-end-and-call-macro)
(setq kmacro-call-repeat-key ?,)
(global-set-key (kbd "C-c C-9") 'kmacro-cycle-ring-previous)
(global-set-key (kbd "C-c C-0") 'kmacro-cycle-ring-next)
(global-set-key (kbd "C-c C-e") 'kmacro-edit-macro)
(global-set-key (kbd "C-c C-a") 'kmacro-step-edit-macro)
;; (global-set-key (kbd "C-c C-n") 'kmacro-name-last-macro)
(global-set-key (kbd "C-c C-i") 'insert-kbd-macro)


;; # git, magit
(global-set-key (kbd "C-M-SPC") 'magit-status)
(global-set-key (kbd "<f5>") 'magit-file-checkout)
(global-set-key (kbd "C-c C-l") 'magit-file-dispatch)
(global-set-key (kbd "C-c C-b") 'magit-blob-mode)
(global-set-key (kbd "C-c C-p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c C-n") 'git-gutter:next-hunk)


;; # org-mode
(global-set-key (kbd "M-S-SPC") 'org-switchb)
