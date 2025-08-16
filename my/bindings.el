;; Global keybindings


;; Guidelines:
;; * Prefer `M` bindings for non-destructive actions.
;; * Prefer `M` bindings for frequently-used actions.
;; * Prefer `C` bindings for destructive commands.
;; * Generally, Emacs convention suggests `M` for larger actions and `C` for smaller ones.


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
(global-unset-key (kbd "C-x r t"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-f"))
(global-unset-key (kbd "C-x 3"))


;; # Mouse config
(global-set-key (kbd "M-<down-mouse-1>") nil)
(global-set-key (kbd "M-<drag-mouse-1>") nil)
(global-set-key (kbd "<mouse-3>") nil)
(global-set-key (kbd "M-<down-mouse-3>") nil)
(global-set-key (kbd "M-<drag-mouse-3>") nil)
(global-set-key [mouse-2] 'clipboard-yank)
(global-set-key [mouse-8] 'switch-to-prev-buffer)  ;; Bind to back button.
(global-set-key [mouse-9] 'switch-to-next-buffer)  ;; Bind to next button.


;; # Rebind important prefixes
(global-set-key (kbd "C-0") 'digit-argument)
(global-unset-key (kbd "C-u"))  ;; Repurpose C-u as a right-hand leader key.
(global-set-key (kbd "C-u C-u") 'universal-argument)


;; # Help, info
(global-set-key (kbd "C-h C-h") 'my/help-dwim)
(global-set-key (kbd "C-h c") 'describe-key)
(global-set-key (kbd "C-h k") 'describe-key-briefly)
(global-set-key (kbd "C-h C-a") 'apropos)
(global-set-key (kbd "C-h q") 'shortdoc-display-group)
(global-set-key (kbd "<escape> k") 'my/goto-to-binding-definition)


;; # Mini-apps
(global-set-key (kbd "<f8>") 'my/list-packages)
(global-set-key (kbd "<escape> n") 'calc)
(global-set-key (kbd "<escape> c") 'ispell)
(global-set-key (kbd "<f10>") (my/cmd (list-processes) (other-window 1)))
(global-set-key (kbd "<f11>") 'proced)
(global-set-key (kbd "<f12>") 'calendar)
(global-set-key (kbd "<escape> y") 'my/visit-snippet-directory)


;; # Appearance, themes
;;


;; # Text navigation, marking
(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "C-u C-SPC") 'pop-to-mark-command)
(global-set-key (kbd "C-\\") 'my/match-delimiter)
(global-set-key (kbd "M-.") 'my/mark-context)
(global-set-key (kbd "M-h") 'my/mark-paragraph)


;; dwim region commands
(global-set-key (kbd ",") 'my/key-comma)
(global-set-key (kbd "\\") 'my/key-backslash)
(global-set-key (kbd "|") 'my/key-pipe)
(global-set-key (kbd "c") 'my/key-c)
(global-set-key (kbd "i") 'my/key-i)
(global-set-key (kbd "k") 'my/key-k)
(global-set-key (kbd "l") 'my/key-l)
(global-set-key (kbd "n") 'my/key-n)
(global-set-key (kbd "o") 'my/key-o)
(global-set-key (kbd "w") 'my/key-w)
(global-set-key (kbd "x") 'my/key-x)


;; # Jumping, definitions, source tracing
(global-set-key (kbd "M-'") 'xref-find-definitions)


;; # Editing
;; ## Kill ring, clipboard, undo
(global-set-key (kbd "M-w") 'my/kill-ring-save-dwim)
(global-set-key (kbd "C-y") 'my/yank)
(global-set-key [\S-insert] 'clipboard-yank)
(global-set-key (kbd "C-M-/") 'undo-only)
;; ## Creation, duplication, opening
(global-set-key (kbd "C-o") 'my/open-line)
(global-set-key (kbd "M-<return>") 'my/duplicate-dwim)
(global-set-key (kbd "M-S-<return>") 'my/duplicate-block)
;; ## Deletion, clearing
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-k") 'my/kill-block)
(global-set-key (kbd "C-M-k") 'my/clear-buffer)
;; ## Whitespace
(global-set-key (kbd "M-\\") 'delete-horizontal-space)
(global-set-key (kbd "C-x r \\") 'delete-whitespace-rectangle)
;; ## Commenting
(global-set-key (kbd "C-;") 'my/comment-line)
(global-set-key (kbd "M-;") 'my/comment-block)
;; ## Transposing
(global-set-key (kbd "M-<up>") 'my/transpose-line)
(global-set-key (kbd "M-<down>") (my/cmd (my/transpose-line t)))
(global-set-key (kbd "M-T") 'transpose-regions)
(global-set-key (kbd "S-<left>") 'transpose-sexps)
;; ## Misc editing
(global-set-key (kbd "C-q") 'fill-paragraph)


;; # Indentation
(global-set-key (kbd "M-q") 'my/indent-block)
(global-set-key (kbd "M-I") 'indent-rigidly-left)
(global-set-key (kbd "M-i") 'indent-rigidly-right)
(global-set-key (kbd "C-w") 'delete-indentation)
(global-set-key (kbd "C-j") 'newline-and-indent)


;; # Commands
(global-set-key (kbd "C-g") 'my/quit)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "<escape> <escape> i") 'my/insert-uuid)
(global-set-key (kbd "<escape> <escape> t") 'org-time-stamp)
(global-set-key (kbd "<escape> p") 'my/toggle-emacs-lisp-mode)


;; # Auto-complete
(global-set-key (kbd "M-/") 'hippie-expand)


;; # Search, replace, regexp, occur
;; ## Search
(global-set-key (kbd "C-s") 'my/isearch-forward-dwim)
(global-set-key (kbd "C-r") 'my/isearch-backward-dwim)
(global-set-key (kbd "C-c >") 'search-forward-regexp)
(global-set-key (kbd "C-c <") 'search-backward-regexp)

;; ## Replace
(global-set-key (kbd "M-r") 'my/query-replace-dwim)
(global-set-key (kbd "M-R") 'my/query-replace-buffer-dwim)
;; ## Grep, find, occur
(global-set-key (kbd "<escape> /") 'deadgrep)
(global-set-key (kbd "C-c o") 'my/occur-dwim)


;; # Files
(global-set-key (kbd "C-u C-f") 'find-file)
(global-set-key (kbd "M-F") 'find-file-other-window)
(global-set-key (kbd "C-c p") 'find-file-at-point)


;; # Projects
(global-set-key (kbd "C-c C-l") 'my/project-list-buffers)
(global-set-key (kbd "C-c C-SPC") 'ido-dired)


;; # Buffers, windows, dired
;; ## Buffers
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "<escape> .") 'kill-current-buffer)
(global-set-key (kbd "C-,") 'my/switch-to-other-buffer)
(global-set-key (kbd "M-SPC") 'ivy-switch-buffer)
(global-set-key (kbd "<escape> SPC") 'ibuffer)
(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)
(global-set-key (kbd "<escape> ,") 'bury-buffer)
(global-set-key (kbd "C-c C-v") 'my/revert-buffer)
(global-set-key (kbd "<escape> RET") 'clone-indirect-buffer)
;; ## Windows
(global-set-key (kbd "C-x C-o") 'my/other-window)
(global-set-key (kbd "C-u C-s") (my/cmd (split-window-right) (windmove-right)))
(global-set-key (kbd "<escape> -") 'window-swap-states)
(global-set-key (kbd "<escape> 9") 'delete-other-windows)
(global-set-key (kbd "<escape> 0") 'delete-window)
(global-set-key (kbd "<escape> = ") 'balance-windows)
;; ## Dired
(global-set-key (kbd "M-`") 'my/dired-jump)
(global-set-key (kbd "M-~") 'dired-jump-other-window)


;; # Bookmarks, registers
(global-set-key (kbd "<escape> l") 'bookmark-bmenu-list)
(global-set-key (kbd "<escape> m") 'bookmark-set)
(global-set-key (kbd "<escape> j") 'jump-to-register)
(global-set-key (kbd "M-1") (my/cmd (jump-to-register ?1)))
(global-set-key (kbd "M-2") (my/cmd (jump-to-register ?2)))
(global-set-key (kbd "M-3") (my/cmd (jump-to-register ?3)))
(global-set-key (kbd "M-4") (my/cmd (jump-to-register ?4)))


;; # Viewing, narrowing, filtering
(global-set-key (kbd "C-<tab>") 'outline-toggle-children)
(global-set-key (kbd "<C-iso-lefttab>") 'my/outline-toggle-all)


;; # Math, numbers
(global-set-key (kbd "C-M-=") 'my/increment)
(global-set-key (kbd "C-M--") 'my/decrement)


;; # Debugging
(global-set-key (kbd "C-u C-c C-c") (my/cmd-u 'eval-defun))  ;; Instrument function w/ edebug.


;; # Keyboard Macros
(global-set-key (kbd "<escape> [") 'kmacro-start-macro)
(global-set-key (kbd "<escape> ]") 'kmacro-end-macro)
(global-set-key (kbd "M-,") 'kmacro-end-and-call-macro)
(global-set-key (kbd "M-,") 'my/call-macro-dwim)  ;; TODO: apply-macro-to-region-lines
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
(global-set-key (kbd "C-M-l") 'magit-blame-addition)
(global-set-key (kbd "C-M-b") 'magit-blob-mode)
(global-set-key (kbd "C-c C-p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c C-n") 'git-gutter:next-hunk)


;; # org
(global-set-key (kbd "<escape> <escape> m") 'org-store-link)
(global-set-key (kbd "C-c C-,") (my/cmd (org-agenda nil "n")))
