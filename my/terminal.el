;; Terminal config


(global-set-key (kbd "<xterm-paste>") 'scroll-up-command)
(global-set-key (kbd "M-,") 'my/switch-to-other-buffer)
(global-set-key (kbd "M-RET") 'my/duplicate-list)
(global-set-key (kbd "C-c <xterm-paste>") 'my/revert-buffer)
(global-set-key (kbd "C-M-@") 'magit)

;; Make <escape> bindings compatible w/ terminal.
(global-set-key (kbd "ESC M-\\") 'my/open-scratch-buffer)
(global-set-key (kbd "ESC M-i") 'string-rectangle)
(global-set-key (kbd "ESC M-/") 'deadgrep)
(global-set-key (kbd "ESC ESC ESC") (lambda () (interactive) (kill-buffer)))
(global-set-key (kbd "ESC M-SPC") 'ibuffer)
(global-set-key (kbd "ESC M-l") 'bookmark-bmenu-list)
(global-set-key (kbd "ESC M-m") 'bookmark-set)
(global-set-key (kbd "ESC M--") 'window-swap-states)
(global-set-key (kbd "ESC M-9") 'delete-other-windows)
(global-set-key (kbd "ESC M-0") 'delete-window)
(global-set-key (kbd "ESC M-= ") 'balance-windows)
(global-set-key (kbd "ESC M-b") 'debug-on-entry)
(global-set-key (kbd "ESC M-u b") 'cancel-debug-on-entry)
(global-set-key (kbd "ESC M-[") 'kmacro-start-macro)
(global-set-key (kbd "ESC M-]") 'kmacro-end-macro)
