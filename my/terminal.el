;; Terminal config


(global-set-key (kbd "<xterm-paste>") 'scroll-up-command)
(global-set-key (kbd "M-,") 'my/switch-to-other-buffer)
(global-set-key (kbd "M-RET") 'my/duplicate-line)
(global-set-key (kbd "C-c <xterm-paste>") 'my/revert-buffer)
(global-set-key (kbd "C-M-@") 'magit)
