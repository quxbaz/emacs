;; Mode hooks


(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'calendar-mode-hook 'diary-mark-entries)
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))
(add-hook 'occur-mode-hook (lambda () (switch-to-buffer-other-window "*Occur*")))

(defun my/calc-minibuffer-setup ()
  "Set up keybindings for calc minibuffers."
  (when (and (minibufferp)
             (with-current-buffer (window-buffer (minibuffer-selected-window))
               (derived-mode-p 'calc-mode)))
    (local-set-key (kbd ";") (my/cmd (insert ":")))))

(add-hook 'minibuffer-setup-hook 'my/calc-minibuffer-setup)

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            (dired-omit-mode t)
            (put 'dired-find-alternate-file 'disabled nil)
            (when (file-remote-p default-directory)
              (setq dired-listing-switches "-l"))))

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)
            (setq-local require-final-newline nil)))

(add-hook 'magit-mode-hook
          (lambda ()
            (setq magit-save-repository-buffers nil)
            (add-to-list 'magit-no-confirm 'stage-all-changes)
            (add-to-list 'magit-no-confirm 'unstage-all-changes)))

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 0)
            (git-gutter-mode t)
            (show-point-mode t)))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-enable-auto-indentation nil)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-css-indent-offset 2)))
