;; Mode hooks


(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'eval-expression-minibuffer-setup-hook (lambda () (paredit-mode t)))
(add-hook 'calendar-mode-hook 'diary-mark-entries)
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))
(add-hook 'occur-mode-hook (lambda () (switch-to-buffer-other-window "*Occur*")))

(add-hook 'after-init-hook
          (lambda ()
            (find-file-noselect user-init-file t)
            (find-file-noselect (expand-file-name "my/*" user-emacs-directory) nil nil t)
            (find-file-noselect (concat user-emacs-directory "bugs.el"))
            (find-file-noselect (concat user-emacs-directory "scratch.el"))
            (find-file-noselect "~/personal/diary")
            (find-file-noselect "~/personal/cad.org")
            (find-file-noselect "~/personal/food.org")
            (find-file-noselect "~/personal/main.org")
            (find-file-noselect "~/personal/media-queue.org")
            (find-file-noselect "~/personal/promise.org")
            (find-file-noselect "~/personal/rem.org")
            (find-file-noselect "~/personal/remember.org")
            (find-file-noselect "~/personal/archive/buy.org")
            (find-file-noselect "~/personal/archive/people.org")
            (find-file-noselect "~/personal/archive/projects.org")
            (find-file-noselect "~/personal/exercise/exercises.org")
            (find-file-noselect "~/personal/exercise/misc.org")
            (find-file-noselect "~/personal/exercise/records.org")
            (find-file-noselect "~/personal/exercise/routines.org")
            (find-file-noselect "~/personal/linux/emacs-log.org")
            (find-file-noselect "~/personal/linux/emacs.org")
            (find-file-noselect "~/personal/linux/linux.org")
            (find-file-noselect "~/conf/i3/config")
            ;; Work related.
            (find-file-noselect "~/work/wnmu/Todo.org")
            (find-file-noselect "~/work/wnmu/projects.org")
            (find-file-noselect "~/work/wnmu/refactor.org")
            (find-file-noselect "~/work/wnmu/huddle-notes.org")
            (find-file-noselect "~/work/wnmu/records/creds.org")))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            (put 'dired-find-alternate-file 'disabled nil)
            (when (file-remote-p default-directory)
              (setq dired-listing-switches "-l"))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (autopair-mode 0)
            (aggressive-indent-mode t)
            (paredit-mode t)
            (rainbow-blocks-mode t)))

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
