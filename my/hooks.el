;; Mode hooks


(setq-local my/init-files (list user-init-file
                                (concat user-emacs-directory "bugs.el")
                                (concat user-emacs-directory "scratch.el")
                                (concat user-emacs-directory "sql-notebook.el")
                                "~/personal/bookmarks.org"
                                "~/personal/cad.org"
                                "~/personal/diary"
                                "~/personal/dog"
                                "~/personal/food.org"
                                "~/personal/main.org"
                                "~/personal/media-queue.org"
                                "~/personal/programming.org"
                                "~/personal/promise.org"
                                "~/personal/rem.org"
                                "~/personal/remember.org"
                                "~/personal/archive/buy.org"
                                "~/personal/archive/people.org"
                                "~/personal/archive/projects.org"
                                "~/personal/exercise/exercises.org"
                                "~/personal/exercise/misc.org"
                                "~/personal/exercise/movement.org"
                                "~/personal/exercise/records.org"
                                "~/personal/exercise/routines.org"
                                "~/personal/linux/emacs.org"
                                "~/personal/linux/linux.org"
                                "~/conf/sync.org"
                                "~/conf/i3/config"
                                "~/conf/zsh/.zshrc"
                                "~/work/wnmu/Todo.org"
                                "~/work/wnmu/huddle-notes.org"
                                "~/work/wnmu/projects.org"
                                "~/work/wnmu/records/creds.org"
                                "~/work/wnmu/refactor.org"))

(add-hook 'after-init-hook
          (lambda ()
            (find-file-noselect (expand-file-name "my/*" user-emacs-directory) nil nil t)
            (dolist (filepath my/init-files)
              (if (file-exists-p filepath)
                  (find-file-noselect filepath)
                (error "Init file does not exist: %s" filepath)))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'calendar-mode-hook 'diary-mark-entries)
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))
(add-hook 'occur-mode-hook (lambda () (switch-to-buffer-other-window "*Occur*")))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
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
