;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Dependencies
(require 's)
(require 'dash)

;; Extensions
(load-file (concat user-emacs-directory "ext/show-point-mode.el"))

;; Local packages
;; Load maf package if it exists.
(let ((maf-path (concat user-emacs-directory "site-lisp/maf")))
  (when (file-exists-p (concat maf-path "/maf.el"))
    (add-to-list 'load-path maf-path)
    (require 'maf)
    ;; Persist each session's calc stack across restarts;
    ;; M-x maf-restore-stack-from loads another session's stack.
    (maf-persist-mode 1)))

;; Load wire package if it exists.
(let ((wire-path (concat user-emacs-directory "site-lisp/wire")))
  (when (file-exists-p (concat wire-path "/wire.el"))
    (add-to-list 'load-path wire-path)
    (autoload 'wire-mode "wire" "Wire annotated regions to a running Claude instance." t)
    (autoload 'wire-dispatch "wire" nil t)
    (autoload 'wire-select-target "wire" nil t)
    (autoload 'wire-list-instances "wire" nil t)
    (autoload 'wire-doctor "wire" nil t)
    (autoload 'global-wire-mode "wire" nil t)
    (global-wire-mode 1)))

;; Load lesson package if it exists.
(let ((lesson-path (concat user-emacs-directory "site-lisp/lesson")))
  (when (file-exists-p (concat lesson-path "/lesson.el"))
    (add-to-list 'load-path lesson-path)
    (autoload 'lesson-load "lesson" "Load and play a lesson plan." t)
    (autoload 'lesson-start "lesson" nil t)))

;; Load empty-line-mode package if it exists.
(let ((eol-path (concat user-emacs-directory "site-lisp/empty-line-mode")))
  (when (file-exists-p (concat eol-path "/empty-line-mode.el"))
    (add-to-list 'load-path eol-path)
    (autoload 'empty-line-mode "empty-line-mode" "Mark blank lines with a fringe indicator." t)
    (autoload 'global-empty-line-mode "empty-line-mode" nil t)
    (global-empty-line-mode 1)))

;; Custom config
(load-file (concat user-emacs-directory "my/data.el"))
(load-file (concat user-emacs-directory "my/conf.el"))
(load-file (concat user-emacs-directory "my/theme.el"))
;; Libraries, helpers, utilities
(load-file (concat user-emacs-directory "my/lib-string.el"))
(load-file (concat user-emacs-directory "my/lib-text.el"))
(load-file (concat user-emacs-directory "my/lib-js.el"))
(load-file (concat user-emacs-directory "my/util.el"))
;; Commands
(load-file (concat user-emacs-directory "my/commands.el"))
;; Lisp
(load-file (concat user-emacs-directory "my/lisp/lib.el"))
(load-file (concat user-emacs-directory "my/lisp/commands.el"))
(load-file (concat user-emacs-directory "my/lisp/conf.el"))
(load-file (concat user-emacs-directory "my/lisp/bindings.el"))
(load-file (concat user-emacs-directory "my/lisp/hooks.el"))
;; Bindings
(load-file (concat user-emacs-directory "my/bindings.el"))
(load-file (concat user-emacs-directory "my/kmacros.el"))
;; Per-mode configuration (settings + hooks + bindings, one block per mode)
(load-file (concat user-emacs-directory "my/modes.el"))

;; project-init
;;
;; If a git project's root contains a project-init.el file, load it the
;; first time a file or directory from that project is visited in this
;; session. The dired hook covers starting Emacs on a directory (emacs .),
;; since dired buffers don't run find-file-hook.
(defvar my/project-init-loaded-roots nil
  "Project roots whose project-init.el has already been loaded.")

;; Init files run with default-directory set to the directory Emacs was
;; invoked in, so this captures the startup project.
(defvar my/project-root
  (when-let* ((root (locate-dominating-file default-directory ".git")))
    (expand-file-name root))
  "Root of the git repository Emacs was started in, or nil if none.")

(defun my/load-project-init ()
  (when-let* ((root (locate-dominating-file default-directory ".git"))
              (root (expand-file-name root))
              (init-file (concat root "project-init.el")))
    (when (and (file-exists-p init-file)
               (not (member root my/project-init-loaded-roots)))
      (push root my/project-init-loaded-roots)
      (load init-file)
      (message "Loaded %s" init-file))))

(add-hook 'find-file-hook #'my/load-project-init)
(add-hook 'dired-mode-hook #'my/load-project-init)

;; Autoloads
(autoload 'sql-lisp-mode (concat user-emacs-directory "my/lib-sql.el") "A mode for SQL interaction through evaluation of Emacs Lisp forms." t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(package-selected-packages
   '(
     aggressive-indent
     autopair
     corfu
     corfu-terminal
     deadgrep
     diff-hl
     doom-themes
     highlight-indent-guides
     ivy
     kkp
     lua-mode
     magit
     markdown-mode
     modus-themes
     paredit
     posframe
     rainbow-blocks
     slime
     web-mode
     xclip
     yasnippet
     )))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-section-highlight ((t (:inherit hl-line :background "blue"))))
 '(org-ellipsis ((t (:foreground "gray50")))))
