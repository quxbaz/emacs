;; -*- lexical-binding: t; -*-
;;
;; Lisp config, variables, modes


;;
;; # Shared Lisp config
;;
;; Apply to all Lisp and Lisp REPL modes.
(dolist (mode-hook '(emacs-lisp-mode-hook lisp-mode-hook slime-repl-mode-hook edebug-eval-mode-hook))
  (add-hook mode-hook (lambda ()
                        (autopair-mode -1)
                        (unless (eq mode-hook 'slime-repl-mode-hook)
                          (aggressive-indent-mode t))
                        (rainbow-blocks-mode t)
                        (paredit-mode t))))

;;
;; # Common Lisp config
(setq inferior-lisp-program (executable-find "sbcl"))
;; That will make sure SLIME plays nice with anything you do with Quicklisp in
;; the future (it will see all of the libraries you install, completion will
;; work etc).
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

;;
;; # SLIME and SLIME REPL config
(setq common-lisp-hyperspec-root (expand-file-name "~/common-lisp/hyperspec/HyperSpec/"))
