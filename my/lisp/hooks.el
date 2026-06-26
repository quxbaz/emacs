;; -*- lexical-binding: t; -*-
;;
;; Lisp mode hooks


(defun my/lisp-editing-setup (&optional no-aggressive-indent)
  "Enable the shared Lisp editing minor modes in the current buffer.
With NO-AGGRESSIVE-INDENT non-nil, skip `aggressive-indent-mode' (used
for the SLIME REPL, where reindenting prior output is undesirable)."
  (autopair-mode -1)
  ;; (unless no-aggressive-indent
  ;;   (aggressive-indent-mode t))
  (rainbow-blocks-mode t)
  (paredit-mode t))

;; lisp-data-mode is the parent of emacs-lisp-mode, lisp-mode, and
;; lisp-interaction-mode (the *scratch* buffer), so this covers all of them.
;; edebug-eval-mode buffers are switched to emacs-lisp-mode (see my/hooks.el),
;; so they pick this up too.
(add-hook 'lisp-data-mode-hook #'my/lisp-editing-setup)

;; slime-repl-mode does not derive from lisp-data-mode, so hook it explicitly
;; (without aggressive-indent).
(add-hook 'slime-repl-mode-hook (lambda () (my/lisp-editing-setup t)))
