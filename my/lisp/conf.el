;; -*- lexical-binding: t; -*-
;;
;; Lisp config, variables, modes


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
