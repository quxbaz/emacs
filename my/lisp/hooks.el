;; -*- lexical-binding: t; -*-
;;
;; Lisp mode hooks


;; Treat ":" as a word character so backward-kill-word stops at keyword
;; boundaries (e.g. :foo) instead of splitting on the colon.
(add-hook 'lisp-data-mode-hook
          (lambda () (modify-syntax-entry ?: "w")))
