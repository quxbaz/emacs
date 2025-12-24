;; -*- lexical-binding: t; -*-
;;
;; Custom calc commands and utilities
;;
;; This file loads all calc-related modules.


;; Load all calc modules
(let ((calc-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (concat calc-dir "var.el"))
  (load (concat calc-dir "lib.el"))
  (load (concat calc-dir "stack.el"))
  (load (concat calc-dir "edit.el"))
  (load (concat calc-dir "selection.el"))
  (load (concat calc-dir "rewrite.el"))
  (load (concat calc-dir "minibuffer.el"))
  (load (concat calc-dir "bindings.el")))

(provide 'my/calc)
