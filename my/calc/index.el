;; -*- lexical-binding: t; -*-
;;
;; Custom calc commands and utilities
;;
;; This file loads all calc-related modules.


(setq calc-display-trail nil)
(setq calc-graph-default-resolution 1000)
(setq calc-gnuplot-default-device "qt")

;; Seed *Gnuplot Commands* with defaults on first init.
;; Calc sends set nogrid / set nokey before every plot, then appends this buffer,
;; so these settings override those resets.
(with-eval-after-load 'calc-graph
  (advice-add 'calc-graph-init-buffers :after
              (lambda ()
                (with-current-buffer calc-gnuplot-input
                  (when (= (buffer-size) 0)
                    (insert "set grid\nset nokey\n"))))))

;; Load all calc modules
(let ((calc-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (concat calc-dir "var.el"))
  (load (concat calc-dir "lib.el"))
  (load (concat calc-dir "stack.el"))
  (load (concat calc-dir "ineq.el"))
  (load (concat calc-dir "edit.el"))
  (load (concat calc-dir "selection.el"))
  (load (concat calc-dir "rewrite.el"))
  (load (concat calc-dir "minibuffer.el"))
  (load (concat calc-dir "bindings.el")))

(provide 'my/calc)
