;; Keep. Eval.
(define-minor-mode my/scratch-mode "my/scratch-mode" nil nil (make-sparse-keymap))
(define-key my/scratch-mode-map (kbd "C-c C-c") 'eros-eval-defun)
(define-key my/scratch-mode-map (kbd "C-c .") 'eros-eval-last-sexp)
(my/scratch-mode t)
;;
