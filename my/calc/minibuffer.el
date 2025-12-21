;; -*- lexical-binding: t; -*-
;;
;; Calc minibuffer functions


(defun my/calc-insert-colon ()
  "Inserts a colon character."
  (interactive)
  (insert ":"))

(defun my/calc-equal-to-from-minibuffer ()
  "Applies equal-to from minibuffer."
  (interactive)
  (let ((input (math-read-number (minibuffer-contents))))
    (delete-minibuffer-contents)
    (calc-wrapper
     (calc-push input)
     (call-interactively 'calc-equal-to))
    (exit-minibuffer)))

(defun my/calc-mod-360-from-minibuffer ()
  "Applies modulo 360 (degrees) from minibuffer."
  (interactive)
  (let ((input (math-read-number (minibuffer-contents))))
    (delete-minibuffer-contents)
    (calc-wrapper
     (calc-push input)
     (calc-enter-result 1 "norm" (math-mod input 360)))
    (exit-minibuffer)))

(provide 'my/calc/minibuffer)
