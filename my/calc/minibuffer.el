;; -*- lexical-binding: t; -*-
;;
;; Calc minibuffer functions


(defun my/calc-insert-colon ()
  "Inserts a colon character."
  (interactive)
  (insert ":"))

(defun my/calc-insert-pi-from-minibuffer ()
  "Pushes entry and multiplies by pi."
  (interactive)
  (let ((input (math-read-number (minibuffer-contents))))
    (delete-minibuffer-contents)
    (calc-wrapper
     (calc-enter-result 0 "n*pi" (math-mul input '(var pi var-pi))))
    (exit-minibuffer)))

(defun my/calc-equal-to-from-minibuffer ()
  "Pushes input then replays e to call my/calc-equal-to contextually."
  (interactive)
  (let ((input (math-read-number (minibuffer-contents))))
    (delete-minibuffer-contents)
    (calc-wrapper
     (calc-set-command-flag 'no-align)
     (calc-enter-result 0 "" input))
    (calcDigit-nondigit)))

(defun my/calc-mod-360-from-minibuffer ()
  "Applies modulo 360 (degrees) from minibuffer."
  (interactive)
  (let ((input (math-read-number (minibuffer-contents))))
    (delete-minibuffer-contents)
    (calc-wrapper
     (calc-push input)
     (calc-enter-result 1 "norm" (math-mod input 360)))
    (exit-minibuffer)))

(with-eval-after-load 'calc
  (advice-add 'calcDigit-nondigit :before
              (lambda ()
                "Suppress calc-align-stack-window when digit entry completes via a
command key. Point only moves to home when RET (13) or SPC (32)
completes the entry or when point is already at home."
                (unless (or (my/calc-point-is-at-home-p)
                            (memq last-command-event '(13 32)))
                  (calc-set-command-flag 'no-align)))))

(provide 'my/calc/minibuffer)
