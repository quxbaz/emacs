;; -*- lexical-binding: t; -*-
;;
;; Calc utility functions


(defun my/calc-at-stack-bottom-p ()
  "Returns t if point is at the stack bottom or beyond it."
  (<= (calc-locate-cursor-element (point)) 1))

(defun my/calc-no-selection-p ()
  "Returns t if there are no active selections."
  (not (my/calc-no-selection-p)))

(defun my/calc-active-selection-p ()
  "Returns t if there are any active selections."
  (and calc-use-selections
       ;; If any stack element has a non-nil value at (nth 2 elt), then
       ;; selection is active.
       (seq-some (lambda (elt) (nth 2 elt)) calc-stack)))

(defmacro my/calc-dont-simplify (&rest forms)
  "Execute FORMS with `calc-simplify-mode' temporarily set to \\='none.

The previous simplification mode is restored afterwards, even if
FORMS signals an error. Returns the value of the last form in FORMS."
  `(let ((mode calc-simplify-mode))
     (unwind-protect
         (progn
           (calc-change-mode 'calc-simplify-mode 'none)
           ,@forms)
       (calc-change-mode 'calc-simplify-mode mode))))

(provide 'my/calc/lib)
