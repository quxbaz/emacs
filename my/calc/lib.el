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

(defun my/calc-active-selection-at-line-p ()
  "Returns t if there are any active selections at point."
  (let ((m (calc-locate-cursor-element (point))))
    (nth 2 (nth m calc-stack))))

(defun my/calc-first-active-entry ()
  "Return the first stack entry with an active selection beginning from
the top of the stack, or nil if there are no active selections."
  ;; If any stack element has a non-nil value at (nth 2 elt), then selection is
  ;; active.
  (seq-find (lambda (elt) (nth 2 elt)) calc-stack))

(defun my/calc-first-active-entry-m ()
  "Return the POSITION of the first stack entry with an active selection
beginning from the top of the stack, or nil if there are no active selections."
  ;; If any stack element has a non-nil value at (nth 2 elt), then selection is
  ;; active.
  (cl-loop for i from 1 below (length calc-stack)
           when (nth 2 (nth i calc-stack))
           return i))

(defun my/calc-active-entry-m-dwim ()
  "Return the POSITION of the active stack entry at the current line. If
there is no active selection at the current line, return the active entry
closest to the stack. Return nil if there are no active entries."
  ;; If any stack element has a non-nil value at (nth 2 elt), then selection is
  ;; active.
  (if (my/calc-active-selection-at-line-p)
      (calc-locate-cursor-element (point))
    (my/calc-first-active-entry-m)))

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
