;; -*- lexical-binding: t; -*-
;;
;; Calc utility functions


(defun my/calc-at-stack-bottom-p ()
  "Returns t if point is at the stack bottom or beyond it."
  (<= (calc-locate-cursor-element (point)) 1))

(provide 'my/calc/lib)
