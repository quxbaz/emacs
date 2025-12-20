;; -*- lexical-binding: t; -*-
;;
;; Calc utility functions


(defun my/calc-at-stack-bottom-p ()
  "Returns t if point is at the stack bottom or beyond it."
  (save-excursion
    (condition-case nil
        (progn (next-line 2) (previous-line))
      (error nil))
    (let ((line (string-trim (substring-no-properties (thing-at-point 'line)))))
      (string= line "."))))

(provide 'my/calc/lib)
