;; -*- lexical-binding: t; -*-
;;
;; Calc plotting via gnuplot


(defvar my/calc-graph-quick-range ""
  "Default x range for my/calc-graph-quick (gnuplot format: min:max, or empty for auto).")

(defun my/calc-to-gnuplot-string (expr)
  "Translate a Calc expression to a gnuplot formula string.
Uses Calc's Fortran language mode as a base (explicit *, ** for power,
LOG/LOG10 matching gnuplot's log/log10), then lowercases the result
and converts Fortran scientific notation (1D5) to gnuplot form (1e5)."
  (let* ((s (let ((calc-language 'fortran)
                  (calc-language-option nil))
               (math-format-value expr 0)))
         (s (downcase s))
         (s (replace-regexp-in-string "\\^" "**" s))
         ;; Fortran uses D for scientific notation exponents; gnuplot uses e
         (s (replace-regexp-in-string "\\([0-9]\\)[d]\\([+-]?[0-9]\\)" "\\1e\\2" s)))
    s))

(defun my/calc-graph-quick (_arg)
  "Plot top-of-stack formula by invoking gnuplot directly.
Prompts for x range; press RET to use the default, or clear for auto.
The formula is not consumed from the stack."
  (interactive "P")
  (calc-slow-wrapper
   (when (< (calc-stack-size) 1)
     (error "Stack is empty"))
   (let* ((range (read-string "X range (min:max): " my/calc-graph-quick-range))
          (formula-str (my/calc-to-gnuplot-string (calc-top 1)))
          (range-str (if (and range (not (string-empty-p range)))
                         (format "[%s] " range) ""))
          (script (format "plot %s%s notitle with lines\npause mouse close\n" range-str formula-str))
          (tmpfile (make-temp-file "calc-plot" nil ".gp")))
     (write-region script nil tmpfile nil 'silent)
     (start-process "calc-gnuplot" nil "gnuplot" tmpfile))))

(provide 'my/calc/plot)
