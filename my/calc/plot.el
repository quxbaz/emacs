;; -*- lexical-binding: t; -*-
;;
;; Calc plotting via gnuplot


(defvar my/calc-graph-quick-range ""
  "Default x range for my/calc-graph-quick (gnuplot format: min:max, or empty for auto).")

(defun my/calc-to-gnuplot-string (expr)
  "Translate a Calc expression to a gnuplot formula string."
  (let* ((s (let ((calc-language 'fortran)
                  (calc-language-option nil))
               (math-format-value expr 0)))
         (s (downcase s))
         (s (replace-regexp-in-string "\\^" "**" s))
         ;; Fortran scientific notation (1d5) -> gnuplot (1e5)
         (s (replace-regexp-in-string "\\([0-9]\\)[d]\\([+-]?[0-9]\\)" "\\1e\\2" s))
         ;; ln(x) -> log(x)  (gnuplot uses log for natural log)
         (s (replace-regexp-in-string "ln(" "log(" s))
         ;; log(x, b) -> (log(x)/log(b))  (change of base; gnuplot has no 2-arg log)
         (s (replace-regexp-in-string "log(\\([^,()]*\\), *\\([^()]*\\))"
                                      "(log(\\1)/log(\\2))" s)))
    ;; Fortran uses spaces for implicit multiplication; gnuplot needs explicit *
    (while (string-match "\\([0-9a-zA-Z)]\\) \\([a-zA-Z(]\\)" s)
      (setq s (replace-regexp-in-string
               "\\([0-9a-zA-Z)]\\) \\([a-zA-Z(]\\)" "\\1*\\2" s)))
    s))

(defun my/calc-graph-quick--plot (range)
  "Internal: plot top-of-stack formula with RANGE string (may be nil or empty)."
  (calc-slow-wrapper
   (when (< (calc-stack-size) 1)
     (error "Stack is empty"))
   (let* ((range (and range (string-trim range)))
          (formula-str (my/calc-to-gnuplot-string (calc-top 1)))
          (range-str (cond ((or (null range) (string-empty-p range)) "")
                          ((string-match "^-?[0-9.]+$" range)
                           (format "[-%s:%s] " range range))
                          ((string-match "^:\\(.*\\)$" range)
                           (format "[0:%s] " (match-string 1 range)))
                          (t (format "[%s] " range))))
          (script (format "plot %s%s notitle with lines\npause mouse close\n" range-str formula-str))
          (tmpfile (make-temp-file "calc-plot" nil ".gp")))
     (write-region script nil tmpfile nil 'silent)
     (start-process "calc-gnuplot" nil "gnuplot" tmpfile))))

(defun my/calc-graph-quick ()
  "Plot top-of-stack formula with auto x range."
  (interactive)
  (my/calc-graph-quick--plot nil))

(defun my/calc-graph-quick-with-range ()
  "Plot top-of-stack formula, prompting for x range."
  (interactive)
  (my/calc-graph-quick--plot
   (read-string "X range (min:max): " my/calc-graph-quick-range)))

(provide 'my/calc/plot)
