;; Utility functions
;;
;; Mostly non-interactive definitions.


;; # Debug

(defun my/flash-mode-line ()
  "Flash the mode line to communicate an effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;; # Util

(defun my/is-inside-comment ()
  "Returns t if point is inside a comment."
  (if (nth 4 (syntax-ppss)) t))
