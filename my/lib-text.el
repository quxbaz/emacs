;; My text library
;;
;; Collection of functions for handling text inside a buffer. This is not a
;; string library.


;; Non-destructive functions

(defun my/is-inside-string ()
  "Returns t if point is inside a string."
  (if (nth 3 (syntax-ppss)) t))

(defun my/string-beginning-position ()
  "Returns position of opening quote of current string."
  (let ((parse-state (syntax-ppss)))
    (when (nth 3 parse-state)
      (nth 8 parse-state))))

(defun my/string-at (pos &optional offset)
  "Gets the string at a specified point."
  (let ((offset (or offset 0)))
    (string (char-after (+ pos offset)))))

(defun my/is-string-marked ()
  "Returns true if the region marks a complete string."
  ;; 34 is the integer representation of the quote character.
  (and (use-region-p)
       (= (char-after (region-beginning)) 34)
       (= (char-before (region-end)) 34)))


;; Destructive functions

(defun my/goto-beginning-of-string ()
  "Moves point to opening quote of current string."
  (let ((parse-state (syntax-ppss)))
    (when (nth 3 parse-state)
      (goto-char (nth 8 parse-state)))))

(defun my/mark-string (&optional position)
  "Marks the string at a position."
  (if position
      (goto-char position))
  (when (my/is-inside-string)
    (my/goto-beginning-of-string)
    (mark-sexp)))
