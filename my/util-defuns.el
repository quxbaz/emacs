;; Utility functions
;;
;; Mostly non-interactive definitions.


;; # Debug

(defun my/flash-mode-line ()
  "Flash the mode line to communicate an effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(defun my/print-to-buffer (obj &optional buffer-name)
  "Prints OBJ to a buffer. If BUFFER-NAME is nil, print to *scratch* buffer."
  (let* ((buffer-name (or buffer-name "*scratch*"))
         (buffer (get-buffer buffer-name)))
    (princ obj buffer)
    (princ "\n" buffer)))


;; # Util

(defun my/is-line-empty ()
  "Returns t if the line at point is empty, otherwise nil."
  (eq (point-at-bol) (point-at-eol)))

(defun my/is-inside-string ()
  "Returns t if point is inside a string."
  (if (nth 3 (syntax-ppss)) t))
(defun my/string-at (pos &optional offset)
  "Gets the string at a specified point."
  (let ((offset (or offset 0)))
    (string (char-after (+ pos offset)))))

(defun my/string-equal-at (pos str)
  "Returns t if string at POS matches STR."
  (string= (my/string-at pos) str))

(defun my/string-at-point (&optional offset)
  "Gets the string at point."
  (let ((offset (or offset 0)))
    (string (char-after (+ (point) offset)))))

(defun my/line-text ()
  "Gets text string at the current line."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun my/region-text ()
  "Gets text within the region. If region is inactive, return nil."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my/is-at-opening-parens ()
  "Returns t point is at opening parens. Considers strings and comments."
  (let ((parse-state (syntax-ppss)))
    (and (looking-at "(")
         (null (nth 3 parse-state))     ;; Return false if point is inside a string.
         (null (nth 4 parse-state)))))  ;; Return false if point is inside a comment.

(defun my/root-list-position (&optional max-depth)
  "Gets the position of the root list starting from point."
  (if (eq max-depth nil) (setq max-depth 100))
  (let ((current-point (point)))
    (save-excursion
      (condition-case nil
          (dotimes (n max-depth)
            (backward-up-list 1 t t)
            (setq current-point (point)))
        (t nil)))
    current-point))

(defun my/goto-root-list (&optional max-depth)
  "Moves point to opening parens of parent-most (ie, root) list."
  (interactive)
  (if (eq max-depth nil)
      (setq max-depth 100))
  (condition-case nil
      (backward-up-list max-depth t t)
    (scan-error nil)
    (user-error nil)))
