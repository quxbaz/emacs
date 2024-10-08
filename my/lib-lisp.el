;; My Lisp library
;;
;; Collection of functions for handling Lisp structures inside a buffer.


;; # Non-destuctive

(defun my/is-inside-list ()
  "Returns t if point is inside a list."
  (> (car (syntax-ppss)) 0))

(defun my/is-at-opening-paren ()
  "Returns t if point is at opening paren. Ignores strings and comments."
  (let ((parse-state (syntax-ppss)))
    (and (looking-at "(")
         (null (nth 3 parse-state))     ;; Return nil if point is inside a string.
         (null (nth 4 parse-state)))))  ;; Return nil if point is inside a comment.

(defun my/is-after-closing-paren ()
  "Returns t if point is after a closing paren. Ignores strings and comments."
  (save-excursion
    (backward-char)
    (let ((parse-state (syntax-ppss)))
      (and (looking-at ")")
           (null (nth 3 parse-state))      ;; Return nil if point is inside a string.
           (null (nth 4 parse-state))))))  ;; Return nil if point is inside a comment.

(defun my/opening-paren-position ()
  "Gets the position of the opening paren."
  (nth 1 (syntax-ppss)))

(defun my/closing-paren-position ()
  "Gets the position of the closing paren."
  (scan-lists (nth 1 (syntax-ppss)) 1 0))

(defun my/distance-from-opening-paren ()
  "Gets the distance between point and the opening paren."
  (if (my/is-at-opening-paren)
      0
    (- (point) (my/opening-paren-position))))

(defun my/list-root-position (&optional max-depth)
  "Gets the position of the root list starting from point."
  (if (null max-depth) (setq max-depth 100))
  (let ((pos (or (my/string-beginning-position) (point))))
    (condition-case nil
        (dotimes (n max-depth)
          (setq pos (scan-lists pos -1 1)))
      (scan-error pos))))

(defun my/is-list-marked ()
  "Returns t if a list is marked exactly from it's opening to closing paren."
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end))
          (parse-state (syntax-ppss)))
      (and (string= (my/string-at start) "(")
           (null (nth 3 parse-state))
           (null (nth 4 parse-state))
           (= end (scan-lists start 1 0))))))


;; # Destructive

(defun my/goto-opening-paren ()
  "Moves point to opening paren."
  (interactive)
  (goto-char (nth 1 (syntax-ppss))))

(defun my/goto-root-list (&optional max-depth)
  "Moves point to the opening paren of the parent-most (aka, root) list."
  (interactive)
  (goto-char (my/list-root-position)))
