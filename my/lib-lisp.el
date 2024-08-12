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
  (if (eq max-depth nil) (setq max-depth 100))
  (let ((current-point (point)))
    (save-excursion
      (condition-case nil
          (dotimes (n max-depth)
            (backward-up-list 1 t t)
            (setq current-point (point)))
        (t nil)))
    current-point))

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


;; # Destuctive

(defun my/goto-opening-paren ()
  "Moves point to opening paren."
  (interactive)
  (goto-char (nth 1 (syntax-ppss))))

(defun my/goto-root-list (&optional max-depth)
  "Moves point to opening paren of parent-most (ie, root) list."
  (interactive)
  (if (eq max-depth nil)
      (setq max-depth 100))
  (condition-case nil
      (backward-up-list max-depth t t)
    (scan-error nil)
    (user-error nil)))
