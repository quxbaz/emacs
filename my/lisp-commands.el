;; Lisp commands
;;
;;


;; # Evaluation

(defun my/eval-dwim ()
  "Evals either the current region, block, or line - in that order of preference.

BUG: Does not work inside comments."
  (interactive)
  (cond ((use-region-p)
         (eval-region (region-beginning) (region-end) t))
        ((my/is-inside-list)
         (let ((root-pos (my/list-root-position)))
           (eval-region root-pos (scan-lists root-pos 1 0) t)))
        ((my/is-at-opening-paren)
         (eval-region (point) (scan-lists (point) 1 0) t))
        ((my/is-after-closing-paren)
         (let ((opening-paren-pos (save-excursion (backward-char) (my/opening-paren-position))))
           (eval-region opening-paren-pos (point) t)))
        (t
         ;; NOTE:BUG: This doesn't work for quoted expressions.
         (eval-expression (read (thing-at-point 'sexp)))))
  (my/flash-mode-line))

(defun my/eval-here ()
  "Evaluates the most immediate list at point."
  (interactive)
  (cond ((my/is-inside-list)
         (if (my/is-inside-string)
             (eval-region (my/opening-paren-position) (my/closing-paren-position) t)
           (eval-expression (read (thing-at-point 'list)))))
        (t
         (eval-expression (read (thing-at-point 'sexp))))))

(defun my/eval-kill-ring ()
  "Evals the car of the kill ring. Surrounds the string with parens when needed."
  (interactive)
  (let ((code (car kill-ring)))
    (when (and code (stringp code))
      (if (not (string= (substring code 0 1) "("))
          (setq code (format "(%s)" code)))
      (eval-expression (read code))
      (my/flash-mode-line))))


;; # Navigation

(defun my/forward-sexp ()
  "Like forward-sexp, but moves point to the first character of the sexp."
  (interactive)
  (condition-case nil (forward-sexp) (scan-error nil))
  (condition-case nil (forward-sexp) (scan-error nil))
  (condition-case nil (backward-sexp) (scan-error nil)))


;; # Editing

(defun my/close-round-and-newline ()
  (interactive)
  (when (my/is-inside-string)
    (my/goto-beginning-of-string))
  (call-interactively 'paredit-close-round-and-newline))

(defun my/open-new-round ()
  "Like paredit-close-round-and-newline, but also opens a new round."
  (interactive)
  (let ((parse-state (syntax-ppss)))
    ;; Noop when inside a comment.
    (when (not (nth 4 parse-state))
      ;; If inside a string, move to beginning of string.
      (if (nth 3 parse-state)
          (goto-char (nth 8 parse-state)))
      (paredit-close-round-and-newline)
      (paredit-open-round))))

(defun my/wrap-sexp ()
  "Like paredit-wrap-sexp, but moves to the beginning of the sexp, then wraps.
Also works from inside strings."
  (interactive)
  (let ((parse-state (syntax-ppss))
        (origin (point)))
    (cond ((nth 3 parse-state)  ;; If point is inside a string, move to opening quote.
           (goto-char (nth 8 parse-state)))
          ;; Noop on any of the following conditions.
          ((or (nth 4 parse-state)  ;; Is point inside a comment.
               (eq (point) (line-beginning-position))
               (my/is-at-opening-paren)
               (looking-back "[[:blank:]]")
               (looking-back "\\s\("))
           nil)
          (t (thing-at-point--beginning-of-sexp)))
    (paredit-wrap-round)
    (forward-char (+ (- origin (point)) 1))))

(defun my/lisp-comment-dwim ()
  "Comment out a list if point is on opening round. Otherwise, comment the line."
  (interactive)
  (cond ((use-region-p)
         (call-interactively 'my/comment-line))
        ((my/is-inside-comment)
         (let* ((comment-start (nth 8 (syntax-ppss)))
                (comment-end (+ comment-start 3))
                (next-line-range (save-excursion (next-line) (cons (line-beginning-position) (line-end-position))))
                (next-line-text (string-trim (buffer-substring-no-properties (car next-line-range) (cdr next-line-range)))))
           (delete-region comment-start comment-end)
           ;; If next line is composed only of hanging closing rounds, join it with current line.
           (if (string-match-p "^)\+$" next-line-text)
               (save-excursion (paredit-close-round)))))
        ((my/is-at-opening-paren)
         (mark-sexp)
         (call-interactively 'paredit-comment-dwim)
         (forward-char 3))
        (t
         (call-interactively 'my/comment-line))))

(defun my/lisp-kill-ring-save-dwim ()
  "Like kill-ring-save, but saves the current list if possible."
  (interactive)
  (cond ((use-region-p)
         (kill-ring-save nil nil t))
        ((eq last-command 'my/lisp-kill-ring-save-dwim)
         (kill-ring-save (point-min) (point-max)))
        ((my/is-inside-list)
         (if (my/is-inside-string)
             (kill-new (buffer-substring-no-properties (my/opening-paren-position)
                                                       (my/closing-paren-position)))
           (kill-new (thing-at-point 'list))))
        (t
         (call-interactively 'kill-ring-save)))
  (message "%s "(car kill-ring)))

(defun my/duplicate-list (&optional arg)
  "Duplicates the current list. Uses dwim behavior in certain contexts."
  (interactive "p")
  ;; If region is active, or point is outside a list, or point is inside a comment,
  ;; use my/duplicate-dwim.
  (if (or (use-region-p)
          (null (nth 1 (syntax-ppss)))
          (my/is-inside-comment))
      (call-interactively 'my/duplicate-dwim)
    (let ((offset (my/distance-from-opening-paren))
          (text (buffer-substring-no-properties (my/opening-paren-position)
                                                (my/closing-paren-position))))
      (if (not (my/is-at-opening-paren))
          (my/goto-opening-paren))
      (when (my/is-at-opening-paren)
        (forward-sexp)
        (newline)
        (insert text)
        (indent-for-tab-command)
        (backward-list 1 t)
        (forward-char offset)))))

(defun my/lisp-kill-dwim (arg)
  (interactive "p")
  (if (use-region-p)
      (paredit-kill-region (region-beginning) (region-end))
    (if (= arg 1) (paredit-kill) (paredit-kill arg))))

(defun my/kill-list (arg)
  "Kills a list or string from inside of it."
  (interactive "p")
  (condition-case nil
      (progn (backward-up-list 1 t t) (kill-sexp))
    (scan-error nil)))

(defun my/lisp-transpose-chars ()
  "Like transpose-chars, but calls transpose-sexps if point is on an opening delimiter."
  (interactive)
  (if (my/is-at-opening-paren)
      (progn (call-interactively 'transpose-sexps)
             (thing-at-point--beginning-of-sexp)
             (thing-at-point--beginning-of-sexp))
    (call-interactively 'transpose-chars)))


;; # Marking

(defun my/mark-list (&optional position)
  "Marks the list at point or at a given POSITION."
  (if position
      (goto-char position))
  (if (not (my/is-at-opening-paren))
      (my/goto-opening-paren))
  (mark-sexp))

(defun my/mark-list-command ()
  "Marks the list at point. Invoke again to restore point to origin."
  (interactive)
  (cond ((and (eq last-command 'my/mark-list-command)
              (my/is-list-marked))
         (deactivate-mark)
         (goto-char my/mark-list/origin))
        ((my/is-inside-list)
         (setq-local my/mark-list/origin (point))
         (my/mark-list))))


;; # Common Lisp

(defun my/visit-slime-repl ()
  "Activates SLIME or visits the SLIME buffer in the right-side window."
  (interactive)
  (let ((slime-buffer (get-buffer "*slime-repl sbcl*")))
    (cond (slime-buffer
           (if (= (length (window-list)) 1)
               (split-window-right))
           (my/ignore-error (windmove-right))
           (switch-to-buffer slime-buffer))
          (t
           (slime)))))
