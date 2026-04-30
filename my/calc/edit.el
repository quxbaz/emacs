;; -*- lexical-binding: t; -*-
;;
;; Calc edit mode operations

(defvar calc-alg-entry-history nil)
(defvar my/calc-edit-saved-point nil)
(defvar my/calc-edit-new-entry nil
  "Non-nil when the edit buffer was opened to create a new stack entry.
my/calc-edit-finish calls calc-align-stack-window instead of restoring point.")
(defvar my/calc-history-index 0)

(defun my/calc-edit (n)
  "Like `calc-edit' but starts with point at end of the expression line.
After my/calc-edit-finish, point is moved to home via calc-align-stack-window."
  (interactive "p")
  (setq my/calc-edit-saved-point (point)
        my/calc-edit-new-entry t)
  (cl-letf (((symbol-function 'calc-align-stack-window) #'ignore))
    (calc-edit n))
  (move-end-of-line nil))

(defun my/calc-edit-selection ()
  "Like `calc-edit-selection' (j`) but fixes an atom-encasing bug.
`calc-edit-selection' captures the formula before calling `calc-auto-selection',
which calls `calc-prepare-selection' which wraps bare atoms — e.g. 5 becomes
\(cplx 5 0) — via `calc-encase-atoms', mutating \(car entry) in place.  If
`calc-auto-selection' then returns nil \(cursor off the formula), the fallback
`expr' is the pre-mutation value, while `calc-top' later returns the
post-mutation value; the `eq' check in `calc-find-sub-formula' fails and
throws \"Original selection has been lost\".  Capturing the fallback as
\(car entry) after the mutation resolves the mismatch."
  (interactive)
  (calc-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
          (calc-sel-reselect calc-keep-selection)
          (entry (calc-top num 'entry))
          (sel (or (calc-auto-selection entry) (car entry)))
          (str (math-showing-full-precision
                (math-format-nice-expr sel (frame-width))))
          (csr calc-sel-reselect))
     (calc--edit-mode (lambda () (calc-finish-selection-edit num sel csr)))
     (insert str "\n")))
  (calc-show-edit-buffer))

(defun my/calc-edit-dwim ()
  "Opens edit mode or edits the current entry."
  (interactive)
  (setq my/calc-edit-saved-point (point)
        my/calc-edit-new-entry (my/calc-point-is-at-home-p))
  ;; Suppress calc-align-stack-window so it doesn't move the window-point
  ;; while opening the edit buffer, which would leave the calc window at the
  ;; wrong position both visually and when the edit buffer is killed.
  (cl-letf (((symbol-function 'calc-align-stack-window) #'ignore))
    (if (my/calc-active-selection-p)
        (call-interactively 'calc-edit)
      (if my/calc-edit-new-entry
          (funcall (kmacro "'`"))
        (my/calc-edit-selection)
        (move-end-of-line nil)))))

(defun my/calc-edit-history-prev ()
  "Recall previous calc history entry."
  (interactive)
  (when calc-alg-entry-history
    (let ((history-length (length calc-alg-entry-history)))
      (setq my/calc-history-index (min (1+ my/calc-history-index) (1- history-length)))
      (goto-char (point-min)) (forward-line 2) (delete-region (point) (point-max))
      (insert (nth my/calc-history-index calc-alg-entry-history)))))

(defun my/calc-edit-history-next ()
  "Recall next calc history entry."
  (interactive)
  (when calc-alg-entry-history
    (goto-char (point-min)) (forward-line 2) (delete-region (point) (point-max))
    (when (> my/calc-history-index 0)
      (setq my/calc-history-index (1- my/calc-history-index))
      (insert (nth my/calc-history-index calc-alg-entry-history)))))

(defun my/calc-vector-edit ()
  "Begins a vector entry."
  (interactive)
  (if (minibufferp)
      (call-interactively 'self-insert-command)
    (setq my/calc-edit-new-entry t)
    (funcall (kmacro "'`"))
    (insert "[]") (backward-char)))

(defun my/calc-edit-finish ()
  "Like calc-edit-finish, but pushes to calc-alg-entry-history."
  (interactive)
  (save-excursion
    (goto-char (point-min)) (forward-line 2) (beginning-of-line)
    (let ((text (string-trim (buffer-substring-no-properties (point) (point-max)))))
      (cond ((string= text "") nil)    ;; Don't save empty strings to history.
            ((string= text "[]") (delete-region (line-beginning-position) (1+ (line-end-position))))
            ((string= text (cadr calc-alg-entry-history)) nil)  ;; Don't save string to history if it's a duplicate of the previous entry.
            (t (push text calc-alg-entry-history)))))
  (calc-edit-finish)
  (when (my/calc-active-selection-p)
    (call-interactively 'calc-clear-selections))
  (if my/calc-edit-new-entry
      (calc-align-stack-window)
    (when my/calc-edit-saved-point
      (goto-char my/calc-edit-saved-point))))

(defun my/calc-edit-newline ()
  "Like newline, but also sets indentation."
  (interactive)
  (newline)
  (if (string= (string-trim (thing-at-point 'line)) "]")
      (delete-horizontal-space)))

(defun my/calc-edit-duplicate (arg)
  "Duplicates the current line. Adds a comma if necessary."
  (interactive "p")
  ;; Add comma to current line if it doesn't have one.
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t")
    (unless (looking-back "," (line-beginning-position))
      (insert ",")))
  ;; Duplicate the line.
  (call-interactively 'my/duplicate-dwim)
  ;; Remove comma from the duplicated line.
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t")
    (when (looking-back "," (line-beginning-position))
      (delete-char -1))))

(defun my/calc-edit-square-dwim ()
  "Inserts ^2. Subsequent invocations increment the exponent value."
  (interactive)
  (if (looking-back "\\^[0-9]+" (line-beginning-position))
      ;; We're right after an exponent, increment it
      (let* ((end (point))
             (start (save-excursion
                      (skip-chars-backward "0-9")
                      (point)))
             (exponent (string-to-number (buffer-substring-no-properties start end))))
        (delete-region start end)
        (insert (number-to-string (1+ exponent))))
    ;; No exponent before point, insert ^2
    (insert "^2")))

(defun my/calc-edit-cube-dwim ()
  "Inserts ^3. Subsequent invocations increment the exponent value."
  (interactive)
  (if (looking-back "\\^[0-9]+" (line-beginning-position))
      ;; We're right after an exponent, increment it
      (let* ((end (point))
             (start (save-excursion
                      (skip-chars-backward "0-9")
                      (point)))
             (exponent (string-to-number (buffer-substring-no-properties start end))))
        (delete-region start end)
        (insert (number-to-string (1+ exponent))))
    ;; No exponent before point, insert ^3
    (insert "^3")))

(defun my/calc-edit-sqrt-dwim ()
  "Applies sqrt() to the preceding expression or to the expression in region."
  (interactive)
  (my/calc-edit-wrap-dwim "sqrt"))

(defun my/calc-edit-ln-dwim ()
  "Applies ln() to the preceding expression or to the expression in region."
  (interactive)
  (my/calc-edit-wrap-dwim "ln"))

(defun my/calc-edit-abs-dwim ()
  "Applies abs() to the preceding expression or to the expression in region."
  (interactive)
  (my/calc-edit-wrap-dwim "abs"))

(defun my/calc-edit--scan-expr-start ()
  "Move point to the start of the preceding expression.
Scans backward, skipping balanced bracket/brace/paren groups (treating
function calls like sqrt(...) as atoms), stopping at unmatched delimiters,
comparison/equation operators, commas, or BOL."
  (while (and (not (bolp))
              (not (memq (char-before) '(?\( ?\[ ?\{ ?= ?< ?> ?, ?/ ?^ ?+ ?-))))
    (cond
     ((memq (char-before) '(?\] ?\} ?\)))
      (backward-sexp)
      (skip-chars-backward "a-zA-Z0-9_"))
     (t
      (backward-char)))))

(defun my/calc-edit--apply-wrap (start end)
  "Insert parens around [START, END) and advance cursor past closing paren."
  (save-excursion
    (goto-char end)   (insert ")")
    (goto-char start) (skip-chars-forward " \t") (insert "("))
  (forward-char 1))

(defun my/calc-edit-wrap-parens ()
  "Wrap the preceding expression (or active region) with parentheses.
Invoked with cursor just after `)', expands the wrapped region instead."
  (interactive)
  (if (use-region-p)
      (progn
        (save-excursion
          (goto-char (region-end))       (insert ")")
          (goto-char (region-beginning)) (insert "("))
        (deactivate-mark))
    (let* ((repeat (and (eq (char-before) ?\))
                        (save-excursion
                          (backward-sexp)
                          (not (looking-back "[a-zA-Z0-9_]" (max (point-min) (1- (point))))))))
           (paren-open (when repeat
                         (save-excursion
                           (backward-sexp)
                           (skip-chars-backward " \t")
                           (point)))))
      (when repeat
        (delete-char -1)
        (save-excursion
          (goto-char paren-open)
          (skip-chars-forward " \t")
          (delete-char 1)))
      (let* ((end   (point))
             (start (save-excursion
                      (when repeat
                        (goto-char paren-open)
                        (unless (bolp)
                          (if (memq (char-before) '(?\] ?\}))
                              (backward-sexp)
                            (backward-char))))
                      (my/calc-edit--scan-expr-start)
                      (point))))
        (when (< start end)
          (my/calc-edit--apply-wrap start end))))))

(defun my/calc-duplicate-paren-expr ()
  "Duplicates the innermost parenthesized expression surrounding point.

The duplicate is inserted immediately after the closing parenthesis.
Point is moved to the corresponding position within the duplicate."
  (interactive)
  (when (> (nth 0 (syntax-ppss)) 0)
    (let ((expr (thing-at-point 'list))
          (offset (my/distance-from-opening-paren)))
      (goto-char (my/closing-paren-position))
      (insert expr) (backward-char)
      (goto-char (+ (my/opening-paren-position) offset))
      ;; (let ((char (read-char-from-minibuffer "Replace character at point: ")))
      ;;   (unless (= char 0)
      ;;     (delete-char 1)
      ;;     (insert char)))
      )))

(defvar-local my/calc-edit-tab-lhs nil
  "Saved LHS position for TAB toggling.")
(defvar-local my/calc-edit-tab-rhs nil
  "Saved RHS position for TAB toggling.")

(defun my/calc-edit-tab ()
  "Toggle between LHS and RHS of an equation, or insert ' = ' if line has none.
Defers to yasnippet when active."
  (interactive)
  (cond
   ((and (bound-and-true-p yas-minor-mode)
         (yas--snippets-at-point))
    (yas-next-field))
   ((and (bound-and-true-p yas-minor-mode)
         (let ((yas-fallback-behavior 'return-nil))
           (yas-expand))))
   ((not (string-match-p "=" (or (thing-at-point 'line t) "")))
    (setq my/calc-edit-tab-lhs (point)
          my/calc-edit-tab-rhs nil)
    (end-of-line)
    (delete-horizontal-space)
    (insert " = "))
   (t
    (let* ((eq-pos (save-excursion
                     (beginning-of-line)
                     (search-forward "=" (line-end-position) t)
                     (1- (point))))
           (on-rhs (> (point) eq-pos)))
      (if on-rhs
          (let ((dest (or my/calc-edit-tab-lhs (line-beginning-position))))
            (setq my/calc-edit-tab-rhs (point))
            (goto-char dest))
        (let ((dest (or my/calc-edit-tab-rhs
                        (save-excursion
                          (goto-char (1+ eq-pos))
                          (skip-chars-forward " \t")
                          (point)))))
          (setq my/calc-edit-tab-lhs (point))
          (goto-char dest)))))))

(defun my/calc-edit-toggle-brackets ()
  "Toggle between parentheses and square brackets at point."
  (interactive)
  (cond ((looking-at "(")
         (delete-char 1) (insert "[") (backward-char 1))
        ((looking-at ")")
         (delete-char 1) (insert "]") (backward-char 1))
        ((looking-at "\\[")
         (delete-char 1) (insert "(") (backward-char 1))
        ((looking-at "\\]")
         (delete-char 1) (insert ")") (backward-char 1))))

(defun my/calc-edit-insert-pi ()
  "Insert \"pi\", preceded by a space if point is after a letter."
  (interactive)
  (when (and (not (bolp)) (string-match-p "[a-zA-Z]" (string (char-before))))
    (insert " "))
  (insert "pi"))

(provide 'my/calc/edit)
