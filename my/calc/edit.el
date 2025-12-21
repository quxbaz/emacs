;; -*- lexical-binding: t; -*-
;;
;; Calc edit mode operations


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
  (calc-edit-finish))

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
  "Applies square root to the preceeding expression or to the expression in region.
Treats / as a separator (only applies sqrt after /), but keeps x:y together."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (expr (buffer-substring-no-properties start end)))
        (delete-region start end)
        (insert (format "sqrt(%s)" expr))
        (deactivate-mark))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "a-zA-Z0-9:._")
                    (point)))
           (expr (buffer-substring-no-properties start end)))
      (if (> (length expr) 0)
          (progn
            (delete-region start end)
            (insert (format "sqrt(%s)" expr)))
        (insert "sqrt()")
        (backward-char)))))

(defun my/calc-edit-insert-sqrt ()
  "Inserts sqrt()."
  (interactive)
  (insert "sqrt()")
  (backward-char))

(defun my/calc-edit-ln ()
  "Applies natural log to the preceeding expression or to the expression in region.
Treats / as a separator (only applies sqrt after /), but keeps x:y together."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (expr (buffer-substring-no-properties start end)))
        (delete-region start end)
        (insert (format "ln(%s)" expr))
        (deactivate-mark))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "a-zA-Z0-9:._")
                    (point)))
           (expr (buffer-substring-no-properties start end)))
      (if (> (length expr) 0)
          (progn
            (delete-region start end)
            (insert (format "ln(%s)" expr)))
        (insert "ln()")
        (backward-char)))))

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

(provide 'my/calc/edit)
