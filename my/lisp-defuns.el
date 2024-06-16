;; Like defuns.el, but for lisp-related stuff


(defun my/mark-list ()
  (interactive)
  (if (/= (char-after) ?\()
      (backward-up-list 1 t t))
  (mark-sexp))

(defun my/eval-dwim ()
  "Evals either the current region, block, or line - in that order of preference."
  (interactive)
  (cond ((use-region-p)
         (eval-region (region-beginning) (region-end) t))
        ((my/is-inside-list)
         (let ((root-pos (my/list-root-position)))
           (eval-region root-pos (scan-lists root-pos 1 0) t)))
        (t
         (eval-region (line-beginning-position) (line-end-position) t)))
  (my/flash-mode-line))

(defun my/eval-here ()
  "Evaluates the most immediate list at point."
  (interactive)
  (let ((thing (if (my/is-inside-list) 'list 'sexp)))
    (eval-expression (read (thing-at-point thing)))))

(defun my/eval-kill-ring ()
  "Evals the car of the kill ring."
  (interactive)
  (let ((code (car kill-ring)))
    (when (and code (stringp code))
      (eval-expression (read code))
      (my/flash-mode-line))))

(defun my/lisp-forward-sexp ()
  "Like forward-sexp, but moves point to the first character of the sexp."
  (interactive)
  (condition-case nil (forward-sexp) (scan-error nil))
  (condition-case nil (forward-sexp) (scan-error nil))
  (condition-case nil (backward-sexp) (scan-error nil)))

(defun my/lisp-kill-ring-save-dwim ()
  "Like kill-ring-save, but saves the current list if possible."
  (interactive)
  (cond ((use-region-p)
         (kill-ring-save nil nil t))
        ((my/is-inside-list)
         (kill-new (thing-at-point 'list)))
        (t
         (call-interactively 'kill-ring-save)))
  (message (car kill-ring)))

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

(defun my/duplicate-list (&optional arg)
  "Duplicates the current list. Uses dwim behavior in certain contexts."
  (interactive "p")
  ;; If region is active, or point is outside a list, or point is inside a comment,
  ;; use my/duplicate-dwim.
  (if (or (use-region-p)
          (null (nth 1 (syntax-ppss)))
          (my/is-inside-comment))
      (call-interactively 'my/duplicate-dwim)
    (let ((origin (point))
          (offset (my/distance-from-opening-parens)))
      (if (not (my/is-at-opening-parens))
          (my/goto-opening-parens))
      (if (my/is-at-opening-parens)
          (let* ((start (point))
                 (NULL (forward-sexp))
                 (end (point))
                 (text (buffer-substring-no-properties start end)))
            (newline)
            (insert text)
            (indent-for-tab-command)
            (backward-list 1 t)
            (forward-char offset))))))

(defun my/append-new-round ()
  "Creates a sibling round."
  (interactive)
  (up-list 1 t t)
  (paredit-open-round))

(defun my/insert-new-round ()
  "Creates a sibling round behind point."
  (interactive)
  (up-list -1 t t)
  (paredit-open-round))

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
  (let ((parse-state (syntax-ppss)))
    (cond ((nth 3 parse-state)  ;; If point is inside a string, move to opening quote.
           (goto-char (nth 8 parse-state)))
          ;; Noop on any of the following conditions.
          ((or (nth 4 parse-state)  ;; Is point inside a comment.
               (eq (point) (line-beginning-position))
               (my/is-at-opening-parens)
               (looking-back "[[:blank:]]")
               (looking-back "\\s\("))
           nil)
          (t (thing-at-point--beginning-of-sexp))))
  (paredit-wrap-round))

(defun my/transpose-chars ()
  "Like transpose-chars, but calls transpose-sexps if point is on an opening delimiter."
  (interactive)
  (if (my/is-at-opening-parens)
      (call-interactively 'transpose-sexps)
    (call-interactively 'transpose-chars)))


;; Macros

(defmacro my/if-buffer-changes (body then &optional else)
  "Executes BODY. If the execution of BODY causes any change in the buffer,
execute THEN. Otherwise execute ELSE."
  (declare (indent 1))
  `(let* ((get-buffer-content (lambda () (buffer-substring-no-properties (point-min) (point-max))))
          (buffer-before (funcall get-buffer-content)))
     ,body
     (if (string= (funcall get-buffer-content) buffer-before)
         ,else
       ,then)))
