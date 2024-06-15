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
         (let ((root-pos (my/root-list-position)))
           (eval-region root-pos (scan-lists root-pos 1 0) t)))
        (t
         (eval-region (line-beginning-position) (line-end-position) t)))
  (my/flash-mode-line))

(defun my/eval-here ()
  "Evaluates the most immediate list at point."
  (interactive)
  (let* ((origin (point))
         (position* (point))
         (eval (lambda ()
                 (let ((start (scan-lists (point) -1 1))
                       (end (scan-lists (point) 1 1)))
                   (save-excursion
                     (goto-char origin)  ;; Always execute eval from the starting point.
                     (eval-region start end t)
                     (if (/= (point) origin)
                         (setq position* (point))))))))
    (condition-case nil
        (funcall eval)
      (scan-error (save-excursion
                    (backward-up-list nil t t)
                    (funcall eval))))
    (goto-char position*)))

(defun my/eval-kill-ring ()
  "Evals the car of the kill ring."
  (interactive)
  (let ((code (car kill-ring)))
    (if (and code (stringp code))
        (message "%s" (eval (read code))))))

(defun my/lisp-forward-sexp ()
  "Like forward-sexp, but moves point to the first character of the sexp."
  (interactive)
  (condition-case nil (forward-sexp) (scan-error nil))
  (condition-case nil (forward-sexp) (scan-error nil))
  (condition-case nil (backward-sexp) (scan-error nil)))

(defun my/lisp-kill-ring-save-dwim ()
  (interactive)
  (if (use-region-p)
      (kill-ring-save nil nil t)
    (save-excursion
      (let ((n 0))
        (while (and (< n 20)
                    (not (eq (char-after) ?\()))
          (condition-case nil
              (backward-up-list 1 t)
            (scan-error (if (looking-back ")")
                            (backward-char))))
          (setq n (+ n 1))))
      (push-mark (point) t t)
      (forward-list 1 t)
      (kill-ring-save nil nil t)))
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
  (interactive)
  (paredit-close-round-and-newline)
  (paredit-open-round))

(defun my/wrap-sexp ()
  "Like paredit-wrap-sexp, but moves to the beginning of the sexp, then wraps.
Also works from inside strings."
  (interactive)
  (let ((parse-state (syntax-ppss)))
    (if (nth 3 parse-state)              ;; Check if point is inside a string.
        (goto-char (nth 8 parse-state))  ;; Move point to beginning of string.
      (if (not (my/is-at-opening-parens))
          (thing-at-point--beginning-of-sexp))))
  (paredit-wrap-round))

(defun my/transpose-chars ()
  "Like transpose-chars, but calls transpose-sexps if point is on an opening delimiter."
  (interactive)
  (if (my/is-at-opening-parens)
      (call-interactively 'transpose-sexps)
    (call-interactively 'transpose-chars)))
