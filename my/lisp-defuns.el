;; Like defuns.el, but for lisp-related stuff


(defun my/mark-list ()
  (interactive)
  (if (/= (char-after) ?\()
      (backward-up-list 1 t t))
  (mark-sexp))

(defun my/eval-dwim ()
  "Evals either the current region, block, or line - in that order of preference."
  (interactive)
  (if (use-region-p)
      (eval-region (region-beginning) (region-end) t)
    (save-excursion
      (my/goto-root-list)
      ;; If the point is at an opening parens, eval the list.
      ;; Else-if point is to the right of a closing parens, eval the list.
      ;; Else eval the line.
      (cond ((looking-at "(")
             (eval-region (point) (scan-lists (point) 1 0) t))
            ((looking-back ")")
             (eval-last-sexp nil))
            (t
             (eval-region (point-at-bol) (point-at-eol) t)))))
  (my/flash-mode-line))

;; BUG: This does not work inside strings.
(defun my/eval-here ()
  "Evaluates the most immediate list at point."
  (interactive)
  (let ((start (scan-lists (point) -1 1))
        (end (scan-lists (point) 1 1)))
    (eval-region start end t)))

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
  (interactive "p")
  ;; Kill the sexp, not the parent sexp when point is on a (.
  (if (eq (char-after) ?\()
      (forward-char))
  (condition-case nil
      (progn (backward-up-list) (kill-sexp))
    (scan-error (my/kill-block arg))))

(defun my/open-new-round ()
  (interactive)
  (paredit-close-round-and-newline)
  (paredit-open-round))
