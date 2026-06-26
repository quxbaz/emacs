;; Lisp commands
;;
;;


;; # Misc

(defun my/toggle-emacs-lisp-mode ()
  (interactive)
  (if (eq major-mode 'emacs-lisp-mode)
      (call-interactively 'normal-mode)
    (emacs-lisp-mode)))


;; # Evaluation

(cl-defun my/eval-dwim--dispatch (arg &key region defun buffer)
  "Shared dispatch for the eval-dwim commands.
ARG is the raw prefix argument.  :REGION is called with START and END to
evaluate the text of that region; :DEFUN and :BUFFER take no arguments and
evaluate the enclosing defun / whole buffer respectively.  Each branch flashes
the region it acted on, and the mode line flashes at the end."
  (cl-flet ((eval-and-flash (start end)
              (funcall region start end)
              (my/flash-region start end)))
    (cond
     ;; C-u: delegate to the defun evaluator.
     ((equal arg '(4))
      (funcall defun)
      (my/flash-region (save-excursion (beginning-of-defun) (point))
                       (save-excursion (end-of-defun) (point))))
     ;; C-u C-u: eval the entire buffer.
     ((equal arg '(16))
      (funcall buffer)
      (message "%s" (buffer-name))
      (my/flash-region (point-min) (point-max)))
     ;; Active region: eval the selected region.
     ((use-region-p)
      (eval-and-flash (region-beginning) (region-end)))
     ;; Inside any list: eval the enclosing top-level form.
     ((my/is-inside-list)
      (let ((root-pos (my/list-root-position)))
        (eval-and-flash root-pos (scan-lists root-pos 1 0))))
     ;; At a top-level opening paren: eval that sexp.
     ((my/is-at-opening-paren)
      (eval-and-flash (point) (scan-lists (point) 1 0)))
     ;; Just after a top-level closing paren: eval the preceding sexp.
     ((my/is-after-closing-paren)
      (let ((opening-paren-pos (save-excursion (backward-char) (my/opening-paren-position))))
        (eval-and-flash opening-paren-pos (point))))
     ;; Top-level comment: noop.
     ((my/is-inside-comment)
      (message "noop"))
     ;; Fallback: eval the sexp at point if any.
     (t
      (if-let ((bounds (bounds-of-thing-at-point 'sexp)))
          (eval-and-flash (car bounds) (cdr bounds))
        (message "noop")))))
  (my/flash-mode-line))

(defun my/eval-dwim (&optional arg)
  "Evals either the current region, block, or line - in that order of preference.
With one C-u prefix, calls `eval-defun' (instruments for edebug).
With two C-u prefixes, evals the entire buffer."
  (interactive "P")
  (my/eval-dwim--dispatch
   arg
   :region (lambda (start end) (eval-region start end t))
   :defun (lambda () (call-interactively 'eval-defun))
   :buffer #'eval-buffer))

(defun my/slime-eval-dwim (&optional arg)
  "Like `my/eval-dwim', but evaluates Common Lisp forms through SLIME.
Evals either the current region, block, or line - in that order of preference.
With one C-u prefix, calls `slime-eval-defun'.
With two C-u prefixes, evals the entire buffer."
  (interactive "P")
  (my/eval-dwim--dispatch
   arg
   ;; Send the region's text to the CL image and echo the result;
   ;; slime-interactive-eval is the SLIME analog of eval-region's print behavior.
   :region (lambda (start end)
             (slime-interactive-eval (buffer-substring-no-properties start end)))
   :defun (lambda () (call-interactively 'slime-eval-defun))
   :buffer #'slime-eval-buffer))

(defun my/eval-buffer-show-messages ()
  "Eval the entire buffer, flash it, and show *Messages* in the right window."
  (interactive)
  (message "=== eval-buffer: %s ===" (buffer-name))
  (eval-buffer)
  (my/flash-region (point-min) (point-max))
  (when (<= (length (window-list)) 2)
    ;; With 2 windows, keep the current window on the left.
    (when (= (length (window-list)) 2)
      (let ((cur-win (selected-window))
            (other-win (next-window)))
        (when (> (car (window-edges cur-win)) (car (window-edges other-win)))
          (window-swap-states cur-win other-win)
          (select-window other-win))))
    ;; display-buffer sets the window's quit-restore parameter, so quitting
    ;; *Messages* deletes the window if it was created here, or restores the
    ;; window's previous buffer otherwise.
    (let ((win (display-buffer
                "*Messages*"
                '((display-buffer-reuse-window
                   display-buffer-use-some-window
                   display-buffer-in-direction)
                  (direction . right)
                  (inhibit-same-window . t)))))
      (set-window-point win (point-max)))))

(cl-defun my/eval-here--dispatch (&key eval)
  "Shared dispatch for the eval-here commands.
Find the text of the most immediate list at point -- or, inside a
string, the enclosing list; or, outside any list, the sexp at point --
and pass it to EVAL, the evaluator supplied by each call site."
  (cond ((and (my/is-inside-string) (my/is-inside-list))
         (funcall eval (buffer-substring-no-properties
                        (my/opening-paren-position)
                        (my/closing-paren-position))))
        ((my/is-inside-list)
         (funcall eval (thing-at-point 'list)))
        (t
         (funcall eval (thing-at-point 'sexp)))))

(defun my/eval-here ()
  "Evaluates the most immediate list at point."
  (interactive)
  (my/eval-here--dispatch
   :eval (lambda (text) (eval-expression (read text)))))

(defun my/slime-eval-here ()
  "Like `my/eval-here', but evaluates the most immediate list at point
through SLIME."
  (interactive)
  (my/eval-here--dispatch
   :eval (lambda (text) (slime-interactive-eval text))))

(defun my/eval-kill-ring ()
  "Evals the car of the kill ring. Surrounds the string with parens when needed."
  (interactive)
  (let ((code (car kill-ring)))
    (when (and code (stringp code))
      (if (not (string= (substring code 0 1) "("))
          (setq code (format "(%s)" code)))
      (eval-expression (read code))
      (my/flash-mode-line))))

(defun my/macroexpand-here ()
  "Macroexpand the sexp at point."
  (interactive)
  (if (my/is-inside-list)
      (when-let ((sexp (thing-at-point 'list)))
        (pp-macroexpand-expression (read sexp)))
    (when-let ((sexp (thing-at-point 'sexp)))
      (pp-macroexpand-expression (read sexp)))))


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
Also works from inside strings. Call twice to wrap the parent list."
  (interactive)
  (cond ((eq last-command 'my/wrap-sexp)
         (call-interactively 'paredit-splice-sexp)
         (call-interactively 'my/up-wrap-list))
        (t
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
           (forward-char (+ (- origin (point)) 1))))))

(defun my/up-wrap-list ()
  "Wraps the parent list in parens."
  (interactive)
  (paredit-backward-up)
  (paredit-wrap-sexp))

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
             (kill-new (buffer-substring-no-properties (my/opening-paren-position) (my/closing-paren-position)))
           (kill-new (thing-at-point 'list))))
        (t (call-interactively 'kill-ring-save)))
  (message "%s "(car kill-ring)))

(defun my/duplicate-list (&optional arg)
  "Duplicates the current list. Uses dwim behavior in certain contexts."
  (interactive "p")
  ;; If region is active, or point is outside a list, or point is inside a comment,
  ;; use duplicate-dwim.
  (if (or (use-region-p)
          (null (nth 1 (syntax-ppss)))
          (my/is-inside-comment))
      (call-interactively 'duplicate-dwim)
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
  (if (or (my/is-inside-list) (my/is-inside-string))
      (condition-case nil
          (progn (backward-up-list 1 t t) (kill-sexp))
        (scan-error nil))
    (my/kill-block arg)))

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


;; # SLIME

(defun my/slime-load-this-file ()
  "Like slime-load-file, but loads the current buffer's file without asking.
Saves the buffer first if it has unsaved changes."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p)
    (save-buffer))
  (slime-load-file buffer-file-name))

(defun my/slime-help-dwim ()
  "Displays documentation for the function at point."
  (interactive)
  (call-interactively 'slime-describe-function)
  ;; The window selection only works when a delay is used, hence run-with-idle-timer.
  (run-with-idle-timer 0.02 nil (lambda () (select-window (get-buffer-window "*slime-description*")))))
