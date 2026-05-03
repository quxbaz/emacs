;; -*- lexical-binding: t; -*-
;;
;; Calc utility functions


(defmacro my/calc-without-simplification (&rest body)
  "Run BODY with calc-simplify-mode set to \\='none, restoring it afterwards.
Uses unwind-protect so the mode is restored even if BODY signals an error."
  (declare (indent 0))
  (let ((saved (gensym)))
    `(let ((,saved calc-simplify-mode))
       (unwind-protect
           (progn (setq calc-simplify-mode 'none) ,@body)
         (setq calc-simplify-mode ,saved)
         (when (fboundp 'calc-set-mode-line)
           (calc-set-mode-line))))))

(defun my/calc-point-is-at-home-p ()
  "Return t if point is past the last stack entry (at the . line or below).
Used to determine whether my/calc-edit-dwim should open a new entry or edit
an existing one.  calc-locate-cursor-element returns 0 at the . line and -1
below it; stack entries return 1 or higher."
  (<= (calc-locate-cursor-element (point)) 0))

(defun my/calc-point-is-at-entry-end-p ()
  "Return t if point is at the end of a stack entry line.
Returns nil if point is at the . line, below it, or before the end of
the displayed formula."
  (and (> (calc-locate-cursor-element (point)) 0)
       (eolp)))

(defun my/calc-at-stack-bottom-p ()
  "Returns t if point is at the stack bottom or beyond it."
  (<= (calc-locate-cursor-element (point)) 1))

(defun my/calc-no-selection-p ()
  "Returns t if there are no active selections."
  (not (my/calc-active-selection-p)))

(defun my/calc-active-selection-p ()
  "Returns t if there are any active selections."
  (and calc-use-selections
       ;; If any stack element has a non-nil value at (nth 2 elt), then
       ;; selection is active.
       (seq-some (lambda (elt) (nth 2 elt)) calc-stack)))

(defun my/calc-subformula-at-point ()
  "Return the sub-formula under point, ignoring any existing selection.
Unlike `calc-auto-selection', which returns the active selection if one
exists, this always auto-detects based on cursor position. Returns nil
if no sub-formula is found.

How it works:

  1. Find which stack entry point is on. `calc-locate-cursor-element'
     returns the position (1=top, 2=second, etc.); `calc-top' fetches
     the entry data.

  2. Build a map from screen positions back to formula parts.
     `calc-prepare-selection' re-renders the formula with each
     sub-expression tagged, then caches that tagged tree plus the
     column where the formula text starts. Like a heatmap: every
     character cell on screen is now linked to the sub-expression
     that produced it.

  3. Look up the cell under point. `calc-find-selected-part' uses
     `(current-column)' minus the cached offset to walk the tagged
     tree and find the smallest sub-expression covering that spot.
     `calc-grow-assoc-formula' then expands outward through
     associative operators -- so pointing at `b' in `a + b + c'
     returns the whole `a + b + c' rather than just `b'."
  (let* ((num (max 1 (calc-locate-cursor-element (point))))
         (entry (calc-top num 'entry)))
    (calc-prepare-selection num)
    (calc-grow-assoc-formula (car entry) (calc-find-selected-part))))

(defun my/calc-active-selection-at-line-p ()
  "Returns t if there are any active selections at point."
  (let ((m (calc-locate-cursor-element (point))))
    (nth 2 (nth m calc-stack))))

(defun my/calc-first-active-entry ()
  "Return the first stack entry with an active selection beginning from
the top of the stack, or nil if there are no active selections."
  ;; If any stack element has a non-nil value at (nth 2 elt), then selection is
  ;; active.
  (seq-find (lambda (elt) (nth 2 elt)) calc-stack))

(defun my/calc-first-active-entry-m ()
  "Return the POSITION of the first stack entry with an active selection
beginning from the top of the stack, or nil if there are no active selections."
  ;; If any stack element has a non-nil value at (nth 2 elt), then selection is
  ;; active.
  (cl-loop for i from 1 below (length calc-stack)
           when (nth 2 (nth i calc-stack))
           return i))

(defun my/calc-active-entry-m-dwim ()
  "Return stack POSITION of active selection at point, or top-most selection.

Returns the stack position (m) of the active selection at the current
line. If no selection exists at point, returns the position of the first
active selection from the top of the stack. Returns nil if no selections
are active."
  ;; If any stack element has a non-nil value at (nth 2 elt), then selection is
  ;; active.
  (if (my/calc-active-selection-at-line-p)
      (calc-locate-cursor-element (point))
    (my/calc-first-active-entry-m)))

(defmacro my/calc-replace-expr-dwim (bindings options &rest body)
  "Execute BODY with bindings for operating on calc selections or stack entries.

Provides a unified interface for calc operations. The target expression is
chosen based on context, in priority order:

  1. The active calc selection, if any.
  2. The whole stack entry, if point is at the end of an entry line.
  3. The sub-formula under point, if point is on a stack entry.
  4. The stack item at OPT-M (default 1), if point is at \"home\".

BINDINGS is a list of binding names to expose inside BODY. Order does not
matter; any subset may be requested. Unknown names raise an error.

    expr             The target expression (selection or stack formula)
    replace-expr     Function that replaces EXPR with a new expression
    top              The top stack item (calc-top-n 1), nil if unbound
    sel-is-active    t if operating on a selection, nil otherwise

Unrequested bindings are auto-gensymmed and their values are not computed.

OPTIONS is an alist of (SYMBOL VALUE) pairs:

    (m N)                  Stack level when no selection (default: 1)
    (prefix STRING)        Calc trail prefix (default: \"\")
    (keep-point BOOL)      Preserve point unless set to -1 (default: t)
    (calc-wrapper BOOL)    Wrap body in calc-wrapper (default: t)
    (simp VAL)             nil = use current setting (default),
                           -1  = disable via my/calc-without-simplification

EXAMPLES

    ;; Takes the absolute value of either the ACTIVE SELECTION or TOP STACK ENTRY.
    (my/calc-replace-expr-dwim (expr replace-expr) ()
      (replace-expr (calcFunc-abs expr)))

    ;; Same as previous example (calc-wrapper wraps body by default).
    (my/calc-replace-expr-dwim (expr replace-expr) ((prefix \"abs\"))
      (let ((result (calcFunc-abs expr)))
        (replace-expr result)))

    ;; Takes the square root of either the ACTIVE SELECTION or SECOND STACK ENTRY (m=2).
    (my/calc-replace-expr-dwim (expr replace-expr) ((m 2) (prefix \"sqrt\"))
      (let ((result (calcFunc-sqrt expr)))
        (replace-expr result)))

    ;; Opt out of calc-wrapper to manage it manually.
    (my/calc-replace-expr-dwim (expr replace-expr) ((prefix \"abs\") (calc-wrapper nil))
      (let ((result (calcFunc-abs expr)))
        (calc-wrapper
         (replace-expr result))))

    ;; Disable simplification while operating (e.g. to preserve unevaluated form).
    (my/calc-replace-expr-dwim (expr replace-expr) ((prefix \"foo\") (simp -1))
      (replace-expr (calcFunc-foo expr)))
"
  (declare (indent 2))
  ;; Convert `options` to alist for convenience:
  ;;   ((a 1) (b 2)) -> ((a . 1) (b . 2))
  (setq options (mapcar (lambda (pair) (cons (car pair) (cadr pair))) options))
  ;; Validate BINDINGS: only known names allowed.
  (let ((unknown (cl-set-difference bindings '(expr replace-expr top sel-is-active))))
    (when unknown
      (error "Unknown binding(s) in my/calc-replace-expr-dwim: %s" unknown)))
  (let (;; BINDING PARAMS
        ;;
        ;; If the user requested a binding by name, use that name as-is so it
        ;; is visible inside BODY. Otherwise, gensym so it is invisible.
        (sym-expr (if (memq 'expr bindings) 'expr (gensym)))
        (sym-replace-expr (if (memq 'replace-expr bindings) 'replace-expr (gensym)))
        (sym-top (if (memq 'top bindings) 'top (gensym)))
        (sym-sel-is-active (if (memq 'sel-is-active bindings) 'sel-is-active (gensym)))
        ;; OPTION PARAMS
        ;; The stack level to target. Only relevant when selection is inactive.
        ;; Default is 1 (the top stack entry).
        (opt-m (alist-get 'm options 1))
        ;; The prefix to use in the calc trail. Defaults to the empty string "".
        (opt-prefix (alist-get 'prefix options ""))
        ;;  Controls point preservation. Preserve point unless set to -1 (default: t).
        (opt-keep-point (alist-get 'keep-point options t))
        ;; Whether to wrap body in calc-wrapper (default: t).
        (opt-calc-wrapper (alist-get 'calc-wrapper options t))
        ;; Simplification control. nil = use current setting,
        ;; -1 = wrap with my/calc-without-simplification.
        (opt-simp (alist-get 'simp options nil)))
    (let* ((wrapped-body (if (eq opt-simp -1) `((my/calc-without-simplification ,@body)) body))
           (wrapped-body (if opt-calc-wrapper `((calc-wrapper ,@wrapped-body)) wrapped-body)))
      `(let ((,sym-sel-is-active (my/calc-active-selection-p))  ;; Bind to t if selection is active, otherwise nil.
             (,sym-top ,(and (memq 'top bindings) '(calc-top-n 1)))  ;; Top stack item, evaluated only if requested.
             (keep-args calc-keep-args-flag)
             (saved-point (point)))  ;; Restore point later if `opt-keep-point` is true.
         (prog1
             (cond
              ;; Selection is active. Operate on active selection.
              (,sym-sel-is-active
               (let* ((m (my/calc-active-entry-m-dwim))
                      (entry (nth m calc-stack))
                      (,sym-expr (nth 2 entry)))
                 (cl-flet ((,sym-replace-expr (new-expr)
                             (let ((new-formula (calc-replace-sub-formula (car entry) ,sym-expr new-expr)))
                               (calc-pop-push-record-list 1 ,opt-prefix new-formula m new-expr))))
                   ,@wrapped-body)))
              ;; Point is on a stack entry. Operate on the sub-formula at
              ;; point, or the whole entry if point is at end of line.
              ((not (my/calc-point-is-at-home-p))
               (let* ((m (calc-locate-cursor-element (point)))
                      (entry (nth m calc-stack))
                      (,sym-expr (if (eolp) (car entry) (my/calc-subformula-at-point))))
                 (cl-flet ((,sym-replace-expr (new-expr)
                             (let ((new-formula (calc-replace-sub-formula (car entry) ,sym-expr new-expr)))
                               (calc-pop-push-record-list 1 ,opt-prefix new-formula m))))
                   ,@wrapped-body)))
              ;; Point is at "home" position. Operate on stack item at OPT-M.
              (t
               (let ((,sym-expr (car (calc-top ,opt-m 'entry))))
                 (cl-flet ((,sym-replace-expr (new-expr)
                             (calc-pop-push-record-list 1 ,opt-prefix new-expr (if keep-args 1 ,opt-m))))
                   ,@wrapped-body))))
           ;; POINT BEHAVIOR:
           ;; 1. If calc selection is active, always preserve point.
           ;; 2. if keep-args is active, always reset point.
           ;; 3. Else, `opt-keep-point` dictates behavior.
           (cond (,sym-sel-is-active
                  (goto-char saved-point))
                 ((or keep-args (eq ,opt-keep-point -1))
                  (calc-align-stack-window))
                 (t
                  (goto-char saved-point))))))))

(defun my/calc-edit-wrap-dwim (fn)
  "Wrap the preceding token or active region with FN(...).

With region active:
  [x+1] -> fn(x+1)

With token before point:
  x+1| -> x+fn(1)

With nothing before point:
  | -> fn()|"
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (expr (buffer-substring-no-properties start end)))
        (delete-region start end)
        (insert (format "%s(%s)" fn expr))
        (deactivate-mark))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "a-zA-Z0-9:._")
                    (point)))
           (expr (buffer-substring-no-properties start end)))
      (if (> (length expr) 0)
          (progn
            (delete-region start end)
            (insert (format "%s(%s)" fn expr)))
        (insert (format "%s()" fn))
        (backward-char)))))

(provide 'my/calc/lib)
