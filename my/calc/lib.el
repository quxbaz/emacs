;; -*- lexical-binding: t; -*-
;;
;; Calc utility functions


(defun my/calc-at-stack-bottom-p ()
  "Returns t if point is at the stack bottom or beyond it."
  (<= (calc-locate-cursor-element (point)) 1))

(defun my/calc-no-selection-p ()
  "Returns t if there are no active selections."
  (not (my/calc-no-selection-p)))

(defun my/calc-active-selection-p ()
  "Returns t if there are any active selections."
  (and calc-use-selections
       ;; If any stack element has a non-nil value at (nth 2 elt), then
       ;; selection is active.
       (seq-some (lambda (elt) (nth 2 elt)) calc-stack)))

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
  "Return stack POSITION of active selection at point, or topmost selection.

Returns the stack position (m) of the active selection at the current
line. If no selection exists at point, returns the position of the first
active selection from the top of the stack. Returns nil if no selections
are active."
  ;; If any stack element has a non-nil value at (nth 2 elt), then selection is
  ;; active.
  (if (my/calc-active-selection-at-line-p)
      (calc-locate-cursor-element (point))
    (my/calc-first-active-entry-m)))

(defmacro my/calc-apply-sel-or-top (bindings options &rest body)
  "Execute BODY with bindings for operating on calc selections or stack entries.

Provides a unified interface for calc operations that work on either a selected
sub-expression or a stack entry, automatically handling both cases.

BINDINGS is a list of up to 3 symbols to bind in BODY:

    (EXPR REPLACE-EXPR SEL-IS-ACTIVE)

    EXPR             The target expression (selection or stack formula)
    REPLACE-EXPR     Function that replaces EXPR with a new expression
    SEL-IS-ACTIVE    t if operating on a selection, nil otherwise

OPTIONS is an alist of (SYMBOL VALUE) pairs:

    (m N)                Stack level when no selection (default: 1)
    (prefix STRING)      Calc trail prefix (default: \"\")
    (keep-point BOOL)    Preserve point unless set to -1 (default: t)

EXAMPLE

    (my/calc-apply-sel-or-top (expr replace-expr) ((m 2) (prefix \"sqrt\"))
      (let ((result (calcFunc-sqrt expr)))
        (calc-wrapper
          (replace-expr result))))

Takes the square root of the active selection or stack level 2."
  (declare (indent 2))
  ;; Convert `options` to alist for convenience:
  ;;   ((a 1) (b 2)) -> ((a . 1) (b . 2))
  (setq options (mapcar (lambda (pair) (cons (car pair) (cadr pair))) options))
  (let (;; BINDING PARAMS
        ;; Bound to the target expression. This is either the selection or stack
        ;; formula depending on if a selection is active.
        (sym-expr (or (nth 0 bindings) (gensym)))
        ;; This is a function. The argument (an expression) replaces the target expression.
        (sym-replace-expr (or (nth 1 bindings) (gensym)))
        ;; Bound to t if the target expression is a selection.
        (sym-sel-is-active (or (nth 2 bindings) (gensym)))
        ;; OPTION PARAMS
        ;; The stack level to target. Only relevant when selection is inactive.
        ;; Default is 1 (the top stack entry).
        (opt-m (alist-get 'm options 1))
        ;; The prefix to use in the calc trail. Defaults to the empty string "".
        (opt-prefix (alist-get 'prefix options ""))
        ;;  Controls point preservation. Preserve point unless set to -1 (default: t).
        (opt-keep-point (alist-get 'keep-point options t)))
    `(let ((,sym-sel-is-active (my/calc-active-selection-p))  ;; Bind to t if selection is active, otherwise nil.
           (keep-args calc-keep-args-flag)
           (saved-point (point)))  ;; Restore point later if `opt-keep-point` is true.
       (prog1 ;; Return value from `body`.
           (cond (,sym-sel-is-active
                  (let* ((m (my/calc-active-entry-m-dwim))
                         (entry (nth m calc-stack))
                         (,sym-expr (nth 2 entry)))
                    (cl-flet ((,sym-replace-expr (new-expr)
                                (let ((new-formula (calc-replace-sub-formula (car entry) ,sym-expr new-expr)))
                                  (calc-pop-push-record-list 1 ,opt-prefix new-formula m new-expr))))
                      ,@body)))
                 (t
                  (let ((,sym-expr (calc-top-n ,opt-m)))
                    (cl-flet ((,sym-replace-expr (new-expr)
                                (calc-pop-push-record-list 1 ,opt-prefix new-expr (if keep-args 1 ,opt-m))))
                      ,@body))))
         ;; Preserve point unless `calc-keep-args-flag` is t or `opt-keep-point` is -1.
         (unless (or keep-args (eq ,opt-keep-point -1))
           (setf (point) saved-point))))))

(provide 'my/calc/lib)
