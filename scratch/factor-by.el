;;
;; Generalizing factor function. Needs more testing.
;;

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
    `(let ((,sym-sel-is-active (my/calc-active-selection-p))  ;; Bind to `t` if selection is active.
           (saved-point (point)))  ;; Restore point later if `opt-keep-point` is true.
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
                            (calc-pop-push-record-list 1 ,opt-prefix new-expr ,opt-m)))
                  ,@body))))
       (unless (eq ,opt-keep-point -1)
         (setf (point) saved-point)))))

(defun my/calc-factor-by ()
  (interactive)
  (my/calc-apply-sel-or-top (expr replace-expr sel-is-active) ((prefix "fctr") (m 2))
    (my/calc-dont-simplify
     (let* ((factor (calc-top-n 1))
            (divided (math-simplify (calcFunc-nrat (calcFunc-expand (calcFunc-div expr factor)))))
            (product (calcFunc-mul factor divided)))
       (calc-wrapper
        (replace-expr product)
        (calc-pop-stack 1))))))
