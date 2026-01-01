;;
;; Generalizing factor function. Needs more testing.
;;

;; TODO: Add comments.
(defmacro my/calc-apply-sel-or-top (bindings options &rest body)
  (declare (indent 2))
  (let ((sym-expr (or (nth 0 bindings) (gensym)))
        (sym-replace-expr (or (nth 1 bindings) (gensym)))
        (sym-sel-is-active (or (nth 2 bindings) (gensym)))
        (opt-m (or (car (alist-get 'm options)) 1))
        (opt-prefix (or (car (alist-get 'prefix options)) ""))
        ;; TODO: Fix this. Default to `t` when not assigned.
        ;; (keep-point (or (car (alist-get 'keep-point options)) t))
        (opt-keep-point nil))
    `(let ((,sym-sel-is-active (my/calc-active-selection-p))
           (saved-point (point)))
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
       (if ,opt-keep-point
           (setf (point) saved-point)))))

(defun my/calc-factor-by ()
  (interactive)
  (my/calc-apply-sel-or-top (expr replace-expr sel-is-active) ((prefix "fctr") (m 2) (keep-point t))
    (my/calc-dont-simplify
     (let* ((factor (calc-top-n 1))
            (divided (math-simplify (calcFunc-nrat (calcFunc-expand (calcFunc-div expr factor)))))
            (product (calcFunc-mul factor divided)))
       (calc-wrapper
        (replace-expr product)
        (calc-pop-stack 1))))))

(my/calc-apply-sel-or-top (expr replace-expr) ((m 2) (prefix "fctr"))
  (my/calc-dont-simplify
   (let* ((factor (calc-top-n 1))
          (divided (math-simplify (calcFunc-nrat (calcFunc-expand (calcFunc-div expr factor)))))
          (product (calcFunc-mul factor divided)))
     (my/preserve-point
      (calc-wrapper
       (calc-pop-stack 1)
       (replace-expr product))))))
