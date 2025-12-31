;;
;; Generalizing factor function. Needs more testing.
;;

(defmacro my/calc-apply-sel-or-top (bindings options &rest body)
  (declare (indent 2))
  (let ((expr (or (nth 0 bindings) (gensym)))
        (replace-expr (or (nth 1 bindings) (gensym)))
        (sel-is-active (or (nth 2 bindings) (gensym)))
        (m (or (car (alist-get 'm options)) 1))
        (prefix (or (car (alist-get 'prefix options)) "")))
    `(let* ((,sel-is-active (my/calc-active-selection-p))
            (m (cond (,sel-is-active
                      (if (my/calc-active-selection-at-line-p)
                          (calc-locate-cursor-element (point))
                        (my/calc-first-active-entry-m)))
                     (t ,m)))
            (entry (nth m calc-stack))
            (,expr (if ,sel-is-active (nth 2 entry) (calc-top-n m)))
            (,replace-expr (lambda (new-expr)
                             (if ,sel-is-active
                                 (let ((new-formula (calc-replace-sub-formula (car entry) ,expr new-expr)))
                                   (calc-pop-push-record-list 1 ,prefix new-formula m new-expr))
                               (calc-pop-push-record-list 1 ,prefix new-expr m)))))
       ,@body)))

(defmacro my/calc-apply-sel-or-top (bindings options &rest body)
  (declare (indent 2))
  (let ((expr (or (nth 0 bindings) (gensym)))
        (replace-expr (or (nth 1 bindings) (gensym)))
        (sel-is-active (or (nth 2 bindings) (gensym)))
        (m (or (car (alist-get 'm options)) 1))
        (prefix (or (car (alist-get 'prefix options)) "")))
    `(let ((,sel-is-active (my/calc-active-selection-p)))
       (cond (,sel-is-active
              (let* ((m (my/calc-active-entry-m-dwim))
                     (entry (nth m calc-stack))
                     (,expr (nth 2 entry))
                     (,replace-expr))
                (fset ',replace-expr
                      (lambda (new-expr)
                        (let ((new-formula (calc-replace-sub-formula (car entry) ,expr new-expr)))
                          (calc-pop-push-record-list 1 ,prefix new-formula m new-expr))))
                ,@body))
             (t
              (let* ((,expr (calc-top-n ,m))
                     (,replace-expr))
                (fset ',replace-expr
                      (lambda (new-expr)
                        (calc-pop-push-record-list 1 ,prefix new-expr ,m)))
                ,@body))))))

(defun my/calc-factor-by ()
  (interactive)
  (my/calc-apply-sel-or-top (expr replace-expr) ((m 2) (prefix "fctr"))
    (my/calc-dont-simplify
     (let* ((factor (calc-top-n 1))
            (divided (math-simplify (calcFunc-nrat (calcFunc-expand (calcFunc-div expr factor)))))
            (product (calcFunc-mul factor divided)))
       (my/preserve-point
        (calc-wrapper
         (replace-expr product)
         (calc-pop-stack 1))
        )))))

(my/calc-apply-sel-or-top (expr replace-expr) ((m 2) (prefix "fctr"))
  (my/calc-dont-simplify
   (let* ((factor (calc-top-n 1))
          (divided (math-simplify (calcFunc-nrat (calcFunc-expand (calcFunc-div expr factor)))))
          (product (calcFunc-mul factor divided)))
     (my/preserve-point
      (calc-wrapper
       (calc-pop-stack 1)
       (replace-expr product))))))
