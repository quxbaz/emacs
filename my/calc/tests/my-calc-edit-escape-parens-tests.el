;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-edit-escape-parens

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/util.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/edit.el" user-emacs-directory))


;;; Helper

(defmacro with-escape-buffer (text point-re &rest body)
  "Run BODY in a temp buffer containing TEXT with point after POINT-RE match."
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     (re-search-forward ,point-re)
     ,@body))

(defun escape-result ()
  "Return \"BEFORE|AFTER\" string showing current point position."
  (concat (buffer-substring (point-min) (point))
          "|"
          (buffer-substring (point) (point-max))))


;;; Single invocation

(ert-deftest test-my-calc-edit-escape-parens-simple ()
  "(x+1|) → (x+1)|"
  (with-escape-buffer "(x+1)" "x.1"
    (my/calc-edit-escape-parens)
    (should (string= (escape-result) "(x+1)|"))))

(ert-deftest test-my-calc-edit-escape-parens-denominator ()
  "y/(x+1|) → y/(x+1)|"
  (with-escape-buffer "y/(x+1)" "x.1"
    (my/calc-edit-escape-parens)
    (should (string= (escape-result) "y/(x+1)|"))))

(ert-deftest test-my-calc-edit-escape-parens-from-open ()
  "sin(|x) → sin(x)| — point at open paren escapes the whole group."
  (with-escape-buffer "sin(x)" "sin("
    (my/calc-edit-escape-parens)
    (should (string= (escape-result) "sin(x)|"))))


;;; Repeated invocations (nested groups)

(ert-deftest test-my-calc-edit-escape-parens-nested-first ()
  "y/((x+1|)) first call → y/((x+1)|)"
  (with-escape-buffer "y/((x+1))" "x.1"
    (my/calc-edit-escape-parens)
    (should (string= (escape-result) "y/((x+1)|)"))))

(ert-deftest test-my-calc-edit-escape-parens-nested-second ()
  "y/((x+1|)) second call → y/((x+1))|"
  (with-escape-buffer "y/((x+1))" "x.1"
    (my/calc-edit-escape-parens)
    (my/calc-edit-escape-parens)
    (should (string= (escape-result) "y/((x+1))|"))))

(ert-deftest test-my-calc-edit-escape-parens-triple-first ()
  "sin((a|+1)^2) first call → sin((a+1)|^2)"
  (with-escape-buffer "sin((a+1)^2)" "a"
    (my/calc-edit-escape-parens)
    (should (string= (escape-result) "sin((a+1)|^2)"))))

(ert-deftest test-my-calc-edit-escape-parens-triple-second ()
  "sin((a|+1)^2) second call → sin((a+1)^2)|"
  (with-escape-buffer "sin((a+1)^2)" "a"
    (my/calc-edit-escape-parens)
    (my/calc-edit-escape-parens)
    (should (string= (escape-result) "sin((a+1)^2)|"))))


;;; No-op cases

(ert-deftest test-my-calc-edit-escape-parens-toplevel-noop ()
  "At top level (no enclosing parens), point does not move."
  (with-escape-buffer "x+1" "x"
    (let ((pos (point)))
      (my/calc-edit-escape-parens)
      (should (= (point) pos)))))

(ert-deftest test-my-calc-edit-escape-parens-unbalanced-noop ()
  "Unbalanced parens (no closing paren) do not signal an error and point does not move."
  (with-escape-buffer "(x+1" "x"
    (let ((pos (point)))
      (should-not (condition-case err
                      (progn (my/calc-edit-escape-parens) nil)
                    (error err)))
      (should (= (point) pos)))))
