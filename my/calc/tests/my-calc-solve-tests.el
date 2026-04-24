;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-auto-solve
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper

(defun my/calc-auto-solve-test--eq-match (actual expected)
  "Return t if both are equations with equal LHS and math-equal RHS."
  (and (eq (car-safe actual) 'calcFunc-eq)
       (eq (car-safe expected) 'calcFunc-eq)
       (equal (nth 1 actual) (nth 1 expected))
       (math-zerop (math-simplify (math-sub (nth 2 actual) (nth 2 expected))))))

(defmacro my/calc-auto-solve-test (input expected)
  "Push INPUT, run my/calc-auto-solve once, compare top-of-stack to EXPECTED."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     (my/calc-auto-solve)
     (let ((actual   (car (nth 1 calc-stack)))
           (expected (math-read-expr ,expected)))
       (should (or (equal actual expected)
                   (my/calc-auto-solve-test--eq-match actual expected))))))

(defmacro my/calc-auto-solve-cycle-test (input &rest expected-sequence)
  "Push INPUT, run my/calc-auto-solve once per element of EXPECTED-SEQUENCE,
asserting top-of-stack matches each expected string in order."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     ,@(mapcar (lambda (expected)
                 `(progn
                    (my/calc-auto-solve)
                    (let ((actual   (car (nth 1 calc-stack)))
                          (expected (math-read-expr ,expected)))
                      (should (or (equal actual expected)
                                  (my/calc-auto-solve-test--eq-match actual expected))))))
               expected-sequence)))


;;; Single variable

(ert-deftest test-my/calc-auto-solve-1var-linear ()
  "x + 3 = 7 -> x = 4."
  (my/calc-auto-solve-test "x + 3 = 7" "x = 4"))

(ert-deftest test-my/calc-auto-solve-1var-linear-symbolic ()
  "x + a = b -> x = b - a."
  (with-temp-buffer
    (calc-mode) (calc-reset 0)
    (calc-push (math-read-expr "x + a = b"))
    (my/calc-auto-solve)
    (should (equal (car (nth 1 calc-stack))
                   (math-read-expr "x = b - a")))))

(ert-deftest test-my/calc-auto-solve-1var-quadratic ()
  "x^2 = 4 is solved for x."
  (with-temp-buffer
    (calc-mode) (calc-reset 0)
    (calc-push (math-read-expr "x^2 = 4"))
    (my/calc-auto-solve)
    (should (eq (car-safe (car (nth 1 calc-stack))) 'calcFunc-eq))))


;;; Multiple variables — first alphabetically

(ert-deftest test-my/calc-auto-solve-2var-solves-first-alpha ()
  "x + y = 5 -> x = -y + 5 (x comes first alphabetically)."
  (my/calc-auto-solve-test "x + y = 5" "x = -y + 5"))

(ert-deftest test-my/calc-auto-solve-2var-alpha-ordering ()
  "b + a = 5 -> a = -b + 5 (a comes before b)."
  (my/calc-auto-solve-test "b + a = 5" "a = -b + 5"))


;;; Cycling through variables

(ert-deftest test-my/calc-auto-solve-2var-cycle ()
  "x + y = 5: first press -> x = -y + 5, second -> y = -x + 5."
  (my/calc-auto-solve-cycle-test "x + y = 5"
    "x = -y + 5"
    "y = -x + 5"))

(defun my/calc-auto-solve-test--lhs-var (expr)
  "Return the name symbol of the variable on the LHS of a solved equation, or nil."
  (when (eq (car-safe expr) 'calcFunc-eq)
    (let ((lhs (nth 1 expr)))
      (when (eq (car-safe lhs) 'var) (nth 1 lhs)))))

(ert-deftest test-my/calc-auto-solve-3var-cycle ()
  "x + y + z = 0 cycles x -> y -> z -> x (checks which var is solved for)."
  (with-temp-buffer
    (calc-mode) (calc-reset 0)
    (calc-push (math-read-expr "x + y + z = 0"))
    (my/calc-auto-solve)
    (should (eq (my/calc-auto-solve-test--lhs-var (car (nth 1 calc-stack))) 'x))
    (my/calc-auto-solve)
    (should (eq (my/calc-auto-solve-test--lhs-var (car (nth 1 calc-stack))) 'y))
    (my/calc-auto-solve)
    (should (eq (my/calc-auto-solve-test--lhs-var (car (nth 1 calc-stack))) 'z))
    (my/calc-auto-solve)
    (should (eq (my/calc-auto-solve-test--lhs-var (car (nth 1 calc-stack))) 'x))))


;;; No-op cases

(ert-deftest test-my/calc-auto-solve-no-variables ()
  "Expression with no variables leaves stack unchanged."
  (with-temp-buffer
    (calc-mode) (calc-reset 0)
    (let ((expr (math-read-expr "3 = 3")))
      (calc-push expr)
      (my/calc-auto-solve)
      (should (equal (car (nth 1 calc-stack)) expr)))))

(provide 'my-calc-auto-solve-tests)
