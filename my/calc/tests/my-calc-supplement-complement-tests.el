;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-supplement and my/calc-complement
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

(defun my-calc-angle-tests--run (fn input-str angle-mode)
  "Push INPUT-STR, set ANGLE-MODE, run FN, return top of stack."
  (calc-reset 0)
  (let ((calc-angle-mode angle-mode))
    (calc-push (math-read-expr input-str))
    (funcall fn)
    (car (nth 1 calc-stack))))

;;; Supplement — degrees

(ert-deftest test-my/calc-supplement-deg-30 ()
  "supplement(30°) = 150°."
  (with-temp-buffer
    (calc-mode)
    (should (equal (my-calc-angle-tests--run #'my/calc-supplement "30" 'deg) 150))))

(ert-deftest test-my/calc-supplement-deg-90 ()
  "supplement(90°) = 90°."
  (with-temp-buffer
    (calc-mode)
    (should (equal (my-calc-angle-tests--run #'my/calc-supplement "90" 'deg) 90))))

(ert-deftest test-my/calc-supplement-deg-150 ()
  "supplement(150°) = 30°."
  (with-temp-buffer
    (calc-mode)
    (should (equal (my-calc-angle-tests--run #'my/calc-supplement "150" 'deg) 30))))

;;; Supplement — radians

(ert-deftest test-my/calc-supplement-rad-pi-over-6 ()
  "supplement(pi/6) = 5*pi/6."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-angle-tests--run #'my/calc-supplement "pi/6" 'rad))
           (diff (math-normalize (list '- result (math-read-expr "5*pi/6")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-supplement-rad-pi-over-2 ()
  "supplement(pi/2) = pi/2."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-angle-tests--run #'my/calc-supplement "pi/2" 'rad))
           (diff (math-normalize (list '- result (math-read-expr "pi/2")))))
      (should (math-zerop (math-simplify diff))))))

;;; Complement — degrees

(ert-deftest test-my/calc-complement-deg-30 ()
  "complement(30°) = 60°."
  (with-temp-buffer
    (calc-mode)
    (should (equal (my-calc-angle-tests--run #'my/calc-complement "30" 'deg) 60))))

(ert-deftest test-my/calc-complement-deg-45 ()
  "complement(45°) = 45°."
  (with-temp-buffer
    (calc-mode)
    (should (equal (my-calc-angle-tests--run #'my/calc-complement "45" 'deg) 45))))

(ert-deftest test-my/calc-complement-deg-60 ()
  "complement(60°) = 30°."
  (with-temp-buffer
    (calc-mode)
    (should (equal (my-calc-angle-tests--run #'my/calc-complement "60" 'deg) 30))))

;;; Complement — radians

(ert-deftest test-my/calc-complement-rad-pi-over-6 ()
  "complement(pi/6) = pi/3."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-angle-tests--run #'my/calc-complement "pi/6" 'rad))
           (diff (math-normalize (list '- result (math-read-expr "pi/3")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-complement-rad-pi-over-4 ()
  "complement(pi/4) = pi/4."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-angle-tests--run #'my/calc-complement "pi/4" 'rad))
           (diff (math-normalize (list '- result (math-read-expr "pi/4")))))
      (should (math-zerop (math-simplify diff))))))

(provide 'my-calc-supplement-complement-tests)
