;; -*- lexical-binding: t; -*-
;;
;; Tests for my/math-ref-angle and my/calc-ref-angle
;;

(require 'ert)
(require 'calc)
(require 'cl-lib)

;; Load the code under test
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

;;; Quadrant I (0-90)

(ert-deftest test-my/math-ref-angle-quadrant-1-zero ()
  "Test reference angle of 0 is 0."
  (should (equal (my/math-ref-angle 0) 0)))

(ert-deftest test-my/math-ref-angle-quadrant-1-30 ()
  "Test reference angle of 30 is 30."
  (should (equal (my/math-ref-angle 30) 30)))

(ert-deftest test-my/math-ref-angle-quadrant-1-45 ()
  "Test reference angle of 45 is 45."
  (should (equal (my/math-ref-angle 45) 45)))

(ert-deftest test-my/math-ref-angle-quadrant-1-60 ()
  "Test reference angle of 60 is 60."
  (should (equal (my/math-ref-angle 60) 60)))

;;; Quadrantal angle 90

(ert-deftest test-my/math-ref-angle-90 ()
  "Test reference angle of 90 is 90."
  (should (equal (my/math-ref-angle 90) 90)))

;;; Quadrant II (90-180)

(ert-deftest test-my/math-ref-angle-quadrant-2-100 ()
  "Test reference angle of 100 is 80."
  (should (equal (my/math-ref-angle 100) 80)))

(ert-deftest test-my/math-ref-angle-quadrant-2-120 ()
  "Test reference angle of 120 is 60."
  (should (equal (my/math-ref-angle 120) 60)))

(ert-deftest test-my/math-ref-angle-quadrant-2-135 ()
  "Test reference angle of 135 is 45."
  (should (equal (my/math-ref-angle 135) 45)))

(ert-deftest test-my/math-ref-angle-quadrant-2-150 ()
  "Test reference angle of 150 is 30."
  (should (equal (my/math-ref-angle 150) 30)))

;;; Quadrantal angle 180

(ert-deftest test-my/math-ref-angle-180 ()
  "Test reference angle of 180 is 0."
  (should (equal (my/math-ref-angle 180) 0)))

;;; Quadrant III (180-270)

(ert-deftest test-my/math-ref-angle-quadrant-3-210 ()
  "Test reference angle of 210 is 30."
  (should (equal (my/math-ref-angle 210) 30)))

(ert-deftest test-my/math-ref-angle-quadrant-3-225 ()
  "Test reference angle of 225 is 45."
  (should (equal (my/math-ref-angle 225) 45)))

(ert-deftest test-my/math-ref-angle-quadrant-3-240 ()
  "Test reference angle of 240 is 60."
  (should (equal (my/math-ref-angle 240) 60)))

;;; Quadrantal angle 270

(ert-deftest test-my/math-ref-angle-270 ()
  "Test reference angle of 270 is 90."
  (should (equal (my/math-ref-angle 270) 90)))

;;; Quadrant IV (270-360)

(ert-deftest test-my/math-ref-angle-quadrant-4-300 ()
  "Test reference angle of 300 is 60."
  (should (equal (my/math-ref-angle 300) 60)))

(ert-deftest test-my/math-ref-angle-quadrant-4-315 ()
  "Test reference angle of 315 is 45."
  (should (equal (my/math-ref-angle 315) 45)))

(ert-deftest test-my/math-ref-angle-quadrant-4-330 ()
  "Test reference angle of 330 is 30."
  (should (equal (my/math-ref-angle 330) 30)))

;;; Quadrantal angle 360

(ert-deftest test-my/math-ref-angle-360 ()
  "Test reference angle of 360 is 0."
  (should (equal (my/math-ref-angle 360) 0)))

;;; Angles > 360 (coterminal reduction)

(ert-deftest test-my/math-ref-angle-450 ()
  "Test reference angle of 450 (90 + 360) is 90."
  (should (equal (my/math-ref-angle 450) 90)))

(ert-deftest test-my/math-ref-angle-750 ()
  "Test reference angle of 750 (30 + 720) is 30."
  (should (equal (my/math-ref-angle 750) 30)))

(ert-deftest test-my/math-ref-angle-855 ()
  "Test reference angle of 855 (135 + 720) is 45."
  (should (equal (my/math-ref-angle 855) 45)))

;;; Negative angles

(ert-deftest test-my/math-ref-angle-negative-45 ()
  "Test reference angle of -45 is 45."
  (should (equal (my/math-ref-angle -45) 45)))

(ert-deftest test-my/math-ref-angle-negative-90 ()
  "Test reference angle of -90 is 90."
  (should (equal (my/math-ref-angle -90) 90)))

(ert-deftest test-my/math-ref-angle-negative-180 ()
  "Test reference angle of -180 is 0."
  (should (equal (my/math-ref-angle -180) 0)))

(ert-deftest test-my/math-ref-angle-negative-270 ()
  "Test reference angle of -270 is 90."
  (should (equal (my/math-ref-angle -270) 90)))

(ert-deftest test-my/math-ref-angle-negative-360 ()
  "Test reference angle of -360 is 0."
  (should (equal (my/math-ref-angle -360) 0)))

(provide 'my-math-ref-angle-tests)
