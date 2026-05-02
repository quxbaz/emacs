;; -*- lexical-binding: t; -*-
;;
;; Selection functions

(require 'my/calc/lib)

(defun my/calc-clear-selections ()
  "Like calc-clear-selections, but retains point position."
  (interactive)
  (my/preserve-point
   (unwind-protect (calc-clear-selections))))

(defun my/calc-commute ()
  "Like calc-sel-commute, but works on the line instead of the current selection."
  (interactive)
  (my/calc-without-simplification
    (my/preserve-point
     (unwind-protect
         (progn
           (move-end-of-line nil)
           (call-interactively 'calc-sel-commute))))))

;; Extend JumpRules with != (neq) variants of every = rule so that
;; calc-sel-jump-equals also works on inequalities.
(defconst my/calc--jump-neq-extra-rules
  "[ plain(select(x) != y)         :=  0 != select(-x) + y,
     plain(a + select(x) != y)     :=  a != select(-x) + y,
     plain(a - select(x) != y)     :=  a != select(x) + y,
     plain(select(x) + a != y)     :=  a != select(-x) + y,
     plain(a * select(x) != y)     :=  a != y / select(x),
     plain(a / select(x) != y)     :=  a != select(x) * y,
     plain(select(x) / a != y)     :=  1/a != y / select(x),
     plain(a ^ select(2) != y)     :=  a != select(sqrt(y)),
     plain(a ^ select(x) != y)     :=  a != y ^ select(1/x),
     plain(select(x) ^ a != y)     :=  a != log(y, select(x)),
     plain(log(a, select(x)) != y) :=  a != select(x) ^ y,
     plain(log(select(x), a) != y) :=  a != select(x) ^ (1/y),
     plain(y != select(x))         :=  y - select(x) != 0,
     plain(y != a + select(x))     :=  y - select(x) != a,
     plain(y != a - select(x))     :=  y + select(x) != a,
     plain(y != select(x) + a)     :=  y - select(x) != a,
     plain(y != a * select(x))     :=  y / select(x) != a,
     plain(y != a / select(x))     :=  y * select(x) != a,
     plain(y != select(x) / a)     :=  y / select(x) != 1/a,
     plain(y != a ^ select(2))     :=  select(sqrt(y)) != a,
     plain(y != a ^ select(x))     :=  y ^ select(1/x) != a,
     plain(y != select(x) ^ a)     :=  log(y, select(x)) != a,
     plain(y != log(a, select(x))) :=  select(x) ^ y != a,
     plain(y != log(select(x), a)) :=  select(x) ^ (1/y) != a ]")

(advice-add 'calc-JumpRules :filter-return
  (lambda (rules)
    (let ((neq (math-read-plain-expr my/calc--jump-neq-extra-rules t)))
      (if (and (eq (car-safe rules) 'vec) (eq (car-safe neq) 'vec))
          (append rules (cdr neq))
        rules))))

(defun my/calc-sel-jump-equals ()
  "Like calc-sel-jump-equals, but unselects after jumping."
  (interactive)
  (let ((saved/calc-show-selections calc-show-selections))
    (unwind-protect
        (progn (calc-show-selections -1)
               (call-interactively 'calc-sel-jump-equals)
               (beginning-of-line)
               (search-forward-regexp "#")
               (backward-char))
      (call-interactively 'calc-unselect)
      (if (eq saved/calc-show-selections t)
          (calc-show-selections t)))))

(defun my/calc-sel-negate ()
  "Like calc-sel-negate, but unselects afterwards."
  (interactive)
  (if (my/calc-active-selection-p)
      (call-interactively 'calc-sel-negate)
    (my/preserve-point
     (unwind-protect
         (call-interactively 'calc-sel-negate)
       (call-interactively 'calc-unselect)))))

(defun my/calc-sel-distribute ()
  "Like calc-sel-distribute, but unselects afterwards and runs without simplification."
  (interactive)
  (my/calc-without-simplification
    (if (my/calc-active-selection-p)
        (call-interactively 'calc-sel-distribute)
      (my/preserve-point
       (unwind-protect
           (call-interactively 'calc-sel-distribute)
         (call-interactively 'calc-unselect))))))

(defun my/calc-sel-merge ()
  "Like calc-sel-merge, but unselects afterwards."
  (interactive)
  (if (my/calc-active-selection-p)
      (call-interactively 'calc-sel-merge)
    (my/preserve-point
     (unwind-protect
         (call-interactively 'calc-sel-merge)
       (call-interactively 'calc-unselect)))))

(provide 'my/calc/selection)
