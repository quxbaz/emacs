;; -*- lexical-binding: t; -*-
;;
;; Selection functions


(defun my/calc-clear-selections ()
  "Like calc-clear-selections, but retains point position."
  (interactive)
  (my/preserve-point
   (unwind-protect (calc-clear-selections))))

(defun my/calc-commute ()
  "Like calc-sel-commute, but works on the line instead of the current selection."
  (interactive)
  (my/preserve-point
   (unwind-protect
       (progn
         (move-end-of-line nil)
         (call-interactively 'calc-sel-commute)))))

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
  "Like calc-sel-distribute, but unselects afterwards."
  (interactive)
  (if (my/calc-active-selection-p)
      (call-interactively 'calc-sel-distribute)
    (my/preserve-point
     (unwind-protect
         (call-interactively 'calc-sel-distribute)
       (call-interactively 'calc-unselect)))))

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
