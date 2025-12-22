;; -*- lexical-binding: t; -*-
;;
;; Selection functions


(defun my/calc-empty-selection-p ()
  "Returns t if there are no active selections."
  (or (null calc-selection-cache-entry)
      ;; (not (equal (nthcdr 2 calc-selection-cache-entry) '(nil)))
      (null (nth 2 calc-selection-cache-entry))))

(defun my/calc-active-selection-p ()
  "Returns t if there are any active selections."
  (not (my/calc-empty-selection-p)))

(defun my/calc-clear-selections ()
  "Like calc-clear-selections, but retains point position."
  (interactive)
  (let ((saved-point (point)))
    (unwind-protect
        (calc-clear-selections)
      (setf (point) saved-point))))

(defun my/calc-commute ()
  "Like calc-sel-commute, but works on the line instead of the current selection."
  (interactive)
  (let ((saved-point (point)))
    (unwind-protect
        (progn
          (move-end-of-line nil)
          (call-interactively 'calc-sel-commute))
      (setf (point) saved-point))))

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
  (let ((saved-point (point)))
    (unwind-protect
        (call-interactively 'calc-sel-negate)
      (call-interactively 'calc-unselect)
      (setf (point) saved-point))))

(provide 'my/calc/selection)
