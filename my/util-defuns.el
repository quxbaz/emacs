;; Utility functions
;;
;; Mostly non-interactive definitions.


(defun my/print-to-buffer (obj &optional buffer-name)
  "Prints OBJ to a buffer. If BUFFER-NAME is nil, print to *scratch* buffer."
  (let* ((buffer-name (or buffer-name "*scratch*"))
         (buffer (get-buffer buffer-name)))
    (princ obj buffer)
    (princ "\n" buffer)))

(defun my/is-line-empty? ()
  "Returns t if the line at point is empty, otherwise nil."
  (eq (point-at-bol) (point-at-eol)))

(defun my/string-at (pos &optional offset)
  "Gets the string at a specified point."
  (let ((offset (or offset 0)))
    (string (char-after (+ pos offset)))))

(defun my/string-at-point (&optional offset)
  "Gets the string at point."
  (let ((offset (or offset 0)))
    (string (char-after (+ (point) offset)))))

(defun my/line-text ()
  "Gets text string at the current line."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun my/region-text ()
  "Gets text within the region. If region is inactive, return nil."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my/root-list-position (&optional max-depth)
  "Gets the position of the root list starting from point."
  (if (eq max-depth nil)
      (setq max-depth 100))
  (let ((current-point (point)))
    (condition-case nil
        (dotimes (n max-depth)
          (setq current-point (scan-lists current-point -1 1)))
      (scan-error nil))
    current-point))

(defun my/goto-root-list (&optional max-depth)
  "Moves point to opening parens of root list."
  (interactive)
  (if (eq max-depth nil)
      (setq max-depth 100))
  (condition-case nil
      (backward-up-list max-depth t t)
    (scan-error nil)
    (user-error nil)))
