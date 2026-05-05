;; -*- lexical-binding: t; -*-
;;
;; Calc debug utilities


(defconst my/calc-debug-buffer "*calc-debug*")

(defvar-local my/calc-debug--source-buffer nil)

(defun my/calc-debug--quit ()
  (interactive)
  (when (buffer-live-p my/calc-debug--source-buffer)
    (with-current-buffer my/calc-debug--source-buffer
      (my/calc-debug-mode -1))))

(defun my/calc-debug--print-stack ()
  (interactive)
  (when (buffer-live-p my/calc-debug--source-buffer)
    (let ((stack (buffer-local-value 'calc-stack my/calc-debug--source-buffer)))
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (let* ((entries (cl-remove-if (lambda (e) (eq (car e) 'top-of-stack)) stack))
                 (entries (reverse entries)))
            (insert ";; Bottom of stack ▼\n"
                    (mapconcat #'prin1-to-string entries "\n")
                    "\n;; Top of stack ▲\n")))
        (when-let ((win (get-buffer-window (current-buffer))))
          (with-selected-window win
            (goto-char (point-max))
            (forward-line -1)
            (recenter-top-bottom -1)))))))

(defun my/calc-debug--output (expr)
  (let ((s (prin1-to-string expr)))
    (with-current-buffer (get-buffer-create my/calc-debug-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert s "\n"))
      (when-let ((win (get-buffer-window my/calc-debug-buffer)))
        (with-selected-window win
          (goto-char (point-max))
          (forward-line -1)
          (recenter-top-bottom -1))))))

(defvar-local my/calc-debug--hl-overlay nil)

;; Like math-comp-sel-flat-term but also records the flat-string start/end
;; positions of the matched tag node, avoiding a second render pass.
(defvar math-comp-pos)
(defvar math-comp-sel-start nil)
(defvar math-comp-sel-end nil)

(defun my/calc-debug--sel-flat-term (c)
  (cond
   ((not (consp c))
    (setq math-comp-pos (+ math-comp-pos (length c))))
   ((memq (car c) '(set break)))
   ((eq (car c) 'horiz)
    (while (and (setq c (cdr c)) (< math-comp-sel-cpos 1000000))
      (my/calc-debug--sel-flat-term (car c))))
   ((eq (car c) 'tag)
    (if (<= math-comp-pos math-comp-sel-cpos)
        (let ((start math-comp-pos))
          (my/calc-debug--sel-flat-term (nth 2 c))
          (when (> math-comp-pos math-comp-sel-cpos)
            (setq math-comp-sel-tag c
                  math-comp-sel-cpos 1000000
                  math-comp-sel-start start
                  math-comp-sel-end math-comp-pos)))
      (my/calc-debug--sel-flat-term (nth 2 c))))
   (t
    (my/calc-debug--sel-flat-term (nth 2 c)))))

(defun my/calc-debug--find-selected-bounds ()
  "Return (buf-start . buf-end) of the subexpression at point, or nil.
Mirrors calc-find-selected-part but records flat-string positions to
avoid a second render pass."
  (let* ((math-comp-sel-hpos (- (current-column) calc-selection-cache-offset))
         toppt
         (lcount 0)
         (spaces 0)
         (math-comp-sel-vpos
          (save-excursion
            (beginning-of-line)
            (let ((line (point)))
              (calc-cursor-stack-index calc-selection-cache-num)
              (setq toppt (point))
              (while (< (point) line)
                (forward-line 1)
                (setq spaces (+ spaces (current-indentation))
                      lcount (1+ lcount)))
              (- lcount (math-comp-ascent calc-selection-cache-comp) -1))))
         (math-comp-sel-cpos (- (point) toppt calc-selection-cache-offset
                                spaces lcount))
         (math-comp-sel-tag nil)
         (math-comp-sel-start nil)
         (math-comp-sel-end nil))
    (when (and (>= math-comp-sel-hpos 0)
               (> calc-selection-true-num 0))
      (let ((math-comp-pos 0))
        (my/calc-debug--sel-flat-term calc-selection-cache-comp)))
    (when math-comp-sel-start
      (let ((base (+ toppt calc-selection-cache-offset)))
        (cons (+ base math-comp-sel-start)
              (+ base math-comp-sel-end))))))

(defun my/calc-debug--highlight-update ()
  "Highlight the subexpression at point using an overlay."
  (when (and (derived-mode-p 'calc-mode) (not (minibufferp)))
    (require 'calc-sel)
    (require 'calc-yank)
    (when (overlayp my/calc-debug--hl-overlay)
      (delete-overlay my/calc-debug--hl-overlay)
      (setq my/calc-debug--hl-overlay nil))
    (let ((idx (calc-locate-cursor-element (point))))
      (when (> idx 0)
        (calc-prepare-selection idx)
        (when-let ((bounds (my/calc-debug--find-selected-bounds)))
          (setq my/calc-debug--hl-overlay
                (make-overlay (car bounds) (cdr bounds)))
          (overlay-put my/calc-debug--hl-overlay 'face 'highlight))))))

(define-minor-mode my/calc-debug-highlight-mode
  "Highlight subexpression at point as cursor moves."
  :lighter " HL"
  (if my/calc-debug-highlight-mode
      (add-hook 'post-command-hook #'my/calc-debug--highlight-update nil t)
    (remove-hook 'post-command-hook #'my/calc-debug--highlight-update t)
    (when (overlayp my/calc-debug--hl-overlay)
      (delete-overlay my/calc-debug--hl-overlay)
      (setq my/calc-debug--hl-overlay nil))))

(defun my/calc-debug-print-line ()
  "Print structural representation of calc expression at point."
  (interactive)
  (require 'calc-yank)
  (let* ((idx (calc-locate-cursor-element (point)))
         (expr (and (> idx 0) (calc-top idx))))
    (if expr
        (my/calc-debug--output expr)
      (message "No calc expression at point"))))

(defun my/calc-debug--post-command ()
  (require 'calc-yank)
  (let* ((idx (calc-locate-cursor-element (point)))
         (expr (and (> idx 0) (calc-top idx))))
    (when expr
      (my/calc-debug--output expr))))

(define-minor-mode my/calc-debug-mode
  "Continuously show structural representation of expression at point."
  :lighter " CDbg"
  (if my/calc-debug-mode
      (progn
        (my/calc-debug-highlight-mode 1)
        (add-hook 'post-command-hook #'my/calc-debug--post-command nil t)
        (let ((calc-buf (current-buffer)))
          (with-current-buffer (get-buffer-create my/calc-debug-buffer)
            (let ((inhibit-read-only t)) (erase-buffer))
            (emacs-lisp-mode)
            (rainbow-blocks-mode 1)
            (read-only-mode 1)
            (setq my/calc-debug--source-buffer calc-buf)
            (local-set-key (kbd "q") #'my/calc-debug--quit)
            (local-set-key (kbd "S") #'my/calc-debug--print-stack)))
        (display-buffer my/calc-debug-buffer
                        '(display-buffer-below-selected . ((window-height . 20)))))
    (my/calc-debug-highlight-mode -1)
    (remove-hook 'post-command-hook #'my/calc-debug--post-command t)
    (when-let ((win (get-buffer-window my/calc-debug-buffer)))
      (delete-window win))))

(add-hook 'calc-mode-hook #'my/calc-debug-highlight-mode)

(provide 'my/calc/debug)
