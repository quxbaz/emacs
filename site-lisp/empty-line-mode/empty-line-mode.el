;;; empty-line-mode.el --- Mark blank lines with a fringe indicator  -*- lexical-binding: t; -*-

;; Author: David
;; Keywords: convenience, faces
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.0

;;; Commentary:

;; Emacs's built-in `indicate-empty-lines' only marks the empty screen
;; lines *past the end of the buffer*.  This minor mode instead marks every
;; blank line *within* the text with a bitmap in the fringe -- the way
;; diff-hl and git-gutter mark changed lines.
;;
;; `empty-line-mode' is buffer-local.  `global-empty-line-mode' turns it on
;; in every ordinary editing buffer, skipping the minibuffer and
;; `special-mode' buffers (dired, magit, *Help*, ...).
;;
;; Overlays carrying the fringe bitmap are (re)computed by jit-lock as text
;; is fontified, so they track edits.  This is fringe-only: it has no effect
;; on a terminal without fringes.
;;
;; Usage:
;;
;;   (require 'empty-line-mode)
;;   (global-empty-line-mode 1)
;;
;; Customize `empty-line-bitmap' (any `fringe-bitmap-p' symbol),
;; `empty-line-side', and the `empty-line-fringe' face to taste.

;;; Code:

(require 'jit-lock)

(defgroup empty-line nil
  "Mark blank lines with a fringe indicator."
  :group 'convenience
  :prefix "empty-line-")

(defcustom empty-line-bitmap 'horizontal-bar
  "Fringe bitmap shown on each blank line.
Any symbol satisfying `fringe-bitmap-p'.  See the palette just below to
switch, or Info node `(elisp) Fringe Bitmaps' for the full set."
  :type '(choice (const  :tag "Thin dash"              horizontal-bar)
                 (const  :tag "Low hollow box"         empty-line)
                 (const  :tag "Vertical line"          vertical-bar)
                 (const  :tag "Outlined box"           hollow-rectangle)
                 (const  :tag "Solid block"            filled-rectangle)
                 (const  :tag "Small outlined square"  hollow-square)
                 (const  :tag "Solid square"           filled-square)
                 (const  :tag "Outlined circle"        large-circle)
                 (const  :tag "Left triangle"          left-triangle)
                 (const  :tag "Right triangle"         right-triangle)
                 (symbol :tag "Other `fringe-bitmap-p' symbol"))
  :group 'empty-line)

;; Bitmap palette -- the default is `horizontal-bar' (above); uncomment one
;; line to switch the indicator. All commented out = use the default.
;; (setq empty-line-bitmap 'horizontal-bar)    ;; thin dash
;; (setq empty-line-bitmap 'empty-line)        ;; low hollow box (the indicate-empty-lines look)
;; (setq empty-line-bitmap 'vertical-bar)      ;; vertical line
;; (setq empty-line-bitmap 'hollow-rectangle)  ;; outlined box
;; (setq empty-line-bitmap 'filled-rectangle)  ;; solid block
;; (setq empty-line-bitmap 'hollow-square)     ;; small outlined square
;; (setq empty-line-bitmap 'filled-square)     ;; solid square
;; (setq empty-line-bitmap 'large-circle)      ;; outlined circle
;; (setq empty-line-bitmap 'left-triangle)     ;; triangle pointing left
;; (setq empty-line-bitmap 'right-triangle)    ;; triangle pointing right

(defcustom empty-line-side 'left-fringe
  "Which fringe displays the blank-line indicator."
  :type '(choice (const left-fringe) (const right-fringe))
  :group 'empty-line)

(defface empty-line-fringe '((t :inherit shadow))
  "Face giving the color of the blank-line fringe indicator."
  :group 'empty-line)

(defun empty-line--exempt-p ()
  "Return non-nil if the current buffer should not be marked.
True for the minibuffer and `special-mode'-class buffers."
  (or (minibufferp)
      (eq (get major-mode 'mode-class) 'special)))

(defun empty-line--refresh (start end)
  "Mark each blank line in START..END with a fringe indicator."
  (remove-overlays start end 'empty-line t)
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^\n" end t)
      (let ((ov (make-overlay (match-beginning 0) (match-beginning 0))))
        (overlay-put ov 'empty-line t)
        (overlay-put ov 'before-string
                     (propertize " " 'display
                                 (list empty-line-side
                                       empty-line-bitmap
                                       'empty-line-fringe)))))))

;;;###autoload
(define-minor-mode empty-line-mode
  "Mark blank lines in the current buffer with a fringe indicator."
  :lighter nil
  (if empty-line-mode
      (progn
        (jit-lock-register #'empty-line--refresh)
        (empty-line--refresh (point-min) (point-max)))
    (jit-lock-unregister #'empty-line--refresh)
    (remove-overlays (point-min) (point-max) 'empty-line t)))

(defun empty-line--turn-on ()
  "Enable `empty-line-mode' unless the buffer is exempt."
  (unless (empty-line--exempt-p)
    (empty-line-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-empty-line-mode
  empty-line-mode empty-line--turn-on)

(provide 'empty-line-mode)
;;; empty-line-mode.el ends here
