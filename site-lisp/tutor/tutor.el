;;; tutor.el --- Step through code with explanatory popups  -*- lexical-binding: t; -*-

;; Author: David
;; Keywords: tools, convenience, docs
;; Package-Requires: ((emacs "27.1") (posframe "1.1.0"))
;; Version: 0.1.0

;;; Commentary:

;; Tutor plays a guided tour of source code: a sequence of steps,
;; each pairing a region of code with a prose explanation.  The source
;; file fills the frame with the current step's region highlighted, and
;; the explanation floats in a posframe just below it.  You navigate
;; from the source buffer with n/p (or j/k), jump with g, and quit with
;; q.
;;
;; It is a presentation engine: it plays a static "script" and knows
;; nothing about how that script was produced -- you can hand-author one
;; or have a tool (e.g. an LLM) generate it.
;;
;; A script is a readable plist stored in a file:
;;
;;   (:title "How the reader works"
;;    :file "lisp/lib.el"               ; default file for every step
;;    :steps
;;    ((:lines (12 . 18) :text "This binds the reader and ...")
;;     (:lines (20 . 24) :text "Here we recurse over the ...")
;;     ;; a step may override the file to span multiple files:
;;     (:file "util.el" :lines (3 . 9) :text "...which calls this helper.")))
;;
;; `:file' paths are resolved relative to the script file's directory
;; (absolute paths are used as-is).  `:lines' is an inclusive (START . END)
;; line range (1-based).
;;
;; Usage:
;;
;;   M-x tutor-load RET path/to/script.el RET
;;
;; then n/p/j/k to move, g to jump, q to quit.  A graphical frame is
;; required (posframe does not work in a terminal).
;;
;; See README.md for more.

;;; Code:

(require 'cl-lib)
(require 'posframe nil t)


;;;; Customization

(defgroup tutor nil
  "Step through annotated regions of code with explanatory popups."
  :group 'tools
  :prefix "tutor-")

(defface tutor-highlight
  '((t :inherit highlight :extend t))
  "Face for the highlighted region of the current tutor step."
  :group 'tutor)

(defcustom tutor-posframe-width 80
  "Maximum character width of the explanation posframe."
  :type 'integer
  :group 'tutor)

(defcustom tutor-border-color "white"
  "Border color of the explanation posframe."
  :type 'color
  :group 'tutor)

(defcustom tutor-fill-color "blue"
  "Background (fill) color of the explanation posframe."
  :type 'color
  :group 'tutor)

(defcustom tutor-text-color "white"
  "Foreground (text) color of the explanation posframe."
  :type 'color
  :group 'tutor)


;;;; State

(defvar tutor--posframe-buffer " *tutor*"
  "Name of the hidden buffer backing the explanation posframe.")

(defvar tutor--steps nil
  "Vector of step plists for the active tutor, or nil if none.")
(defvar tutor--index 0
  "Index of the current step within `tutor--steps'.")
(defvar tutor--title nil
  "Title of the active tutor.")
(defvar tutor--default-file nil
  "Default source file for steps that don't specify their own.")
(defvar tutor--base-dir nil
  "Directory the script's `:file' paths are resolved against.")
(defvar tutor--source-buffer nil
  "The source buffer currently shown in the tutorial.")
(defvar tutor--overlay nil
  "Overlay highlighting the current step's region.")
(defvar tutor--window-config nil
  "Window configuration to restore when the tutorial quits.")

(defvar-local tutor--saved-read-only nil
  "Whether the source buffer was read-only before the tutorial.")


;;;; Minor mode
;;
;; Navigation lives in the source buffer; the explanation is display-only.

(defvar tutor-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "n" #'tutor-next)
    (keymap-set map "p" #'tutor-prev)
    (keymap-set map "j" #'tutor-next)
    (keymap-set map "k" #'tutor-prev)
    (keymap-set map "g" #'tutor-goto)
    (keymap-set map "q" #'tutor-quit)
    map)
  "Keymap active in the source buffer during a tutorial.")

(define-minor-mode tutor-source-mode
  "Minor mode for the source buffer during a code tutor.
Makes the buffer read-only (restoring its prior state on exit) so the
highlighted regions stay put."
  :lighter " Tutor"
  :keymap tutor-mode-map
  (if tutor-source-mode
      (progn
        (setq tutor--saved-read-only buffer-read-only)
        (setq buffer-read-only t))
    (setq buffer-read-only tutor--saved-read-only)))


;;;; Display

(defun tutor--step-file (step)
  "Return the absolute source file for STEP, or nil to reuse the current buffer."
  (let ((file (or (plist-get step :file) tutor--default-file)))
    (and file (expand-file-name file tutor--base-dir))))

(defun tutor--highlight (lines)
  "Highlight the inclusive LINES range (START . END) in the current buffer."
  (when (overlayp tutor--overlay)
    (delete-overlay tutor--overlay))
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (car lines)))
    (let ((beg (line-beginning-position)))
      (goto-char (point-min))
      (forward-line (1- (cdr lines)))
      (let ((end (min (point-max) (1+ (line-end-position)))))
        (setq tutor--overlay (make-overlay beg end))
        (overlay-put tutor--overlay 'face 'tutor-highlight))))
  ;; Center the highlighted region on its midpoint, nudged a few lines above
  ;; center for visual balance (and to leave more room for the popup below).
  (let ((win (get-buffer-window (current-buffer))))
    (when win
      (set-window-point win (overlay-start tutor--overlay))
      (with-selected-window win
        (save-excursion
          (goto-char (/ (+ (overlay-start tutor--overlay)
                           (overlay-end tutor--overlay))
                        2))
          (recenter (max 0 (- (/ (window-body-height) 2) 4))))))))

(defun tutor--header ()
  "Progress summary shown atop the posframe."
  (format "Step %d/%d%s"
          (1+ tutor--index)
          (length tutor--steps)
          (if tutor--title
              (concat "  —  " tutor--title)
            "")))

(defun tutor--render (step)
  "Show STEP's explanation in the corner posframe."
  (unless (and (display-graphic-p) (featurep 'posframe))
    (user-error "Tutor needs posframe in a graphical frame"))
  (let ((buf (get-buffer-create tutor--posframe-buffer)))
    (with-current-buffer buf
      ;; Horizontal padding comes from the fringes (recolored to the fill
      ;; color below so they read as padding, not strips); vertical padding
      ;; from the blank lines around the text.  posframe has only one border,
      ;; so it can't double as padding.
      (setq-local truncate-lines nil
                  word-wrap t
                  face-remapping-alist
                  `((fringe (:background ,tutor-fill-color
                             :foreground ,tutor-fill-color))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n")
        (insert (propertize (tutor--header)
                            'face 'bold))
        (insert "\n\n")
        (insert (or (plist-get step :text) ""))
        ;; Trailing line holds a space so posframe doesn't trim it away,
        ;; giving real bottom padding.
        (insert "\n ")))
    (posframe-show
     buf
     ;; Anchor below the *end* of the highlighted region so the popup sits
     ;; under the code rather than over it.
     :position (overlay-end tutor--overlay)
     :poshandler #'posframe-poshandler-point-bottom-left-corner
     :width (min tutor-posframe-width (/ (frame-width) 2))
     :left-fringe 16
     :right-fringe 16
     :internal-border-width 2
     :internal-border-color tutor-border-color
     :foreground-color tutor-text-color
     :background-color tutor-fill-color)))

(defun tutor--show-step (i)
  "Display step I (0-based)."
  (setq tutor--index i)
  (let* ((step (aref tutor--steps i))
         (file (tutor--step-file step))
         (src (if file
                  (if (file-exists-p file)
                      (find-file-noselect file)
                    (user-error "Tutor step %d: no such file: %s" (1+ i) file))
                tutor--source-buffer)))
    (cond
     ;; First step: take over the frame with the source buffer.
     ((null tutor--source-buffer)
      (setq tutor--window-config (current-window-configuration))
      (delete-other-windows)
      (switch-to-buffer src)
      (setq tutor--source-buffer src))
     ;; Later step in a different file: swap the buffer in place.
     ((not (eq src tutor--source-buffer))
      (when (buffer-live-p tutor--source-buffer)
        (with-current-buffer tutor--source-buffer
          (tutor-source-mode -1)))
      (switch-to-buffer src)
      (setq tutor--source-buffer src)))
    (with-current-buffer src
      (unless tutor-source-mode (tutor-source-mode 1))
      (tutor--highlight (plist-get step :lines)))
    (tutor--render step)))


;;;; Commands

(defun tutor-next ()
  "Advance to the next tutor step."
  (interactive)
  (if (< (1+ tutor--index) (length tutor--steps))
      (tutor--show-step (1+ tutor--index))
    (message "Last step")))

(defun tutor-prev ()
  "Rewind to the previous tutor step."
  (interactive)
  (if (> tutor--index 0)
      (tutor--show-step (1- tutor--index))
    (message "First step")))

(defun tutor-goto (n)
  "Jump to step N (1-based)."
  (interactive (list (read-number "Go to step: " (1+ tutor--index))))
  (let ((i (1- n)))
    (if (and (>= i 0) (< i (length tutor--steps)))
        (tutor--show-step i)
      (user-error "No step %d (have %d)" n (length tutor--steps)))))

(defun tutor-quit ()
  "End the tutorial, restoring buffers and the previous window layout."
  (interactive)
  (when (overlayp tutor--overlay)
    (delete-overlay tutor--overlay))
  (when (buffer-live-p tutor--source-buffer)
    (with-current-buffer tutor--source-buffer
      (tutor-source-mode -1)))
  (when (featurep 'posframe)
    (posframe-delete tutor--posframe-buffer))
  (when tutor--window-config
    (set-window-configuration tutor--window-config))
  (setq tutor--steps nil
        tutor--overlay nil
        tutor--source-buffer nil
        tutor--window-config nil))

;;;###autoload
(defun tutor-start (data &optional base-dir)
  "Begin the tutorial described by DATA, a plist (see Commentary).
BASE-DIR resolves relative `:file' paths (defaults to `default-directory')."
  (let ((steps (plist-get data :steps)))
    (unless steps (user-error "Tutor has no :steps"))
    (setq tutor--title (plist-get data :title)
          tutor--default-file (plist-get data :file)
          tutor--base-dir (or base-dir default-directory)
          tutor--steps (vconcat steps)
          tutor--index 0
          tutor--source-buffer nil
          tutor--window-config nil)
    (tutor--show-step 0)))

;;;###autoload
(defun tutor-load (file)
  "Load and play the tutorial script in FILE."
  (interactive "fTutor script: ")
  (let ((data (with-temp-buffer
                (insert-file-contents file)
                (read (current-buffer)))))
    (tutor-start data (file-name-directory (expand-file-name file)))))

(provide 'tutor)
;;; tutor.el ends here
