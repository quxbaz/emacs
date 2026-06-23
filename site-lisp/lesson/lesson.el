;;; lesson.el --- Step through code with explanatory popups  -*- lexical-binding: t; -*-

;; Author: David
;; Keywords: tools, convenience, docs
;; Package-Requires: ((emacs "27.1") (posframe "1.1.0"))
;; Version: 0.1.0

;;; Commentary:

;; Lesson plays a guided tour of source code: a sequence of steps, each
;; pairing a region of code with a prose explanation.  The source file
;; fills the frame with the current step's region highlighted, and the
;; explanation floats in a posframe just below it.  You navigate from the
;; source buffer with n/p (or j/k), jump with g, and quit with q.
;;
;; It is a presentation engine: it plays a static "lesson plan" and knows
;; nothing about how that plan was produced -- you can hand-author one or
;; have a tool (e.g. an LLM) generate it.
;;
;; A lesson plan is a readable plist stored in a file:
;;
;;   (:title "How the reader works"
;;    :file "lisp/lib.el"               ; default file for every step
;;    :steps
;;    ((:lines (12 . 18) :text "This binds the reader and ...")
;;     (:lines (20 . 24) :text "Here we recurse over the ...")
;;     ;; a step may override the file to span multiple files:
;;     (:file "util.el" :lines (3 . 9) :text "...which calls this helper.")))
;;
;; `:file' paths are resolved relative to the lesson-plan file's directory
;; (absolute paths are used as-is).  `:lines' is an inclusive (START . END)
;; line range (1-based).
;;
;; Usage:
;;
;;   M-x lesson-load RET path/to/plan.el RET
;;
;; then n/p/j/k to move, g to jump, q to quit.  A graphical frame is
;; required (posframe does not work in a terminal).
;;
;; See README.md for more.

;;; Code:

(require 'cl-lib)
(require 'posframe nil t)


;;;; Customization

(defgroup lesson nil
  "Step through annotated regions of code with explanatory popups."
  :group 'tools
  :prefix "lesson-")

(defface lesson-highlight
  '((t :inherit highlight :extend t))
  "Face for the highlighted region of the current step."
  :group 'lesson)

(defcustom lesson-posframe-width 80
  "Maximum character width of the explanation posframe."
  :type 'integer
  :group 'lesson)

(defcustom lesson-border-color "white"
  "Border color of the explanation posframe."
  :type 'color
  :group 'lesson)

(defcustom lesson-fill-color "#1d3557"
  "Background (fill) color of the explanation posframe.
A desaturated navy: strong contrast with white text but easier to read
than a fully saturated blue."
  :type 'color
  :group 'lesson)

(defcustom lesson-text-color "white"
  "Foreground (text) color of the explanation posframe."
  :type 'color
  :group 'lesson)

(defcustom lesson-plans-directory (locate-user-emacs-file "lesson-plans")
  "Directory `lesson-load' starts in when prompting for a lesson plan."
  :type 'directory
  :group 'lesson)


;;;; State

(defvar lesson--posframe-buffer " *lesson*"
  "Name of the hidden buffer backing the explanation posframe.")

(defvar lesson--steps nil
  "Vector of step plists for the active lesson plan, or nil if none.")
(defvar lesson--index 0
  "Index of the current step within `lesson--steps'.")
(defvar lesson--title nil
  "Title of the active lesson plan.")
(defvar lesson--default-file nil
  "Default source file for steps that don't specify their own.")
(defvar lesson--base-dir nil
  "Directory the lesson plan's `:file' paths are resolved against.")
(defvar lesson--source-buffer nil
  "The source buffer currently shown.")
(defvar lesson--overlay nil
  "Overlay highlighting the current step's region.")
(defvar lesson--window-config nil
  "Window configuration to restore when the lesson plan finishes.")

(defvar-local lesson--saved-read-only nil
  "Whether the source buffer was read-only before the lesson plan.")


;;;; Minor mode
;;
;; Navigation lives in the source buffer; the explanation is display-only.

(defvar lesson-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "n" #'lesson-next)
    (keymap-set map "p" #'lesson-prev)
    (keymap-set map "j" #'lesson-next)
    (keymap-set map "k" #'lesson-prev)
    (keymap-set map "g" #'lesson-goto)
    (keymap-set map "q" #'lesson-quit)
    map)
  "Keymap active in the source buffer while a lesson plan plays.")

(define-minor-mode lesson-source-mode
  "Minor mode for the source buffer while a lesson plan plays.
Makes the buffer read-only (restoring its prior state on exit) so the
highlighted regions stay put."
  :lighter " Lesson"
  :keymap lesson-mode-map
  (if lesson-source-mode
      (progn
        (setq lesson--saved-read-only buffer-read-only)
        (setq buffer-read-only t))
    (setq buffer-read-only lesson--saved-read-only)))


;;;; Display

(defun lesson--step-file (step)
  "Return the absolute source file for STEP, or nil to reuse the current buffer."
  (let ((file (or (plist-get step :file) lesson--default-file)))
    (and file (expand-file-name file lesson--base-dir))))

(defun lesson--highlight (lines)
  "Highlight the inclusive LINES range (START . END) in the current buffer."
  (when (overlayp lesson--overlay)
    (delete-overlay lesson--overlay))
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (car lines)))
    (let ((beg (line-beginning-position)))
      (goto-char (point-min))
      (forward-line (1- (cdr lines)))
      (let ((end (min (point-max) (1+ (line-end-position)))))
        (setq lesson--overlay (make-overlay beg end))
        (overlay-put lesson--overlay 'face 'lesson-highlight))))
  ;; Center the highlighted region on its midpoint, nudged a few lines above
  ;; center for visual balance (and to leave more room for the popup below).
  (let ((win (get-buffer-window (current-buffer))))
    (when win
      (set-window-point win (overlay-start lesson--overlay))
      (with-selected-window win
        (save-excursion
          (goto-char (/ (+ (overlay-start lesson--overlay)
                           (overlay-end lesson--overlay))
                        2))
          (recenter (max 0 (- (/ (window-body-height) 2) 4))))))))

(defun lesson--header ()
  "Progress summary shown atop the posframe."
  (format "Step %d/%d%s"
          (1+ lesson--index)
          (length lesson--steps)
          (if lesson--title
              (concat "  —  " lesson--title)
            "")))

(defun lesson--render (step)
  "Show STEP's explanation in the corner posframe."
  (unless (and (display-graphic-p) (featurep 'posframe))
    (user-error "Lesson needs posframe in a graphical frame"))
  (let ((buf (get-buffer-create lesson--posframe-buffer)))
    (with-current-buffer buf
      ;; Horizontal padding comes from the fringes (recolored to the fill
      ;; color below so they read as padding, not strips); vertical padding
      ;; from the blank lines around the text.  posframe has only one border,
      ;; so it can't double as padding.
      (setq-local truncate-lines nil
                  word-wrap t
                  face-remapping-alist
                  `((fringe (:background ,lesson-fill-color
                             :foreground ,lesson-fill-color))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n")
        (insert (propertize (lesson--header)
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
     :position (overlay-end lesson--overlay)
     :poshandler #'posframe-poshandler-point-bottom-left-corner
     :width (min lesson-posframe-width (/ (frame-width) 2))
     :left-fringe 16
     :right-fringe 16
     :internal-border-width 2
     :internal-border-color lesson-border-color
     :foreground-color lesson-text-color
     :background-color lesson-fill-color)))

(defun lesson--show-step (i)
  "Display step I (0-based)."
  (setq lesson--index i)
  (let* ((step (aref lesson--steps i))
         (file (lesson--step-file step))
         (src (if file
                  (if (file-exists-p file)
                      (find-file-noselect file)
                    (user-error "Lesson plan step %d: no such file: %s" (1+ i) file))
                lesson--source-buffer)))
    (cond
     ;; First step: take over the frame with the source buffer.
     ((null lesson--source-buffer)
      (setq lesson--window-config (current-window-configuration))
      (delete-other-windows)
      (switch-to-buffer src)
      (setq lesson--source-buffer src))
     ;; Later step in a different file: swap the buffer in place.
     ((not (eq src lesson--source-buffer))
      (when (buffer-live-p lesson--source-buffer)
        (with-current-buffer lesson--source-buffer
          (lesson-source-mode -1)))
      (switch-to-buffer src)
      (setq lesson--source-buffer src)))
    (with-current-buffer src
      (unless lesson-source-mode (lesson-source-mode 1))
      (lesson--highlight (plist-get step :lines)))
    (lesson--render step)))


;;;; Commands

(defun lesson-next ()
  "Advance to the next step."
  (interactive)
  (if (< (1+ lesson--index) (length lesson--steps))
      (lesson--show-step (1+ lesson--index))
    (message "Last step")))

(defun lesson-prev ()
  "Rewind to the previous step."
  (interactive)
  (if (> lesson--index 0)
      (lesson--show-step (1- lesson--index))
    (message "First step")))

(defun lesson-goto (n)
  "Jump to step N (1-based)."
  (interactive (list (read-number "Go to step: " (1+ lesson--index))))
  (let ((i (1- n)))
    (if (and (>= i 0) (< i (length lesson--steps)))
        (lesson--show-step i)
      (user-error "No step %d (have %d)" n (length lesson--steps)))))

(defun lesson-quit ()
  "End the lesson plan, restoring buffers and the previous window layout."
  (interactive)
  (when (overlayp lesson--overlay)
    (delete-overlay lesson--overlay))
  (when (buffer-live-p lesson--source-buffer)
    (with-current-buffer lesson--source-buffer
      (lesson-source-mode -1)))
  (when (featurep 'posframe)
    (posframe-delete lesson--posframe-buffer))
  (when lesson--window-config
    (set-window-configuration lesson--window-config))
  (setq lesson--steps nil
        lesson--overlay nil
        lesson--source-buffer nil
        lesson--window-config nil))

;;;###autoload
(defun lesson-start (data &optional base-dir)
  "Begin the lesson plan described by DATA, a plist (see Commentary).
BASE-DIR resolves relative `:file' paths (defaults to `default-directory')."
  (let ((steps (plist-get data :steps)))
    (unless steps (user-error "Lesson plan has no :steps"))
    (setq lesson--title (plist-get data :title)
          lesson--default-file (plist-get data :file)
          lesson--base-dir (or base-dir default-directory)
          lesson--steps (vconcat steps)
          lesson--index 0
          lesson--source-buffer nil
          lesson--window-config nil)
    (lesson--show-step 0)))

;;;###autoload
(defun lesson-load (file)
  "Load and play the lesson plan in FILE.
Interactively, prompt within `lesson-plans-directory'."
  (interactive
   (list (read-file-name "Lesson plan: "
                         (file-name-as-directory lesson-plans-directory)
                         nil t)))
  (let ((data (with-temp-buffer
                (insert-file-contents file)
                (read (current-buffer)))))
    (lesson-start data (file-name-directory (expand-file-name file)))))

(provide 'lesson)
;;; lesson.el ends here
