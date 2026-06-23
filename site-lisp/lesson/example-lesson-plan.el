;; -*- lexical-binding: t; -*-
;;
;; ============================================================================
;; EXAMPLE LESSON PLAN  --  a template for authoring lesson plans
;; ============================================================================
;;
;; A lesson plan is a SINGLE readable plist (read with `read', so these `;'
;; comments are ignored).  Play it with `M-x lesson-load' and pick this file.
;;
;; Schema
;; ------
;;   (:title  STRING            ; shown in the popup header (optional)
;;    :file   PATH              ; default source file for every step (optional)
;;    :steps  (STEP STEP ...))  ; ordered list of steps (required)
;;
;; Each STEP is a plist:
;;   (:lines (START . END)      ; inclusive 1-based line range (required)
;;    :text  STRING             ; the explanation for this step (required)
;;    :file  PATH)              ; overrides the plan :file for this step
;;                              ;   (optional; lets a plan span files)
;;
;; Path resolution
;;   :file paths are resolved relative to THIS lesson-plan file's directory.
;;   Absolute paths are used as-is.
;;
;; Authoring guidance (for humans and AI agents)
;;   - Keep each region focused: one idea per step.  Prefer a whole defun or
;;     a tight cluster of lines over sprawling ranges.
;;   - Order steps as a narrative: lead with the entry point or the big
;;     picture, then drill into the pieces it relies on.
;;   - Write :text as plain prose, a few short sentences.  It is word-wrapped
;;     in the popup; blank lines separate paragraphs.  No markdown is rendered.
;;   - Verify line numbers against the CURRENT file before shipping a plan;
;;     they drift whenever the source changes.
;;
;; This plan tours the lesson package itself, so it runs out of the box.
;; ============================================================================

(:title "A tour of the lesson package"
 :file "lesson.el"                      ; every step below reads lesson.el ...
 :steps
 ((:lines (130 . 140)
   :text "`lesson-source-mode' is the minor mode that runs in the code buffer \
while a plan plays.

Turning it on records the buffer's read-only state and forces the buffer \
read-only, so the line ranges a plan points at can't drift out from under \
it.  Turning it off restores the original state.")

  (:lines (150 . 173)
   :text "`lesson--highlight' draws the step's region.

It converts the inclusive (START . END) line range into buffer positions, \
lays a `lesson-highlight' overlay over them, then recenters the window on the \
region's midpoint (nudged slightly above center) to leave room for the popup \
underneath.")

  (:lines (184 . 221)
   :text "`lesson--render' builds the explanation popup with posframe.

The text is laid out in a hidden buffer (with blank-line and fringe padding), \
then shown as a child frame anchored just below the highlighted region.  The \
fringes are recolored to the fill color so they read as padding rather than \
stray strips.")

  (:lines (295 . 307)
   :text "`lesson-start' is the engine's entry point.

Give it the lesson-plan plist and it stashes the title, default file, and \
steps, resets the index, and shows step 0.  You can call it directly if you \
build plan data programmatically instead of reading it from a file.")

  ;; --- This step overrides :file to demonstrate spanning multiple files. ---
  (:file "README.md"
   :lines (55 . 72)
   :text "Finally, a step from a DIFFERENT file: the README's \"Lesson-plan \
format\" section.

Because this step sets its own :file, the plan jumps from lesson.el to \
README.md.  That's all it takes for one plan to walk across several \
files.")))
