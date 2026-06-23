;; -*- lexical-binding: t; -*-
;;
;; ============================================================================
;; EXAMPLE LESSON PLAN  --  a template for authoring lesson plans
;; ============================================================================
;;
;; A lesson plan is a SINGLE readable plist (read with `read', so these `;'
;; comments are ignored). Play it with `M-x lesson-load' and pick this file.
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
;; Rendering of :text
;;   - Plain prose, word-wrapped in the popup; blank lines separate paragraphs.
;;   - Wrap symbols and code in Emacs `quotes' (a backtick and a single quote);
;;     they render in an accent color, like `lesson-load' above.
;;   - No other markup is interpreted.
;;   - The popup also shows, automatically, a "FUNCTION:" line naming the
;;     definition that encloses the step's region (a full signature in Lisp).
;;     You never write that line yourself.
;;
;; Authoring guidance (for humans and AI agents)
;;   - Keep each region focused: one idea per step. Prefer a whole defun or a
;;     tight cluster of lines over sprawling ranges.
;;   - Order steps as a narrative: lead with the entry point or the big
;;     picture, then drill into the pieces it relies on.
;;   - Verify line numbers against the CURRENT file before shipping a plan;
;;     they drift whenever the source changes.
;;   - Style: a single space after each period.
;;
;; This plan tours the lesson package itself, so it runs out of the box.
;; ============================================================================

(:title "A tour of the lesson package"
 :file "lesson.el"                      ; every step below reads lesson.el ...
 :steps
 ((:lines (143 . 153)
   :text "`lesson-source-mode' is the minor mode that runs in the code buffer \
while a plan plays.

Turning it on records the buffer's read-only state and forces the buffer \
read-only, so the line ranges a plan points at can't drift out from under it. \
Turning it off restores the original state.")

  (:lines (163 . 186)
   :text "`lesson--highlight' draws the step's region.

It turns the inclusive (START . END) line range into buffer positions, lays a \
`lesson-highlight' overlay over them (a background tint only, so syntax colors \
show through), then recenters the window on the region's midpoint to leave \
room for the popup below.")

  (:lines (253 . 296)
   :text "`lesson--render' builds the explanation popup with `posframe'.

It fills a hidden buffer — the step header, the automatic `FUNCTION:' line, \
then the prose — and shows it as a child frame anchored just below the \
highlighted region. The fringes are recolored to the fill color so they read \
as padding rather than stray strips.")

  (:lines (197 . 208)
   :text "`lesson--insert-prose' is what styles this very text.

After inserting the step's :text it scans for Emacs `quoted' spans with a \
regexp, drops the quote characters, and reinserts the inner token in the \
`lesson-code' accent face. That is why `lesson-load' and friends appear \
highlighted throughout this plan.")

  (:lines (372 . 385)
   :text "`lesson-start' is the engine's entry point.

Give it the lesson-plan plist and it stashes the title, default file, and \
steps, resets the index, and shows step 0. You can call it directly if you \
build plan data programmatically instead of reading it from a file.")

  ;; --- This step overrides :file to demonstrate spanning multiple files. ---
  (:file "README.md"
   :lines (55 . 72)
   :text "Finally, a step from a DIFFERENT file: the README's \"Lesson-plan \
format\" section.

Because this step sets its own :file, the plan jumps from `lesson.el' to \
`README.md'. That is all it takes for one plan to walk across several \
files.")))
