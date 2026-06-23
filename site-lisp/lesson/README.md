# lesson

Play a guided tour of source code in Emacs: a sequence of steps, each
pairing a region of code with a prose explanation. The source file fills
the frame with the current step's region highlighted, and the
explanation floats in a popup ([posframe](https://github.com/tumashu/posframe))
just below it.

It's a presentation engine — it plays a static *lesson plan* and knows
nothing about how that plan was produced. You can hand-author one or
have a tool (e.g. an LLM) generate it.

## Requirements

- Emacs 27.1+
- [posframe](https://github.com/tumashu/posframe)
- A **graphical** frame (posframe doesn't work in a terminal)

## Install

Put `lesson.el` on your `load-path` and autoload the entry point:

```elisp
(let ((path (concat user-emacs-directory "site-lisp/lesson")))
  (when (file-exists-p (concat path "/lesson.el"))
    (add-to-list 'load-path path)
    (autoload 'lesson-load "lesson" "Load and play a lesson plan." t)
    (autoload 'lesson-start "lesson" nil t)))
```

## Usage

```
M-x lesson-load RET path/to/plan.el RET
```

`lesson-load` starts its prompt in `lesson-plans-directory` (default
`~/.emacs.d/lesson-plans/`), so dropping lesson plans there makes them
one keystroke away. See `example-lesson-plan.el` in this directory for an
annotated template.

Navigation (from the source buffer):

| Key       | Action            |
|-----------|-------------------|
| `n` / `j` | next step         |
| `p` / `k` | previous step     |
| `g`       | jump to a step    |
| `q`       | quit              |

While a lesson plan is playing the source buffer is read-only (so the
highlighted regions stay aligned); `q` restores it and your previous
window layout.

## Lesson-plan format

A lesson plan is a readable plist in a file:

```elisp
(:title "How the reader works"
 :file "lisp/lib.el"               ; default file for every step
 :steps
 ((:lines (12 . 18) :text "This binds the reader and ...")
  (:lines (20 . 24) :text "Here we recurse over the ...")
  ;; a step may override the file to span multiple files:
  (:file "util.el" :lines (3 . 9) :text "...which calls this helper.")))
```

- `:file` paths are resolved relative to the **lesson-plan file's
  directory** (absolute paths are used as-is).
- `:lines` is an inclusive `(START . END)` line range, 1-based.
- `:text` is the explanation shown in the popup for that step.

You can also build the data programmatically and call `lesson-start`
directly.

See [STYLE.md](STYLE.md) for guidance on writing good lesson plans, and
[example-lesson-plan.el](example-lesson-plan.el) for an annotated template.

## Customization

`M-x customize-group RET lesson RET`, or:

| Variable                   | Default                   | Meaning                       |
|----------------------------|---------------------------|-------------------------------|
| `lesson-plans-directory`   | `~/.emacs.d/lesson-plans` | where `lesson-load` looks     |
| `lesson-posframe-width`    | `80`                      | max popup width (chars)       |
| `lesson-border-color`      | `"white"`                 | popup border color            |
| `lesson-fill-color`        | `"#1d3557"`               | popup background (fill) color |
| `lesson-text-color`        | `"white"`                 | popup text color              |
| `lesson-highlight`         | face                      | the highlighted-region face   |
