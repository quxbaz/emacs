# tutor

Play a guided tour of source code in Emacs: a sequence of steps, each
pairing a region of code with a prose explanation. The source file fills
the frame with the current step's region highlighted, and the
explanation floats in a popup ([posframe](https://github.com/tumashu/posframe))
just below it.

It's a presentation engine — it plays a static *script* and knows
nothing about how that script was produced. You can hand-author one or
have a tool (e.g. an LLM) generate it.

## Requirements

- Emacs 27.1+
- [posframe](https://github.com/tumashu/posframe)
- A **graphical** frame (posframe doesn't work in a terminal)

## Install

Put `tutor.el` on your `load-path` and autoload the entry point:

```elisp
(let ((path (concat user-emacs-directory "site-lisp/tutor")))
  (when (file-exists-p (concat path "/tutor.el"))
    (add-to-list 'load-path path)
    (autoload 'tutor-load "tutor" "Load and play a tutor script." t)
    (autoload 'tutor-start "tutor" nil t)))
```

## Usage

```
M-x tutor-load RET path/to/script.el RET
```

Navigation (from the source buffer):

| Key       | Action            |
|-----------|-------------------|
| `n` / `j` | next step         |
| `p` / `k` | previous step     |
| `g`       | jump to a step    |
| `q`       | quit              |

While a tutor is running the source buffer is read-only (so the
highlighted regions stay aligned); `q` restores it and your previous
window layout.

## Script format

A script is a readable plist in a file:

```elisp
(:title "How the reader works"
 :file "lisp/lib.el"               ; default file for every step
 :steps
 ((:lines (12 . 18) :text "This binds the reader and ...")
  (:lines (20 . 24) :text "Here we recurse over the ...")
  ;; a step may override the file to span multiple files:
  (:file "util.el" :lines (3 . 9) :text "...which calls this helper.")))
```

- `:file` paths are resolved relative to the **script file's directory**
  (absolute paths are used as-is).
- `:lines` is an inclusive `(START . END)` line range, 1-based.
- `:text` is the explanation shown in the popup for that step.

You can also build the data programmatically and call
`tutor-start` directly.

## Customization

`M-x customize-group RET tutor RET`, or:

| Variable                    | Default   | Meaning                          |
|-----------------------------|-----------|----------------------------------|
| `tutor-posframe-width`| `80`      | max popup width (chars)          |
| `tutor-border-color`  | `"white"` | popup border color               |
| `tutor-fill-color`    | `"blue"`  | popup background (fill) color    |
| `tutor-text-color`    | `"white"` | popup text color                 |
| `tutor-highlight`     | face      | the highlighted-region face      |
