# Lesson-plan style guide

How to author good lesson plans for the `lesson` package. Written for both
humans and AI agents. For the data format and player commands, see
`README.md`; for a working template, see `example-lesson-plan.el`.

## What a lesson plan is

A lesson plan is a single readable plist in a `.el` file. It is *data*, not
code — the player reads it with `read`, so `;` comments are ignored. It pairs
regions of source code with short prose explanations, played one step at a
time.

```elisp
(:title "..."            ; optional, shown in the header
 :file "path.el"         ; optional default file for every step
 :steps ((:lines (START . END) :text "..." [:file "other.el"]) ...))
```

## Choosing regions

- **One idea per step.** A step should make a single point. If the prose needs
  the word "and" to join two unrelated observations, split it into two steps.
- **Prefer a whole definition** (a `defun`, `defmacro`, block) over an arbitrary
  slice, when the point is about that definition. Tight clusters of lines are
  fine when the point is local.
- **Avoid sprawling ranges.** A region taller than roughly a screen defeats the
  highlight — the reader can't see what you mean. Break it up.
- `:lines` is an inclusive `(START . END)` range, 1-based, matching what you see
  in the buffer.

## Ordering steps

Tell a story. Lead with the entry point or the big picture, then drill into the
pieces it depends on. A reader stepping `n` through the plan should feel the
explanation build, not jump around. End where the threads tie together.

## Writing `:text`

- **Plain prose**, a few short sentences per step. The popup is narrow; aim for
  one or two short paragraphs.
- **Paragraphs** are separated by a blank line inside the string.
- **A single space after each period.**
- **Emphasize code with Emacs quotes:** wrap a symbol or expression in a
  backtick and a single quote, like `` `lesson-load' ``. It renders in the
  accent color and the quotes are dropped. Use it for every function, variable,
  and keyword you name.
- **No other markup** is interpreted — no Markdown, no headings, no lists.
- Don't restate the code. Say what isn't obvious from reading it: the intent,
  the tradeoff, the thing that connects this region to the rest.

## What you do *not* write

The popup automatically adds a `FUNCTION:` line naming the definition that
encloses the step's region — a full `*Help*`-style signature in Lisp buffers
(e.g. `(lesson-start DATA &optional BASE-DIR)`), or the enclosing name
elsewhere. Never write that line into `:text`; it is derived from the code.

## Spanning files

- A step may set its own `:file` to jump to a different file. Everything else in
  the plan keeps using the plan-level `:file`.
- **Path resolution:** `:file` is resolved relative to the lesson-plan file's
  own directory; absolute paths are used as-is. Prefer absolute paths when a
  plan lives apart from the code it tours (e.g. in `lesson-plans/`), and
  relative paths when the plan ships beside the code (e.g. inside a package).

## Line numbers drift

Line numbers are the fragile part of a plan. They are correct only against the
exact version of the file you read. **Re-verify every `:lines` range against the
current file before shipping or editing a plan** — a plan that highlights the
wrong region is worse than no plan.

## Quick checklist

- [ ] Each step makes one point.
- [ ] Steps form a narrative, big picture first.
- [ ] Regions are focused (roughly a screen or less).
- [ ] Every named symbol is in `` `quotes' ``.
- [ ] Single space after periods; blank lines between paragraphs.
- [ ] No hand-written `FUNCTION:` lines.
- [ ] `:file` paths resolve (absolute when apart from the code).
- [ ] `:lines` verified against the current file.
