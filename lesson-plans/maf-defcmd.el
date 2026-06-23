;; -*- lexical-binding: t; -*-
;;
;; Lesson plan: a detailed tour of the `maf-defcmd' macro and the
;; resolve/commit machinery it glues together.  Play with M-x lesson-load.

(:title "maf-defcmd: anatomy of a contextual calc command"
 :file "/home/david/.emacs.d/site-lisp/maf/src/maf-defcmd.el"
 :steps
 ((:lines (1 . 7)
   :text "This file defines `maf-defcmd', the macro at the heart of MAF's \
command layer.

The motivating problem: a calc command shouldn't care whether you're acting \
on the formula at the stack top, a sub-expression you've selected, the thing \
under the cursor inside an entry, or both sides of an equation.  You want to \
write the math once.  `maf-defcmd' captures that math in a body and wraps it \
in machinery that, at call time, resolves WHAT to act on and WHERE to put the \
answer.")

  (:lines (9 . 12)
   :text "The dependencies already sketch the architecture.

`calc' is required because the expansion uses `calc-wrapper'.  The other \
three are MAF's own layers: `maf-lib' (utilities), `maf-resolve' (turn point \
+ stack state into a context descriptor), and `maf-commit' (write a result \
back to the right place).

The macro is essentially glue between resolve and commit — we'll visit both \
files near the end of this lesson.")

  (:lines (14 . 17)
   :text "First, four tiny parsers for the macro's argument tail.

`maf--defcmd-parse-docstring' just returns the leading string, if there is \
one.  Splitting the parsing into single-purpose helpers keeps the macro body \
readable and each rule independently testable.")

  (:lines (19 . 27)
   :text "`maf--defcmd-parse-opts' collects the keyword/value option pairs.

It first drops a leading docstring (if present), then walks the list while \
its head is a keyword, pushing each (KEY . VALUE) onto an alist.  This is \
what lets you write options like :arity 'binary before the body.")

  (:lines (29 . 35)
   :text "`maf--defcmd-validate-opts' enforces the one hard requirement.

Every defcmd must declare :arity, and it must be exactly `unary' or \
`binary'.  Validation runs at macro-expansion time, so a malformed defcmd \
fails when you compile or load it — not later, at call time.")

  (:lines (37 . 42)
   :text "`maf--defcmd-parse-body' returns what's left: the actual code.

Like the others it defensively re-skips the optional docstring and every \
keyword/value pair, then hands back the remaining forms.  Everything after \
the options is the body.")

  (:lines (44 . 48)
   :text "`maf--defcmd-parse-rest' ties the three together, returning a list \
(DOCSTRING OPTS BODY) that the macro can destructure in one shot.

Note the design tradeoff: each sub-parser re-scans from the front rather than \
sharing a cursor.  Simpler to reason about, at the cost of skipping the \
docstring/options prefix three times.")

  (:lines (50 . 64)
   :text "Now the macro itself — and the contract you write against:

  (maf-defcmd NAME (EXPR ARG COMMIT) ...)

The BINDINGS triple names the locals your body sees.  EXPR is the operand, \
already resolved from context: the home formula, a selected sub-expression, \
the expression under point, or — for an equation — one side at a time.  ARG \
is the second operand for binary commands, taken from the stack top (nil for \
unary).  COMMIT is a local function: call it with your result to write it \
back.")

  (:lines (66 . 80)
   :text "The rest of the docstring spells out REST and OPTS.

REST is, in order: an optional docstring, then keyword/value option pairs, \
then the body.  :arity (required) decides whether ARG is pulled from the \
stack; :prefix labels the operation in calc's trail.

Crucially, any OTHER keyword is merged verbatim into the resolved context \
alist — that's the extension point resolve and commit read from.  The final \
paragraph is the call-time story we're about to watch the expansion build.")

  (:lines (81 . 86)
   :text "The expansion begins.

`declare' sets indentation (the body indents like a `defun') and marks \
argument 3 as the doc string for editors.  The `pcase-let*' destructures the \
parsed REST into docstring/opts/body and the BINDINGS into expr/arg/commit.

The three `gensym' calls — context, lhs, rhs — mint uninterned symbols so the \
generated code can bind them with no chance of shadowing the user's own \
variables.  That's macro hygiene done by hand.")

  (:lines (87 . 90)
   :text "Two things happen as the output form is assembled.

`maf--defcmd-validate-opts' runs NOW, during expansion, rejecting a bad \
:arity early.  Then we start emitting the command: a plain zero-argument \
`defun' named NAME, with the user's docstring spliced in only if they \
supplied one, and `(interactive)' so it is a real, callable command.")

  (:lines (91 . 95)
   :text "The whole body is wrapped in `calc-wrapper'.

That calc macro makes the command a single undoable unit and runs calc's \
epilogue — updating the trail, refreshing and renumbering the stack, and \
repositioning point.

Inside it, `maf--resolve-context' turns the live point + stack state into the \
CONTEXT alist (using the quoted OPTS), and ARG is bound once from :arg in \
that context, so binary commands see their stack operand.")

  (:lines (96 . 100)
   :text "Here the expansion forks on the kind of target.

If the resolved :target is `equation', the body must run once per side.  The \
comment states the plan: bind EXPR to the LHS and run the body, then to the \
RHS and run it again, capturing each side's committed result, then \
reassemble.

ARG was bound once *above* the fork, so both sides deliberately share the \
same second operand.")

  (:lines (101 . 107)
   :text "The per-side machinery.

`lhs' and `rhs' start empty.  For each side, EXPR is let-bound to that side's \
expression and — the clever part — COMMIT is locally redefined with `cl-flet' \
to simply STASH the value into `lhs' (or `rhs') instead of touching the \
stack.

So the exact same body code, calling COMMIT once, transparently feeds a local \
variable here.")

  (:lines (108 . 109)
   :text "After both sides have run, reassemble and commit once.

A new relation is built as (REL-OP LHS RHS) — the original relation operator \
with the two transformed sides — and handed to `maf--commit' a single time.

The user wrote one COMMIT per side; the macro folded that into one stack \
write for the whole equation.")

  (:lines (110 . 113)
   :text "The ordinary path is the simple mirror image.

For every non-equation target the body runs once: EXPR is bound to the \
resolved :expr, and COMMIT is `cl-flet'-bound directly to `maf--commit'.  So \
calling COMMIT writes straight back to the location the context picked.

Same body, same COMMIT calls — only the wiring behind COMMIT differs between \
the two paths.")

  ;; ---- Cross-file: the two sides the macro glues together ----
  (:file "/home/david/.emacs.d/site-lisp/maf/src/maf-resolve.el"
   :lines (184 . 209)
   :text "Jump to another file now: `maf--resolve-context', the function the \
expansion called.

It inspects point and calc state and returns the CONTEXT alist.  The `cond' \
tries targets in priority order — an active selection wins, then point at or \
below the home (.) line, then an implicit sub-expression under point, then a \
relation (equation), then the entry margins.

The matched target contributes :target/:expr/:arg; then the defcmd's OPTS and \
some ambient calc flags (:keep) are appended.  THIS is where EXPR comes \
from.")

  (:file "/home/david/.emacs.d/site-lisp/maf/src/maf-commit.el"
   :lines (19 . 33)
   :text "And the far side: `maf--commit', which COMMIT ultimately calls.

It reads the bookkeeping the context computed — :target, :prefix, the \
push/pop counts :commit-m / :commit-n / :post-pop, and the stack index :m — \
then dispatches on target.

Those numbers encode how many entries to pop and where to push, so a binary \
command consumes its operand from the stack correctly.")

  (:file "/home/david/.emacs.d/site-lisp/maf/src/maf-commit.el"
   :lines (34 . 52)
   :text "The selection / subexpr case is the subtle one.

The body received a CLEAN :expr and returned a clean value, but that value \
must be spliced back into the larger formula.  :expr-ref is the original \
encased sub-expression cons; `calc-replace-sub-formula' swaps it for the \
(pre-encased) new value inside the full formula.

Pre-encasing here lets the same cons double as the restored selection \
(:reselect), so the rebuilt entry stays selected right where it was.")

  (:file "/home/david/.emacs.d/site-lisp/maf/src/maf-commit.el"
   :lines (53 . 56)
   :text "The remaining targets are plain pushes.

`home' and `entry' just push the value (popping per :commit-n, consuming a \
binary arg via :post-pop).  For `equation', notice the value is ALREADY the \
full relation the macro reassembled from both sides — commit knows nothing \
about sides.

That separation is the whole design: resolve decides the where, the macro \
orchestrates per-side, and commit just writes.")))
