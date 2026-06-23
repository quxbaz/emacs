(:title "my/dired-ret"
 :file "/home/david/.emacs.d/my/commands.el"
 :steps
 ((:lines (278 . 291)
   :text "my/dired-ret is what RET runs in dired (it's bound to <return> in your dired-mode-hook). It has two completely separate jobs depending on whether you give it a prefix argument:

  • no prefix  → visit the file/dir at point
  • a number   → jump point to that line number

Let's take it apart.")

  (:lines (278 . 283)
   :text "The signature: an &optional LINE argument, plus a docstring spelling out the two modes.

The key UX detail is in the docstring: \"type the digits, then RET\". In dired you press a number first (11), which Emacs accumulates as a prefix argument, and *then* RET. So `11 RET` means \"go to line 11\" — no C-u needed.")

  (:lines (284 . 284)
   :text "(interactive \"P\") is the whole mechanism behind that.

\"P\" hands you the *raw* prefix argument: nil when there's no prefix, or the number you typed. (Contrast \"p\", lowercase, which would coerce nil to 1 — that would break the \"no prefix\" detection below, because you could never tell \"no arg\" from \"line 1\".)")

  (:lines (285 . 286)
   :text "The default case: no prefix.

dired-find-alternate-file visits the file or directory at point *in the same window*, killing the dired buffer (the \"alternate\" part). That's the \"replacing the dired buffer\" behavior from the docstring — you navigate into things without piling up dired buffers behind you.")

  (:lines (287 . 289)
   :text "The prefix case: LINE is a number, so jump there.

Go to the top of the buffer, move down LINE-1 lines, then dired-move-to-filename to land point on the filename column rather than at the start of the line. prefix-numeric-value turns the raw \"P\" value into an actual integer.")

  (:lines (290 . 291)
   :text "Finally, an optional twist.

If the user option my/dired-goto-line-visit is non-nil, don't just move point to that line — also visit it (again via dired-find-alternate-file). So you can configure `11 RET` to mean \"jump to line 11 and open it\" in one keystroke, or leave it off to just move the cursor.")))
