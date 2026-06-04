# wire — Quickstart

Send a region (or a whole file/buffer) plus a short note from Emacs
straight into the prompt of a **running** Claude Code session in a
[kitty](https://sw.kovidgoyal.net/kitty/) terminal — and have it submit
automatically.

## Why kitty?

A process can't write to another terminal's stdin on a modern kernel
(TIOCSTI is disabled, and the PTY master is held by the emulator). kitty
owns that fd and exposes a remote-control `send-text` command, so wire
uses kitty as the transport. That means **Claude must be running inside
a kitty window** for wire to reach it.

## Prerequisites

- Emacs 27.1+
- kitty, with remote control enabled (below)
- The `claude` CLI running in a kitty window

## One-time setup

### 1. Enable kitty remote control

In `~/.config/kitty/kitty.conf`:

```conf
allow_remote_control yes
listen_on unix:/tmp/kitty
```

Then **fully restart kitty** (not just a new tab). kitty appends `-PID`
to the socket per instance; wire finds it via a `-*` glob, so the base
`unix:/tmp/kitty` is all you configure.

### 2. Load wire in Emacs

wire is vendored under `site-lisp/wire/`. The `init.el` block autoloads
it when present. To turn it on everywhere:

```elisp
(global-wire-mode 1)
```

Or enable it per-buffer with `M-x wire-mode`.

### 3. Check the setup

```
M-x wire-doctor
```

This reports, in a `*wire-doctor*` buffer: whether the `kitty`
executable is found, which sockets are discovered and reachable, which
Claude instances are visible, and the current target. Start here if
anything misbehaves.

## First dispatch

1. Start Claude in a kitty window: `claude`
2. In Emacs, pick the target:  `C-c y s`  (`wire-select-target`)
   — if only one Claude is running it's chosen automatically.
3. Select a region (or select nothing to talk about the whole file),
   then dispatch:  `C-c y y`  (`wire-dispatch`)
4. A `*wire annotation*` buffer pops up, pre-filled with context and
   your cursor on a blank last line. Type your note.
5. Send it:  `C-c C-c`   (cancel with `C-c C-k`)
6. Jump to the Claude terminal to watch it work:  `C-c y v`
   (`wire-visit-target`)

## Keybindings

Under `wire-mode` (prefix `C-c y`):

| Key       | Command               | Does                                   |
|-----------|-----------------------|----------------------------------------|
| `C-c y y` | `wire-dispatch`       | Compose & send region / whole file     |
| `C-c y s` | `wire-select-target`  | Pick which Claude window to target     |
| `C-c y l` | `wire-list-instances` | Echo the Claude windows kitty sees     |
| `C-c y c` | `wire-visit-target`   | Focus/raise the target's kitty window  |
| `C-c y d` | `wire-doctor`         | Diagnose the wire/kitty setup          |

In the `*wire annotation*` buffer:

| Key       | Does            |
|-----------|-----------------|
| `C-c C-c` | Send to Claude  |
| `C-c C-k` | Cancel          |

## What gets sent

wire prepends a small context header, then your note. The buffer
contents are sent **verbatim** — edit them freely before sending.

**File-backed buffer, with a region:**

````
Project: /home/you/proj/
Branch: main
File: src/foo.el
Lines: 30-42

```emacs-lisp
<the selected code>
```

<your note>
````

**File-backed buffer, no region** — the message is about the whole file
(no line range, no code block; Claude opens the file itself):

```
Project: /home/you/proj/
Branch: main
File: src/foo.el

<your note>
```

**Fileless buffer** (`*scratch*`, dired, `*Messages*`, …): the header
uses `Buffer:`/`Mode:` instead of `File:`. With a region it adds a
`Focused: lines N-M of TOTAL` provenance line and the fenced text; with
no region it's the header plus your note.

## Customization

| Variable                | Default            | Purpose                                            |
|-------------------------|--------------------|----------------------------------------------------|
| `wire-kitty-program`    | `"kitty"`          | Name/path of the kitty executable                  |
| `wire-kitty-socket`     | `"unix:/tmp/kitty"`| Base socket, matching `listen_on` in kitty.conf    |
| `wire-process-regexp`   | `"claude"`         | Matches a kitty window's foreground command line   |
| `wire-bracketed-paste`  | `"enable"`         | Wrap the body in bracketed-paste markers on send   |

## Troubleshooting

Run `M-x wire-doctor` first. Common cases:

- **No socket found** → remote control isn't on, or kitty wasn't fully
  restarted after editing `kitty.conf`. Re-check step 1.
- **No Claude instances** → Claude isn't running in a kitty window, or
  its command line doesn't match `wire-process-regexp`. Confirm with
  `C-c y l`.
- **Send fails** → the target window was closed; re-pick with `C-c y s`.
