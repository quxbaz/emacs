# wire

Send the active region (or the line at point) plus a short annotation to a
**running** Claude Code session living in a [kitty](https://sw.kovidgoyal.net/kitty/)
terminal window.

The message wire builds includes the project root, the file, the line range,
and the code itself, fenced for the buffer's language — e.g.

```
this region has a bug

Project: /home/david/proj/
File: src/a.py (lines 10-12)

```python
def f():
    return 1
```
```

It is injected straight into Claude's prompt and auto-submitted.

## Why kitty

A process cannot write to another terminal's stdin on a modern kernel: the
`TIOCSTI` keystroke-injection syscall is disabled, and a PTY's master fd (the
end that feeds stdin) is held by the terminal emulator, not exposed on the
filesystem. kitty owns that fd and exposes a write command
(`kitty @ send-text`), so it is the transport. That's the whole reason kitty (or
tmux) is required rather than poking `/proc/<pid>/fd/0`.

## One-time setup

1. Enable kitty remote control. In `~/.config/kitty/kitty.conf`:

   ```
   allow_remote_control yes
   listen_on unix:/tmp/kitty
   ```

   Restart kitty (or launch with
   `-o allow_remote_control=yes --listen-on unix:/tmp/kitty`).

   > **Note:** kitty appends `-PID` to the `listen_on` path, so the real socket
   > is `/tmp/kitty-<pid>`, not `/tmp/kitty`, and there is one per kitty
   > instance. wire handles this automatically: `wire-kitty-socket` is treated
   > as a *base* and expanded with a `-*` glob, so all running instances are
   > discovered and each target remembers which socket owns it. You do **not**
   > need to chase the PID.

2. Sanity-check from a shell (substitute the real PID, or just run
   `M-x wire-doctor` which discovers it for you):

   ```
   ls /tmp/kitty-*
   kitty @ --to unix:/tmp/kitty-<pid> ls
   ```

3. Loaded automatically from `init.el` (autoloaded). To load manually:

   ```elisp
   (add-to-list 'load-path "~/.emacs.d/site-lisp/wire")
   (require 'wire)
   ```

## Usage

| Command                | Default key | What it does                                  |
|------------------------|-------------|-----------------------------------------------|
| `wire-mode`            | —           | Buffer-local minor mode enabling the keys     |
| `wire-dispatch`        | `C-c c c`   | Send region/line + annotation to the target   |
| `wire-select-target`   | `C-c c s`   | Choose which Claude window to target          |
| `wire-list-instances`  | `C-c c l`   | Echo the Claude windows kitty can see         |
| `wire-doctor`          | —           | Diagnose the kitty/remote-control setup       |

Typical flow:

1. `M-x wire-mode` in a source buffer.
2. `M-x wire-select-target` (auto-picks if only one Claude window).
3. Mark a region, `M-x wire-dispatch` (or `C-c c c`).
4. A `*wire annotation*` buffer pops up **pre-filled with the full message** —
   project, file, line range and the fenced code block. Point starts at the
   top, ready for a note. Edit anything you like; the buffer is sent verbatim.
   `C-c C-c` sends, `C-c C-k` cancels.

With no region active, the line at point is used. The target is session-only
and is re-validated on each dispatch — if its window has gone away, wire prompts
you to pick another.

## Configuration

| Variable                | Default              | Meaning                                         |
|-------------------------|----------------------|-------------------------------------------------|
| `wire-kitty-program`    | `"kitty"`            | kitty executable                                |
| `wire-kitty-socket`     | `"unix:/tmp/kitty"`  | Base socket (the `listen_on` value); auto-expanded to `<base>-PID` |
| `wire-process-regexp`   | `"claude"`           | Matches a window's foreground command line      |
| `wire-bracketed-paste`  | `"enable"`           | `send-text --bracketed-paste` value             |

## How sending works

`wire--send` does two `kitty @ send-text --stdin` calls to the matched window
id:

1. the message body, wrapped in bracketed paste so embedded newlines do not
   submit the prompt early;
2. a bare carriage return, to submit.

If your Claude TUI's paste handling differs, that function is the single place
to adjust (e.g. send the `\r` without `--stdin`, or append it to the body).

## Troubleshooting

Run `M-x wire-doctor`. It checks the kitty executable, socket reachability,
detected Claude instances, and the current target, and prints `[FAIL]`/`[warn]`
lines with the fix.
