# wire — Quickstart

wire sends the active region — or, with no region, a message about the
whole file/buffer — plus a short note from Emacs into the prompt of a
**running** Claude Code session in a kitty terminal, and submits it.

## Setup

In `~/.config/kitty/kitty.conf`, then fully restart kitty:

```conf
allow_remote_control yes
listen_on unix:/tmp/kitty
```

In Emacs: `(global-wire-mode 1)` (or `M-x wire-mode` per buffer).
Verify the setup with `M-x wire-doctor`.

## Commands

| Key       | Command               | Does                                  |
|-----------|-----------------------|---------------------------------------|
| `C-c y y` | `wire-dispatch`       | Compose & send region / whole file    |
| `C-c y s` | `wire-select-target`  | Pick which Claude window to target    |
| `C-c y l` | `wire-list-instances` | List the Claude windows kitty sees    |
| `C-c y c` | `wire-visit-target`   | Focus the target's kitty window       |
| `C-c y d` | `wire-doctor`         | Diagnose the wire/kitty setup         |

In the `*wire annotation*` buffer: `C-c C-c` sends, `C-c C-k` cancels.

## Workflow

Mark a region (or don't), `C-c y y`, type your note, `C-c C-c`.
