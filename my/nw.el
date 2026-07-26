;; No-window configuration: terminal frames (emacs -nw, console, ssh)

;; Loaded last from init.el, and only when there is no window system. This
;; file layers on top of the graphical configuration: everything here either
;; repairs something that silently degrades without a GUI (fringes,
;; child-frame popups, the system clipboard) or restores a key a terminal
;; cannot send on its own.
;;
;; The graphical config stays authoritative. Apart from the `unless
;; window-system' loader in init.el, only two conditionals live outside this
;; file, and only because they run before it: the
;; highlight-indent-guides block in my/conf.el and the default-face font in
;; my/theme.el.


;; # Packages
;; Packages that make Emacs nw-compatible. They sit in
;; `package-selected-packages' (init.el) with everything else, but nothing
;; outside this file loads them, so a graphical session never touches them.
;; Each is pulled in with a `(require ... nil t)' guard: a missing package
;; degrades its feature instead of breaking startup.
;;
;; * kkp -- kitty keyboard protocol. Makes the terminal report keys together
;;   with their modifiers, which is what keeps the `<escape>' prefix map and
;;   C-; C-, C-. C-<tab> C-<return> and the rest working. See "Keyboard".
;; * corfu-terminal -- draws corfu's completion popup as overlay text rather
;;   than in a child frame, which a terminal cannot create. See "Completion".
;; * popon -- paints the floating box corfu-terminal asks for. A dependency of
;;   corfu-terminal, never used directly.
;; * xclip -- bridges kill and yank to the system clipboard by way of an
;;   external helper, xsel here. See "System clipboard".
;; * doom-themes -- supplies doom-outrun-electric, the terminal theme. Already
;;   installed for the graphical config; listed here because "Theme" uses it.


;; # Keyboard
;; A terminal cannot send most of the bindings in my/bindings.el. Escape *is*
;; the meta prefix there, so the whole `<escape> ...' map is unreachable, and
;; C-; C-, C-. C-< C-> C-<tab> C-<return> C-M-= s-n and friends have no
;; representation in the classic terminal key encoding at all.
;;
;; kkp speaks kitty's keyboard protocol, in which every key is reported with
;; its modifiers as a distinct escape sequence. Under a terminal that
;; implements it (kitty, foot, ghostty, wezterm, alacritty, recent xterm) a
;; terminal frame behaves like a graphical one: <escape> arrives as a real key
;; rather than as the meta prefix, C-; is distinguishable from ';', and the
;; bindings work exactly as written. On a terminal without the protocol
;; global-kkp-mode quietly does nothing and the fallbacks below take over.
;;
;; The one thing the protocol costs: C-g goes out as an escape sequence rather
;; than the raw quit byte, so it cannot interrupt a blocking subprocess call.
;; kkp restores the legacy encoding for the duration of such a call, but its
;; own option only advises `call-process', and the blocking primitives are
;; separate C functions that do not route through it -- magit runs
;; `process-file', wire's send path `call-process-region'. So take the option
;; (it is read when the mode turns on, hence the order) and extend the same
;; advice to the rest, off the mode's own hook so it comes and goes with the
;; mode the way kkp's does.
(defun my/nw-sync-kkp-subprocess-advice ()
  "Advise the blocking primitives kkp's own option leaves alone.
Follows `global-kkp-mode': attached while the mode is on, removed when it
goes off, so toggling the mode does not leave wrappers behind."
  (dolist (fn '(call-process-region process-file process-file-region))
    (if global-kkp-mode
        (advice-add fn :around #'kkp-restore-legacy-keys)
      (advice-remove fn #'kkp-restore-legacy-keys))))

(setq kkp-restore-legacy-keys-around-subprocesses t)
(when (require 'kkp nil t)
  (add-hook 'global-kkp-mode-hook #'my/nw-sync-kkp-subprocess-advice)
  (global-kkp-mode 1))


;; ## Fallbacks for terminals without the keyboard protocol
;; These are translations rather than bindings: `input-decode-map' rewrites the
;; typed sequence into the event the config already binds, so one entry covers
;; every keymap -- global, calc, magit, lisp -- with no duplicated bindings.
;; Stand-in prefixes, all under C-c to stay out of the way:
;;
;;   C-c c KEY  ->  C-KEY      (C-c c ; = C-;, C-c c RET = C-<return>)
;;   C-c a KEY  ->  C-M-KEY    (C-c a . = C-M-., "a" for alt)
;;   C-c s KEY  ->  S-KEY      (C-c s RET = S-<return>)
;;   C-c S KEY  ->  M-S-KEY    (C-c S RET = M-S-<return>; the shifted prefix
;;                              is the shifted one, plus meta)
;;   C-c u KEY  ->  s-KEY      (C-c u n = s-n, "u" for super)
;;   C-c e      ->  <escape>   (C-c e n = <escape> n; see below)
;;
;; Note that this reserves C-c c, C-c a, C-c s, C-c S, C-c u and C-c e as
;; prefixes on a terminal; typing one followed by a key with no translation
;; leaves the keys untouched, so nothing else is shadowed.
(defconst my/nw-modifier-prefixes
  '(("C-c c" . "C-") ("C-c a" . "C-M-") ("C-c s" . "S-")
    ("C-c S" . "M-S-") ("C-c u" . "s-"))
  "Terminal-reachable prefixes standing in for modifiers a terminal drops.")

(defconst my/nw-modifier-keys
  '(";" "," "." "/" "'" "`" "=" "-" "+" "<" ">" "[" "]" "\\" "SPC"
    "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  "Character keys reachable through `my/nw-modifier-prefixes'.")

(defconst my/nw-modifier-function-keys
  '(("RET" . "<return>") ("TAB" . "<tab>")
    ("DEL" . "<backspace>") ("<deletechar>" . "<delete>"))
  "Function keys reachable through `my/nw-modifier-prefixes'.
Car is what the terminal sends, cdr the function key the config binds:
C-<return> and C-M-<delete> are distinct events from C-RET and C-M-DEL,
so the sendable key has to be translated into the bound one.")

(dolist (prefix my/nw-modifier-prefixes)
  (dolist (key my/nw-modifier-keys)
    (define-key input-decode-map
                (kbd (concat (car prefix) " " key))
                (kbd (concat (cdr prefix) key))))
  (dolist (key my/nw-modifier-function-keys)
    (define-key input-decode-map
                (kbd (concat (car prefix) " " (car key)))
                (kbd (concat (cdr prefix) (cdr key))))))

;; Control and Meta reach letters unaided, so the lists above cover only the
;; keys that need help. Super has no terminal encoding whatsoever, so its
;; stand-in has to cover letters as well (s-n is the one super binding today).
(dolist (key (mapcar #'char-to-string (number-sequence ?a ?z)))
  (define-key input-decode-map
              (kbd (concat "C-c u " key))
              (kbd (concat "s-" key))))

;; Two keys a terminal can send, but as a different event than the one the
;; config binds: Meta-Enter arrives as M-RET (the character) instead of the
;; M-<return> function key, and Shift-Tab as <backtab>.
(dolist (pair '(("M-RET"           . "M-<return>")
                ("C-c c <backtab>" . "<C-iso-lefttab>")))
  (define-key input-decode-map (kbd (car pair)) (kbd (cdr pair))))

;; The `<escape> ...' map needs a route of its own. Without the protocol,
;; Escape is the meta prefix and the events it produces are meta-modified
;; characters, never the `escape' key the map is bound on, so every binding in
;; it is out of reach. (With the protocol, Escape reports as itself and the map
;; works untouched -- this is purely a fallback.) Translating to the bare event
;; rather than to a full sequence hands the rest of the lookup back to the map
;; itself, so this one entry covers all of it, `<escape> <escape> ...' included:
;; C-c e n is <escape> n, C-c e C-c e i is <escape> <escape> i.
(define-key input-decode-map (kbd "C-c e") [escape])

;; Inside a multiplexer the protocol is out of reach: tmux does not implement
;; the kitty keyboard protocol (3.4 has no notion of it), so kkp's query goes
;; unanswered and Escape is the meta prefix again no matter what the outer
;; terminal can do. tmux can be told to send something else for that one key,
;; though, and CSI 27 u is what the protocol itself sends for Escape -- so
;; decoding it here costs nothing and makes the two paths agree.
;;
;; The tmux side lives in ~/conf/tmux/tmux.conf. It rewrites Escape to CSI 27 u
;; in Emacs panes only, so everything else in the session keeps a plain Escape,
;; and prefix + e toggles even that off for an Emacs that does not load this
;; file. It also sets extended-keys, which is how the ctrl-punct keys (C-; C-,
;; C-. C-<return> C-<tab>) get through at all: Emacs turns on modifyOtherKeys by
;; itself under TERM=tmux* and TERM=screen*, but tmux forwards those keys only
;; when the option is on.
(define-key input-decode-map "\e[27u" [escape])


;; # Visuals
;; ## Theme
;; A different theme than the graphical config's modus-vivendi, so which kind of
;; session you are looking at is obvious at a glance. This is a plain
;; `load-theme' rather than my/switch-theme, and my/theme.el skips its own
;; load-theme when there is no window system: only one theme is ever loaded, so
;; there is nothing left behind from a disabled one.
;;
;; The rest of my/theme.el still applies on top: its face tweaks go through
;; `set-face-attribute' and `custom-set-faces' rather than a theme spec, so
;; load-theme does not override them. The violet-red mode line, the magit
;; highlight and the pulse color come through unchanged -- what differs is the
;; theme's own palette (background, syntax colors, region).
(load-theme 'doom-outrun-electric t)

;; ## Indent guides
;; The `bitmap' method needs a graphical display, and the face auto-setup reads
;; colors off the `default' face, which a terminal frame does not have when
;; my/conf.el runs (the theme has not loaded yet). That combination printed an
;; error-shaped message at every startup and left the faces unset -- not a
;; signal, just noise and no guides. Use the character method with an explicit
;; face instead.
;; my/conf.el skips its own highlight-indent-guides block when there is no
;; window system.
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-auto-enabled nil)
(when (require 'highlight-indent-guides nil t)
  ;; With the auto-setup off the guide face keeps its default (unstyled)
  ;; definition, so give it a foreground explicitly.
  (set-face-foreground 'highlight-indent-guides-character-face "gray30")
  (highlight-indent-guides-mode t))

;; ## Fringes
;; A terminal has no fringes, so anything drawn in one is invisible. diff-hl
;; can draw its hunk indicators in the margin instead.
(with-eval-after-load 'diff-hl
  (require 'diff-hl-margin)
  (diff-hl-margin-mode 1))

;; empty-line-mode is fringe-only by design, so it has nothing to draw here.
;; Turn it off rather than pay for overlays nothing displays.
(when (fboundp 'global-empty-line-mode)
  (global-empty-line-mode -1))

;; ## Mode line
;; The released-button box is a graphical effect; a terminal renders it as an
;; underline or not at all, so drop it. The colors carry over as-is: kitty and
;; other direct-color terminals report the full 16M.
(set-face-attribute 'mode-line nil :box nil)


;; # Completion
;; corfu shows its candidate popup in a child frame, which a terminal cannot
;; create -- global-corfu-mode is on but nothing ever appears. corfu-terminal
;; redraws the same popup as overlay text via popon.
(when (require 'corfu-terminal nil t)
  (corfu-terminal-mode 1))


;; # Mouse
;; Terminals report clicks and wheel motion as escape sequences; without this
;; they go to the terminal emulator instead of Emacs. The mouse-8 / mouse-9
;; (back / forward) bindings in my/bindings.el have no terminal equivalent and
;; stay dormant.
(xterm-mouse-mode 1)


;; # Commands
;; my/restart-emacs starts a fresh detached `emacs' rather than re-execing, to
;; keep the window class the WM matches on. In a terminal that is backwards:
;; the replacement comes up as a graphical frame while this session dies in the
;; terminal it was started from. There is no window class to protect here, so
;; re-exec in place -- the RESTART flag reuses `command-line-args', -nw
;; included, so Emacs comes back in this same terminal.
(defun my/nw-restart-emacs ()
  "Restart Emacs in place, in this terminal."
  (interactive)
  (let ((confirm-kill-emacs nil))
    (save-buffers-kill-emacs nil t)))

(advice-add 'my/restart-emacs :override #'my/nw-restart-emacs)


;; # System clipboard
;; x-select-enable-clipboard (my/conf.el) does nothing without a GUI: kills and
;; yanks stay inside Emacs. Two ways out, in order of preference.
;;
;; 1. A clipboard helper (xsel, xclip, wl-copy) talking to the display server.
;;    Copy and paste both work. This is the case when the terminal runs inside
;;    X or Wayland, e.g. emacs -nw in kitty.
;; 2. OSC 52, an escape sequence handing the text to the terminal emulator,
;;    which puts it on the clipboard on our behalf. This covers ssh: no display
;;    server needed, but it is copy-only, and the terminal has to implement it
;;    (kitty allows writes by default; see clipboard_control). Pasting back is
;;    the terminal's own paste key, which arrives as ordinary input.
;;
;; A Linux virtual console has neither: no display server to talk to and no OSC
;; 52 in console_codes(4). There the clipboard stays Emacs-internal.
(defun my/nw-osc52-copy (text)
  "Hand TEXT to the terminal emulator's clipboard using OSC 52."
  (let ((payload (base64-encode-string (encode-coding-string text 'utf-8) t)))
    (send-string-to-terminal
     ;; tmux swallows OSC 52 unless it is wrapped in a passthrough DCS -- and
     ;; only forwards it when allow-passthrough is on.
     (if (getenv "TMUX")
         (format "\ePtmux;\e\e]52;c;%s\a\e\\" payload)
       (format "\e]52;c;%s\a" payload)))))

(defun my/nw-clipboard-method ()
  "The xclip method this session can actually use, or nil if there is none.
Not `xclip-method' itself: that picks whichever helper turns up on PATH
first without regard for which display server is running -- xsel ahead of
wl-copy, though xsel only speaks to X -- and it falls back to the symbol
`xclip' even when no such program is installed, which makes `xclip-mode'
signal a file error as it turns on. So pick by display server, and only
after confirming the program is there. wl-copy needs its wl-paste sibling
too: xclip derives the paste command from the copy one by name."
  (when (require 'xclip nil t)
    (cond ((and (getenv "WAYLAND_DISPLAY")
                (executable-find "wl-copy")
                (executable-find "wl-paste"))
           'wl-copy)
          ((null (getenv "DISPLAY")) nil)
          ((executable-find "xclip") 'xclip)
          ((executable-find "xsel") 'xsel))))

(let ((method (my/nw-clipboard-method)))
  (cond (method
         ;; The method selects the code path, `xclip-program' is the binary it
         ;; actually runs, and the latter was computed from xclip's own choice
         ;; when the package loaded. Setting only the method runs the wrong
         ;; program. They are named alike, so one follows from the other.
         (setq xclip-method method
               xclip-program (symbol-name method))
         (xclip-mode 1))
        ((not (equal (tty-type) "linux"))
         (setq interprogram-cut-function #'my/nw-osc52-copy))))
