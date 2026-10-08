;; -*- lexical-binding: t; -*-
;;
;; my/maf-desmos.el
;;
;; A personal maf module: plotting in the Desmos calculator. maf
;; itself ships no Desmos backend, since the Desmos API is not licensed
;; for distribution, so the module lives in this config and registers
;; with maf's module system like any of maf's own. On, g o plots the
;; entry at point in a browser (H g o: every stack entry); a prefix
;; argument prompts for the x bounds the viewport opens on.
;;
;; The formula goes over, not samples — Desmos resamples as you zoom,
;; and calc's own LaTeX language (`maf--latex-string') is the whole
;; translation layer, with the fixes below for the dialect Desmos
;; reads. Relations go whole; a vector of numbers goes as index→value
;; points and a point set as its points. The handoff is the URL
;; fragment of a fixed local page, maf-desmos.html beside this file.
;;
;; Builds on maf-plot's internals (targets, range prompt, entry
;; predicates), so it loads after maf-plot.

(require 'cl-lib)
(require 'url-util)                ; url-hexify-string
(require 'browse-url)              ; browse-url-generic-program
(require 'maf-plot)

(defconst maf-desmos--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory this file loads from; maf-desmos.html sits beside it.")

(defcustom maf-desmos-browser nil
  "Browser program `maf-desmos-plot' launches, or nil to detect one.
Nil looks for a browser rather than giving up: see
`maf-desmos--browser' for the order. Setting this overrides the
search outright, including its refusal to hand the URL to a generic
opener — the URL must reach the browser binary intact, since the
xdg-open route silently drops the fragment of a file:// URL and the
fragment is the whole graph."
  :type '(choice (const :tag "Detect automatically" nil) string)
  :group 'maf)

(defcustom maf-desmos-api-key "dcb31709b452b1cf9dc26972add0fda6"
  "API key the Desmos page loads calculator.js with.
Desmos's published demo key. The key travels in the URL fragment —
the page itself is a fixed asset."
  :type 'string
  :group 'maf)

;;; Entries

(defun maf-desmos--expressions (entries)
  "Flatten ENTRIES for Desmos: a vector entry contributes per element.
Relations stay whole — Desmos graphs equations natively. A vector of
numbers becomes index→value points, one preformatted latex string
per element (Desmos draws a bare coordinate pair as a point), and a
point set its pairs, one point each — a lone [x, y] just the one."
  (mapcan (lambda (entry)
            (cond
             ((maf-plot--point-set-p entry)
              (mapcar (lambda (row)
                        (format "\\left(%s,%s\\right)"
                                (maf--latex-string (nth 1 row))
                                (maf--latex-string (nth 2 row))))
                      (maf-plot--points entry)))
             ((maf-plot--data-vector-p entry)
              (let ((index 0))
                (mapcar (lambda (v)
                          (format "\\left(%d,%s\\right)"
                                  (cl-incf index) (maf--latex-string v)))
                        (cdr entry))))
             ((eq (car-safe entry) 'vec) (copy-sequence (cdr entry)))
             (t (list entry))))
          entries))

;;; LaTeX

;; Desmos reads LaTeX, but not calc's dialect of it. Calc writes
;; function arguments in braces (\cos{x}, and parens only when the
;; argument needs them structurally) where Desmos requires parens, and
;; absolute value as bare pipes (|x + 1|) where Desmos requires
;; \left|...\right|. The fixes below operate on calc's output grammar,
;; which is known and regular — this is not general LaTeX rewriting.
;; Pipes are the one thing a string pass cannot repair (nested abs
;; makes their pairing ambiguous), so abs is lifted out of the
;; expression before formatting; exp goes to e^x the same way, a form
;; both sides agree on.

(defconst maf-desmos--brace-functions
  '("sin" "cos" "tan" "sec" "csc" "cot"
    "arcsin" "arccos" "arctan"
    "sinh" "cosh" "tanh" "arcsinh" "arccosh" "arctanh"
    "ln")
  "Function names whose brace argument becomes parens for Desmos.
Sub/superscripted forms (\\log_{10}) and structural braces (\\sqrt,
\\frac) are untouched — the match requires the brace directly after
the name.")

(defun maf-desmos--parenthesize (latex)
  "Return LATEX with \\func{arg} rewritten to \\func\\left(arg\\right).
Only for `maf-desmos--brace-functions'; the argument's own braces
are respected by depth."
  (with-temp-buffer
    (insert latex)
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\\\" (regexp-opt maf-desmos--brace-functions) "{")
            nil t)
      (let ((depth 1))
        (delete-char -1)
        (insert "\\left(")
        (while (and (> depth 0) (not (eobp)))
          (pcase (char-after)
            (?{ (setq depth (1+ depth)))
            (?} (setq depth (1- depth))))
          (if (and (zerop depth) (eq (char-after) ?}))
              (progn (delete-char 1) (insert "\\right)"))
            (forward-char 1)))))
    (buffer-string)))

(defvar maf-desmos--lifted nil
  "Placeholder-to-latex pairs collected while lifting abs nodes.")

(defun maf-desmos--lift (expr)
  "Return EXPR with abs subtrees as placeholder vars, exp as e^x.
Each lifted abs's finished latex is pushed on `maf-desmos--lifted'
under its placeholder's printed name."
  (pcase (car-safe expr)
    ('calcFunc-abs
     (let* ((name (format "mafabs%c" (+ ?a (length maf-desmos--lifted))))
            (placeholder (list 'var (intern name) (intern (concat "var-" name)))))
       (push (cons name
                   (concat "\\left|"
                           (maf-desmos--latex (nth 1 expr))
                           "\\right|"))
             maf-desmos--lifted)
       placeholder))
    ('calcFunc-exp
     (list '^ '(var e var-e) (maf-desmos--lift (nth 1 expr))))
    (_ (if (consp expr)
           (cons (car expr) (mapcar #'maf-desmos--lift (cdr expr)))
         expr))))

(defun maf-desmos--latex (expr)
  "Format EXPR as LaTeX in the dialect Desmos parses."
  (let* ((maf-desmos--lifted nil)
         (latex (maf-desmos--parenthesize
                 (maf--latex-string (maf-desmos--lift expr)))))
    (dolist (lifted maf-desmos--lifted latex)
      (setq latex (string-replace (car lifted) (cdr lifted) latex)))))

(defconst maf-desmos--known-vars '(x y e pi phi gamma i inf uinf nan)
  "Variable names Desmos already reads: the axes and the constants.")

(defun maf-desmos--normalize (expr)
  "Return EXPR with a lone foreign free variable renamed to x.
Desmos graphs in x and y; \\sin(t) as sent would offer a slider for
t where a curve is meant. The rename fires only when exactly one
variable is neither an axis nor a constant Desmos knows and x itself
is absent; anything else keeps its variables — a slider is the right
offer for a genuinely multi-variable expression."
  (let* ((vars (cl-delete-duplicates (maf--expr-vars expr) :test #'equal))
         (names (mapcar (lambda (v) (nth 1 v)) vars))
         (foreign (cl-remove-if
                   (lambda (v) (memq (nth 1 v) maf-desmos--known-vars))
                   vars)))
    (if (and foreign (null (cdr foreign)) (not (memq 'x names)))
        (math-expr-subst expr (car foreign) '(var x var-x))
      expr)))

(defun maf-desmos--url (entries &optional range)
  "Return the local Desmos page URL plotting ENTRIES.
The fragment is the whole handoff: a URI-encoded JSON object with the
entries as calc-formatted LaTeX (relations go whole — Desmos graphs
equations natively; a preformatted string passes through), the angle
mode, the API key the page loads calculator.js with, and — when
RANGE is given — the x bounds the viewport opens on. Nothing is
generated per plot; the page is a fixed asset and the URL is the
graph."
  (let ((page (expand-file-name "maf-desmos.html" maf-desmos--directory)))
    (unless (file-exists-p page)
      (error "maf-desmos.html missing beside maf-desmos.el"))
    (concat "file://" page "#"
            (url-hexify-string
             (json-serialize
              (append
               (list :e (vconcat
                         (mapcar (lambda (e)
                                   (if (stringp e) e
                                     (maf-desmos--latex
                                      (maf-desmos--normalize e))))
                                 entries))
                     :d (if (eq calc-angle-mode 'deg) t :false)
                     :k maf-desmos-api-key)
               (and range (list :b (vector (float (car range))
                                           (float (cdr range)))))))))))

;;; Browser

(defconst maf-desmos--browser-openers
  '("xdg-open" "gio" "gvfs-open" "gnome-open" "kde-open" "kde-open5"
    "exo-open" "open")
  "Generic openers `maf-desmos--browser' will not return.
None of these is a browser: each hands the URL to the desktop's
handler, and that hop resolves a file:// URL to a bare path. The
fragment does not survive it, and the fragment is the whole graph, so
one of these would open an empty calculator rather than fail.")

(defconst maf-desmos--browser-candidates
  '("firefox" "librewolf" "waterfox" "firefox-esr"
    "google-chrome-stable" "google-chrome" "chromium" "chromium-browser"
    "brave-browser" "brave" "vivaldi-stable" "vivaldi"
    "microsoft-edge" "opera" "epiphany" "qutebrowser")
  "Browser binaries probed on PATH as a last resort.
Only reached when nothing — setting, environment, or desktop — names
a browser. Any of these takes a file:// URL with its fragment intact.")

(defun maf-desmos--browser-desktop-exec (desktop)
  "Return the program the XDG DESKTOP entry runs, or nil.
DESKTOP is a .desktop file name, as `xdg-settings' reports it. The
file is looked for under the XDG application directories, and its
first Exec= line read for the program word — the %U-style field codes
the spec puts after it are not part of the program."
  (let* ((dirs (cons (expand-file-name
                      "applications"
                      (or (getenv "XDG_DATA_HOME") "~/.local/share"))
                     (mapcar (lambda (d) (expand-file-name "applications" d))
                             (split-string
                              (or (getenv "XDG_DATA_DIRS")
                                  "/usr/local/share:/usr/share")
                              ":" t))))
         (file (seq-find #'file-readable-p
                         (mapcar (lambda (d) (expand-file-name desktop d))
                                 dirs))))
    (when file
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^Exec=[[:blank:]]*\\(.+\\)$" nil t)
          (let ((word (car (ignore-errors
                             (split-string-and-unquote (match-string 1))))))
            (and word (not (string-prefix-p "%" word)) word)))))))

(defun maf-desmos--browser-desktop-default ()
  "Return the program the desktop calls its default web browser, or nil.
`xdg-settings' names the browser by .desktop file rather than by
program, so the answer is read out of that entry."
  (when (executable-find "xdg-settings")
    (with-temp-buffer
      (when (eq 0 (ignore-errors
                    (maf-plot--with-work-directory
                      (call-process "xdg-settings" nil t nil
                                    "get" "default-web-browser"))))
        (let ((desktop (string-trim (buffer-string))))
          (unless (string-empty-p desktop)
            (maf-desmos--browser-desktop-exec desktop)))))))

(defun maf-desmos--browser ()
  "Return the program to launch the Desmos URL with, or nil.
`maf-desmos-browser' when set, which settles it. Otherwise the
first that names a program actually present:
`browse-url-generic-program', the BROWSER environment variable, the
desktop's own default browser
\(`maf-desmos--browser-desktop-default'), and failing all three a
probe down `maf-desmos--browser-candidates'.

A generic opener found this way is passed over rather than returned
\(`maf-desmos--browser-openers'): it would open the page and drop
the graph, which is worse than reporting that no browser was found.
An explicit `maf-desmos-browser' is not second-guessed."
  (or maf-desmos-browser
      (seq-find
       (lambda (program)
         (and (stringp program)
              (not (string-empty-p program))
              (not (member (file-name-nondirectory program)
                           maf-desmos--browser-openers))
              (or (and (file-name-absolute-p program)
                       (file-executable-p program))
                  (executable-find program))))
       (append (list browse-url-generic-program
                     (car (split-string (or (getenv "BROWSER") "") ":" t))
                     (maf-desmos--browser-desktop-default))
               maf-desmos--browser-candidates))))

(defun maf-desmos--show (expressions &optional range)
  "Open the Desmos page on EXPRESSIONS in the browser.
RANGE, when given, is the (LO . HI) x bounds the viewport opens on.
The browser is `maf-desmos--browser''s, and the binary gets the
URL directly: `browse-url' via xdg-open resolves a file:// URL to a
bare path and silently drops the fragment, opening an empty
calculator.

The page's calculator is 2D, so a surface over x and y
\(`maf-plot--surface-p') is left out, where it would graph as nothing
or a slider for z; the message counts it, and pointed at the backends
that draw it in 3D, it refuses when nothing else is left to send. A
surface over other unknowns goes over whole: a + b is a line with a
slider for each, as Desmos reads it."
  (unless expressions
    (user-error "Nothing to send to Desmos"))
  (let ((sent nil)
        (surfaces nil))
    (dolist (e expressions)
      (if (and (not (stringp e))
               (equal (maf-plot--surface-p e) '((var x var-x) (var y var-y))))
          (push e surfaces)
        (push e sent)))
    (setq sent (nreverse sent))
    (unless sent
      (user-error "Desmos draws in 2D; g l or g g plots %s in 3D"
                  (if (cdr surfaces)
                      "these surfaces"
                    (maf-plot--label (car surfaces)))))
    (let ((program (maf-desmos--browser)))
      (unless program
        (user-error
         "No browser found: set `maf-desmos-browser' (xdg-open would drop the graph)"))
      (maf-plot--with-work-directory
        (start-process "maf-desmos-browser" nil program
                       (maf-desmos--url sent range)))
      (message "Sent %d %s to Desmos%s" (length sent)
               (if (cdr sent) "expressions" "expression")
               (if surfaces
                   (format "; left out %d %s (g l or g g plots in 3D)"
                           (length surfaces)
                           (if (cdr surfaces) "surfaces" "surface"))
                 "")))))

;;; Command

(defun maf-desmos-plot (arg)
  "Plot the entry at point in the Desmos calculator, in a browser.

  1:  x^2 + y^2 = 4      g o  =>  the circle, in Desmos

Desmos receives the formula itself, relations whole, and scales
interactively — the surface for what gnuplot cannot sample. Point
picks the entry as `maf-plot-embed' does; a vector entry sends one
expression per element. With prefix ARG, prompt for the x bounds the
viewport opens on. The calculator is 2D: a surface in x and y is
left out, for g l or g g to draw in 3D.

With the Hyperbolic flag, every stack entry goes over, one
expression each: H g o."
  (interactive "P")
  (let ((entries (maf-plot--targets)))
    (maf-desmos--show (maf-desmos--expressions entries)
                      (and arg (maf-plot--read-range nil)))))
(put 'maf-desmos-plot 'maf-command t)

;;; Module

(define-minor-mode maf-use-desmos-mode
  "Plot stack entries in the Desmos calculator, on g o in Calc.

g o sends the entry at point to Desmos in a browser, relations and
all; H g o sends every stack entry. Off, g o is calc's again."
  :global t
  :group 'maf
  ;; Module key claims compile in only while the mode is on; the
  ;; toggle is the recompile trigger.
  (maf-bindings--refresh))

(maf-bindings-module-keys 'maf-desmos 'maf-use-desmos-mode
  '(((ergo vim) "g o" maf-desmos-plot)))

(maf-register-module 'maf-desmos #'maf-use-desmos-mode
                     "Plot stack entries in the Desmos calculator.

g o sends the entry at point to Desmos in the browser, relations and
all, where it rescales as you zoom; H g o sends every stack entry. A
prefix argument asks for the x bounds the viewport opens on. The
calculator is 2D, so a surface in x and y stays with g l and g g."
                     "g o (H for the whole stack)" "Plots")

(provide 'maf-desmos)
