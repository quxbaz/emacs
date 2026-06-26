;; -*- lexical-binding: t -*-
;;
;; Utility functions
;;
;; Mostly non-interactive definitions.


;; # Debug

(defun my/flash-mode-line ()
  "Flash the mode line to communicate an effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(defun my/flash-region (start end)
  "Temporarily highlight region from START to END."
  (require 'pulse)
  (let ((pulse-iterations 15)
        (pulse-delay 0.03))
    (pulse-momentary-highlight-region start end)))


;; # Commands

(defmacro my/cmd (&rest forms)
  "Wraps some forms in an (interactive) lambda.

This is useful for writing succinct keybindings."
  `(lambda () (interactive) ,@forms))

(defun my/cmd-or (default-cmd prefix-cmd)
  "Return a command that calls PREFIX-CMD with prefix arg, DEFAULT-CMD otherwise."
  (lambda (arg)
    (interactive "P")
    (if arg
        (call-interactively prefix-cmd)
      (call-interactively default-cmd))))

(defun my/with-prefix (cmd &optional prefix)
  "Return a command that calls CMD as if invoked with C-u.
PREFIX overrides the prefix arg (defaults to '(4))."
  (lambda ()
    (interactive)
    (let ((current-prefix-arg (or prefix '(4))))
      (call-interactively cmd))))


;; # Keybindings
;;
;; Every keybinding block assigns bindings; the blocks differ only in *when* the
;; assignment must happen. These three primitives separate the two concerns:
;;
;;   my/apply-bindings  -- the data: install a list of bindings into a keymap.
;;   my/apply-when      -- the timing: invoke a thunk now, after a feature
;;                         loads, or from a mode hook. The strategy is an
;;                         argument, so the call site reads the same regardless.
;;   my/bind            -- sugar over the two for the common single-map case.

(defun my/apply-bindings (keymap bindings)
  "Assign BINDINGS in KEYMAP.
BINDINGS is a list of (KEY . DEFINITION) cons cells, where KEY is a
`keymap-set' key description and DEFINITION is its command."
  (dolist (binding bindings)
    (keymap-set keymap (car binding) (cdr binding))))

(defun my/apply-when (when thunk)
  "Call THUNK according to the timing strategy WHEN.
WHEN is one of:
  :now              call THUNK immediately
  (:after FEATURE)  call THUNK after FEATURE is loaded
  (:hook HOOK)      call THUNK each time HOOK runs"
  (pcase when
    (:now (funcall thunk))
    (`(:after ,feature) (with-eval-after-load feature (funcall thunk)))
    (`(:hook ,hook) (add-hook hook thunk))
    (_ (error "my/apply-when: unknown timing strategy %S" when))))

(defmacro my/bind (when keymap &rest bindings)
  "Assign keybindings to KEYMAP, installed according to WHEN.

WHEN is passed to `my/apply-when' and selects when/how the bindings
take effect:
  :now              install now (KEYMAP must already exist)
  (:after FEATURE)  install after FEATURE is loaded
  (:hook HOOK)      install every time HOOK runs

KEYMAP is a keymap variable; it is resolved at install time, so maps
that don't exist until their package loads work with `:after' and
`:hook'.

BINDINGS is a flat list of KEY DEFINITION pairs. Each DEFINITION is
evaluated at install time, so it may be a quoted command symbol or a
form that returns a command (e.g. `my/with-prefix')."
  (declare (indent 2))
  (let (pairs)
    (while bindings
      (let ((key (pop bindings))
            (def (pop bindings)))
        (push `(cons ,key ,def) pairs)))
    `(my/apply-when ',when
                    (lambda ()
                      (my/apply-bindings ,keymap (list ,@(nreverse pairs)))))))

(defmacro my/setup (name &rest clauses)
  "Configure a single mode in one form. NAME is a documentation label.

CLAUSES is a sequence of sections, each introduced by one of these
keywords and running until the next section keyword:

  :init      FORM...           forms evaluated immediately (settings,
                               advice, faces, `define-minor-mode', ...)
  :after     FEATURE FORM...   FORMs wrapped in (`with-eval-after-load'
                               FEATURE ...)
  :hooks     (HOOK FN)...       each pair added with `add-hook'
  :bindings  ARG...            ARGs passed verbatim to `my/bind', i.e.
                               WHEN KEYMAP KEY DEFINITION...

Only these keywords act as section delimiters, so keywords nested in
forms (`:override', `:lighter') and `my/bind's own `:now' pass through.
Sections are emitted in source order, so ordering dependencies -- e.g. a
`define-minor-mode' before the hook that uses it -- are preserved."
  (declare (indent 1))
  (ignore name)
  (let ((known '(:init :after :hooks :bindings))
        (sections '())
        (current nil)
        (items '()))
    (dolist (elt clauses)
      (if (and (keywordp elt) (memq elt known))
          (progn
            (when current (push (cons current (nreverse items)) sections))
            (setq current elt items nil))
        (push elt items)))
    (when current (push (cons current (nreverse items)) sections))
    `(progn
       ,@(mapcar
          (lambda (sec)
            (pcase (car sec)
              (:init `(progn ,@(cdr sec)))
              (:after `(with-eval-after-load ',(cadr sec) ,@(cddr sec)))
              (:hooks `(progn ,@(mapcar (lambda (h) `(add-hook ',(car h) ,(cadr h)))
                                        (cdr sec))))
              (:bindings `(my/bind ,@(cdr sec)))))
          (nreverse sections)))))


;; # Util

(defun my/is-inside-comment ()
  "Returns t if point is inside a comment."
  (if (nth 4 (syntax-ppss)) t))

(defun my/is-image-p (filename)
  "Returns t if FILENAME is an image type.

FILENAME is a path to an actual image file."
  (let ((extension (file-name-extension filename)))
    (and extension
         (file-regular-p filename)
         (or (image-type-available-p (intern extension))
             (string= extension "jpg")))))

;; Macros

(defmacro my/preserve-point (&rest forms)
  "Execute FORMS and restore point to its original position afterwards, even
if FORMS signals an error. Returns the value of the last form in FORMS."
  `(let ((point (point)))
     (prog1
         (progn ,@forms)
       (goto-char point))))

(defmacro my/if-buffer-changed (body then &optional else)
  "Executes BODY. If the execution of BODY causes any change in the buffer,
execute THEN. Otherwise execute ELSE."
  (declare (indent 1))
  `(let* ((get-buffer-content (lambda () (buffer-substring-no-properties (point-min) (point-max))))
          (buffer-before (funcall get-buffer-content)))
     ,body
     (if (string= (funcall get-buffer-content) buffer-before)
         ,else
       ,then)))


;; Snippet

;; Could probably clean this up some.
(defun my/snippet-insert-or-wrap (symbol)
  "Expands a snippet with the best intentions that either, depending on context,
inserts itself, or inserts itself AND wraps the proceeeding form.

EMPTY      -> (print )
(pr)       -> (print )
(pr foo)   -> (print foo)
(pr (foo)) -> (print (foo))
pr)        -> (print ))
pr (foo)   -> (print (foo))
pr foo     -> (print foo)  ;; TODO"
  (cond ((and (looking-back "( *")
              (looking-at " *)"))
         (insert (format "%s " symbol)))
        ((and (looking-back "( *")
              (looking-at " *("))
         (insert (format "%s " symbol))
         (delete-horizontal-space)
         (insert " ")
         (backward-char))
        ((looking-at " *(")
         (insert (format "(%s)" symbol))
         (backward-char)
         (paredit-forward-slurp-sexp)
         (delete-horizontal-space)
         (insert " "))
        ((or (and (or (= (point) (line-beginning-position))
                      (looking-back "[[:blank:]]"))
                  (looking-at "[[:blank:]\n]"))
             (and (looking-back "[[:blank:]]*")
                  (looking-at " *)")))
         (insert (format "(%s )" symbol))
         (backward-char))
        (t
         (insert (format "%s " symbol))
         (delete-horizontal-space)
         (insert " ")
         (backward-char))))
