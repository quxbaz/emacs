;; -*- lexical-binding: t; -*-
;;
;; Keybinding utilities
;;
;; Helpers shared by the keybinding files.  Each binding block assigns
;; keybindings; the blocks differ only in *when* the assignment must happen.
;; These three primitives separate the two concerns:
;;
;;   my/apply-bindings  -- the data: install a list of bindings into a keymap.
;;   my/apply-when      -- the timing: invoke a thunk now, after a feature
;;                         loads, or from a mode hook.  The strategy is an
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

BINDINGS is a flat list of KEY DEFINITION pairs.  Each DEFINITION is
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
