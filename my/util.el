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
