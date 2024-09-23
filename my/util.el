;; Utility functions
;;
;; Mostly non-interactive definitions.


;; # Debug

(defun my/flash-mode-line ()
  "Flash the mode line to communicate an effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;; # Commands

(defmacro my/cmd (&rest forms)
  "Wraps some forms in an (interactive) lambda.

This is useful for writing succinct keybindings."
  `(lambda () (interactive) ,@forms))

(defmacro my/cmd-u (&rest forms)
  "Wraps some forms in an (interactive) lambda with the universal
argument (C-u) set.

FORMS can be either a single symbol or set of forms.

This is useful for writing succinct keybindings."
  ;; Check if FORMS is just a symbol.
  (cond ((and (= (length forms) 1)
              (eq (type-of (cadr forms)) 'symbol))
         `(lambda ()
            (interactive)
            (let ((current-prefix-arg t))
              (call-interactively ,@forms))))
        (t `(lambda ()
              (interactive)
              (let ((current-prefix-arg t))
                ,@forms)))))


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

(defmacro my/ignore-error (form)
  "Executes FORM and ignores any errors."
  `(condition-case nil
       ,form
     (error nil)))

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
(pr foo)   -> (print foo)
(pr (foo)) -> (print (foo))
pr)        -> (print ))
pr (foo)   -> (print (foo))
pr foo     -> (print foo)  ;; TODO"
  (cond ((and (looking-back "( *")
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
