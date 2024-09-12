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


;; # Util

(defun my/is-inside-comment ()
  "Returns t if point is inside a comment."
  (if (nth 4 (syntax-ppss)) t))


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
