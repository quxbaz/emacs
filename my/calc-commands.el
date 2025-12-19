;; Calc commands
;;
;;


;; Entry / Commands

(defun my/calc ()
  "Starts calc."
  (interactive)
  (cond ((and (string= (buffer-name) "*scratch*")
              (= (count-windows) 1))
         (calc nil t t))
        ((= (count-windows) 1)
         (split-window-right)
         (other-window 1)
         (calc nil t t))
        (t
         (call-interactively 'calc))))

(defun my/calc-beginning-of-expression ()
  "Moves point to beginning of expression on current line."
  (interactive)
  (move-beginning-of-line nil)
  (when calc-line-numbering
    (condition-case nil
        (search-forward-regexp "^[0-9]+: *")
      (error nil))))

(defun my/calc-kill-ring-save-dwim ()
  "Saves the region if region is active, else save the current line."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (let* ((line (string-trim (substring-no-properties (thing-at-point 'line))))
           (no-prefix-line (replace-regexp-in-string "^[0-9]+:[[:space:]]*" "" line)))
      (kill-new no-prefix-line)))
  (message "%s" (string-trim (car kill-ring))))

(defun my/calc-edit ()
  "Opens edit mode or edits the current entry."
  (interactive)
  (let ((line (substring-no-properties (thing-at-point 'line))))
    (if (string-match "[0-9]+:" line)
        (progn (funcall (kmacro "j`"))
               (move-end-of-line nil))
      (funcall (kmacro "'`")))))

(defun my/calc-point-gte-last-entry-p ()
  "returns t if point is at the last entry or beyond it."
  (save-excursion
    (condition-case nil
        (progn (next-line 2) (previous-line))
      (error nil))
    (let ((line (string-trim (substring-no-properties (thing-at-point 'line)))))
      (string= line "."))))

(defun my/calc-roll-to-top ()
  "Moves the current entry to the top of the stack."
  (interactive)
  (calc-wrapper
   (let ((n (calc-locate-cursor-element (point))))
     (when (and n (> n 0))
       (calc-roll-up n)))))

(defun my/calc-pop ()
  "Pops an entry from the stack. Shows message if stack is empty."
  (interactive)
  (calc-wrapper
   (if (> (calc-stack-size) 0)
       (calc-pop 1)
     (deactivate-mark)
     (message "Stack is empty."))))

(defun my/calc-ret ()
  "Duplicates the line. If selection is active, exit selection mode instead."
  (interactive)
  (let ((before-state (buffer-substring-no-properties (point-min) (point-max))))
    (my/calc-clear-selections)
    (let ((after-state (buffer-substring-no-properties (point-min) (point-max))))
      (when (string= before-state after-state)
        ;; No selection was active, duplicate the line.
        (my/calc-duplicate)))))

(defun my/calc-duplicate ()
  "Duplicates the nearest entry at point. If region is active, duplicate
just the region."
  (interactive)
  (cond ((use-region-p)
         (calc-wrapper
          (calc-push (math-read-expr (buffer-substring-no-properties (region-beginning) (region-end)))))
         (setf (point) (- (point-max) 2)))
        ((my/calc-point-gte-last-entry-p)
         (call-interactively 'calc-enter))
        (t
         (let* ((line (string-trim (substring-no-properties (thing-at-point 'line))))
                (no-prefix-line (replace-regexp-in-string "^[0-9]+:[[:space:]]*" "" line)))
           (calc-wrapper
            (calc-push (math-read-expr no-prefix-line)))
           (setf (point) (- (point-max) 2))))))

(defun my/calc-duplicate-no-move ()
  "Like my/calc-duplicate, but doesn't move point."
  (interactive)
  (let ((n (calc-locate-cursor-element (point)))
        (col (current-column)))
    (call-interactively 'my/calc-duplicate)
    (when (and n (> n 0))
      (calc-cursor-stack-index (1+ n))
      (move-to-column col))))

(defun my/calc-edit-duplicate (arg)
  "Duplicates the current line. Adds a comma if necessary."
  (interactive "p")
  ;; Add comma to current line if it doesn't have one.
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t")
    (unless (looking-back "," (line-beginning-position))
      (insert ",")))
  ;; Duplicate the line.
  (call-interactively 'my/duplicate-dwim)
  ;; Remove comma from the duplicated line.
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t")
    (when (looking-back "," (line-beginning-position))
      (delete-char -1))))

(defun my/calc-evaluate ()
  "Like calc-evaluate, but turns off symbolic mode during evaluation, then restores."
  (interactive)
  (let ((stored-symbolic-mode calc-symbolic-mode))
    (unwind-protect
        (progn (calc-symbolic-mode -1)
               (call-interactively 'calc-evaluate))
      (if stored-symbolic-mode
          (calc-symbolic-mode 1)
        (calc-symbolic-mode -1)))))

(defun my/calc-square ()
  "Squares a number."
  (interactive)
  (funcall (kmacro "I Q")))

(defun my/calc-pi ()
  "Multiplies by pi, or enters pi if the stack is empty."
  (interactive)
  (calc-wrapper
   (if (= (calc-stack-size) 0)
       (calc-push '(var pi var-pi))
     (calc-enter-result 1 "pi*" (math-mul (calc-top-n 1) '(var pi var-pi))))))

(defun my/calc-quick-variable ()
  "Reads a character and pushes it as a variable onto the calc stack."
  (interactive)
  (let ((char (read-char-from-minibuffer "Enter a variable: ")))
    (if (or (and (>= char ?a) (<= char ?z))
            (and (>= char ?A) (<= char ?Z)))
        (let* ((var-name (intern (char-to-string char)))
               (var-symbol (intern (concat "var-" (char-to-string char)))))
          (calc-wrapper
           (calc-push (list 'var var-name var-symbol))))
      (message "Invalid character. Must be a-z or A-Z."))))

(defun my/calc-toggle-big-language ()
  "Toggle between big language and normal display mode."
  (interactive)
  (calc-wrapper
   (if (eq calc-language 'big)
       (calc-normal-language)
     (calc-big-language))))

(defun my/calc-edit-square-dwim ()
  "Inserts ^2. Subsequent invocations increment the exponent value."
  (interactive)
  (if (looking-back "\\^[0-9]+" (line-beginning-position))
      ;; We're right after an exponent, increment it
      (let* ((end (point))
             (start (save-excursion
                      (skip-chars-backward "0-9")
                      (point)))
             (exponent (string-to-number (buffer-substring-no-properties start end))))
        (delete-region start end)
        (insert (number-to-string (1+ exponent))))
    ;; No exponent before point, insert ^2
    (insert "^2")))

(defun my/calc-edit-cube-dwim ()
  "Inserts ^3. Subsequent invocations increment the exponent value."
  (interactive)
  (if (looking-back "\\^[0-9]+" (line-beginning-position))
      ;; We're right after an exponent, increment it
      (let* ((end (point))
             (start (save-excursion
                      (skip-chars-backward "0-9")
                      (point)))
             (exponent (string-to-number (buffer-substring-no-properties start end))))
        (delete-region start end)
        (insert (number-to-string (1+ exponent))))
    ;; No exponent before point, insert ^3
    (insert "^3")))

(defun my/calc-edit-sqrt-dwim ()
  "Applies square root to the preceeding expression or to the expression in region.
Treats / as a separator (only applies sqrt after /), but keeps x:y together."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (expr (buffer-substring-no-properties start end)))
        (delete-region start end)
        (insert (format "sqrt(%s)" expr))
        (deactivate-mark))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "a-zA-Z0-9:._")
                    (point)))
           (expr (buffer-substring-no-properties start end)))
      (if (> (length expr) 0)
          (progn
            (delete-region start end)
            (insert (format "sqrt(%s)" expr)))
        (insert "sqrt()")
        (backward-char)))))

(defun my/calc-edit-ln ()
  "Applies natural log to the preceeding expression or to the expression in region.
Treats / as a separator (only applies sqrt after /), but keeps x:y together."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (expr (buffer-substring-no-properties start end)))
        (delete-region start end)
        (insert (format "ln(%s)" expr))
        (deactivate-mark))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "a-zA-Z0-9:._")
                    (point)))
           (expr (buffer-substring-no-properties start end)))
      (if (> (length expr) 0)
          (progn
            (delete-region start end)
            (insert (format "ln(%s)" expr)))
        (insert "ln()")
        (backward-char)))))

(defun my/calc-vector-edit ()
  "Begins a vector entry."
  (interactive)
  (if (minibufferp)
      (call-interactively 'self-insert-command)
    (funcall (kmacro "'`"))
    (insert "[]") (backward-char)))


;; Edit Mode

(defun my/calc-edit-history-prev ()
  "Recall previous calc history entry."
  (interactive)
  (when calc-alg-entry-history
    (let ((history-length (length calc-alg-entry-history)))
      (setq my/calc-history-index (min (1+ my/calc-history-index) (1- history-length)))
      (goto-char (point-min)) (forward-line 2) (delete-region (point) (point-max))
      (insert (nth my/calc-history-index calc-alg-entry-history)))))

(defun my/calc-edit-history-next ()
  "Recall next calc history entry."
  (interactive)
  (when calc-alg-entry-history
    (goto-char (point-min)) (forward-line 2) (delete-region (point) (point-max))
    (when (> my/calc-history-index 0)
      (setq my/calc-history-index (1- my/calc-history-index))
      (insert (nth my/calc-history-index calc-alg-entry-history)))))

(defun my/calc-edit-finish ()
  "Like calc-edit-finish, but pushes to calc-alg-entry-history."
  (interactive)
  (save-excursion
    (goto-char (point-min)) (forward-line 2) (beginning-of-line)
    (let ((text (string-trim (buffer-substring-no-properties (point) (point-max)))))
      (cond ((string= text "") nil)    ;; Don't save empty strings to history.
            ((string= text "[]") (delete-region (line-beginning-position) (1+ (line-end-position))))
            ((string= text (cadr calc-alg-entry-history)) nil)  ;; Don't save string to history if it's a duplicate of the previous entry.
            (t (push text calc-alg-entry-history)))))
  (calc-edit-finish))

(defun my/calc-edit-newline ()
  "Like newline, but also sets indentation."
  (interactive)
  (newline)
  (if (string= (string-trim (thing-at-point 'line)) "]")
      (delete-horizontal-space)))

(defun my/calc-toggle-brackets ()
  "Toggle between parentheses and square brackets at point."
  (interactive)
  (cond ((looking-at "(")
         (delete-char 1) (insert "[") (backward-char 1))
        ((looking-at ")")
         (delete-char 1) (insert "]") (backward-char 1))
        ((looking-at "\\[")
         (delete-char 1) (insert "(") (backward-char 1))
        ((looking-at "\\]")
         (delete-char 1) (insert ")") (backward-char 1))))


;; Expression Manipulation

(defun my/calc-clear-selections ()
  "Like calc-clear-selections, but retains point position."
  (interactive)
  (let ((saved-point (point)))
    (unwind-protect
        (calc-clear-selections)
      (setf (point) saved-point))))

(defun my/calc-commute ()
  "Like calc-sel-commute, but works on the line instead of the current selection."
  (interactive)
  (let ((saved-point (point)))
    (unwind-protect
        (progn
          (move-end-of-line nil)
          (call-interactively 'calc-sel-commute))
      (setf (point) saved-point))))

(defun my/calc-sel-jump-equals ()
  "Like calc-sel-jump-equals, but unselects after jumping."
  (interactive)
  (let ((saved/calc-show-selections calc-show-selections))
    (unwind-protect
        (progn (calc-show-selections -1)
               (call-interactively 'calc-sel-jump-equals)
               (beginning-of-line)
               (search-forward-regexp "#")
               (backward-char))
      (call-interactively 'calc-unselect)
      (if (eq saved/calc-show-selections t)
          (calc-show-selections t)))))

(defun my/calc-sel-negate ()
  "Like calc-sel-negate, but unselects afterwards."
  (interactive)
  (let ((saved-point (point)))
    (unwind-protect
        (call-interactively 'calc-sel-negate)
      (call-interactively 'calc-unselect)
      (setf (point) saved-point))))


;; Rewrite Rules

(defun my/calc-log-power-rule ()
  "Applies logarithm power rule."
  (interactive)
  (let ((rules (list "ln(x^p) := p * ln(x)"
                     "log(x^p, b) := p * log(x, b)")))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

(defun my/calc-to-degrees ()
  "Converts radian value to degree."
  (interactive)
  (let ((rules (list "r := r * 180 / pi")))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

(defun my/calc-to-radians ()
  "Converts degree value to radian as a factor of pi."
  (interactive)
  (let ((rules (list "d := d * pi / 180")))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

(defun my/calc-mod-360 ()
  "Applies modulo 360 (degrees)."
  (interactive)
  (calc-wrapper
   (calc-enter-result 1 "norm" (math-mod (calc-top-n 1) 360))))

(defun my/calc-mod-360-from-minibuffer ()
  "Applies modulo 360 (degrees) from minibuffer."
  (interactive)
  (let ((input (math-read-number (minibuffer-contents))))
    (delete-minibuffer-contents)
    (calc-wrapper
     (calc-push input)
     (calc-enter-result 1 "norm" (math-mod input 360)))
    (exit-minibuffer)))

(defun my/calc-complete-the-square ()
  "Completes the square for quadratic expressions and equations.

Handles forms like:
    x^2 + bx          ->  (x + b/2)^2 - (b/2)^2
    ax^2 + bx         ->  a*((x + b/2a)^2 - (b/2a)^2)
    ax^2 + bx = y     ->  (x + b/2a)^2 - (b/2a)^2 = y/a"
  (interactive)
  (let ((rules (list
                ;; x^2 ± x
                "x^2 + x := (x + 1/2)^2 - (1/4) :: variable(x)"
                "x^2 - x := (x - 1/2)^2 - (1/4) :: variable(x)"
                ;; x^2 ± bx
                "x^2 + b*x := (x + b/2)^2 - (b/2)^2 :: variable(x)"
                "x^2 - b*x := (x - b/2)^2 - (b/2)^2 :: variable(x)"
                ;; ax^2 ± x = y
                "a*x^2 + x = y := (x + 1/2a)^2 - (1/2a)^2 = y/a :: variable(x)"
                "a*x^2 - x = y := (x - 1/2a)^2 - (1/2a)^2 = y/a :: variable(x)"
                ;; ax^2 ± bx = y
                "a*x^2 + b*x = y := (x + b/2a)^2 - (b/2a)^2 = y/a :: variable(x)"
                "a*x^2 - b*x = y := (x - b/2a)^2 - (b/2a)^2 = y/a :: variable(x)"
                ;; ax^2 ± x (factored)
                "a*x^2 + x := a * ((x + 1/2*a)^2 - (1/2*a)^2) :: variable(x)"
                "a*x^2 - x := a * ((x - 1/2*a)^2 - (1/2*a)^2) :: variable(x)"
                ;; ax^2 ± bx (factored)
                "a*x^2 + b*x := a * ((x + b/2*a)^2 - (b/2*a)^2) :: variable(x)"
                "a*x^2 - b*x := a * ((x - b/2*a)^2 - (b/2*a)^2) :: variable(x)"
                )))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

(defun my/calc-factor-powers ()
  "Factors expressions as sums/differences of squares and cubes.

Handles forms like:
    a^2 - b^2         ->  (a + b)(a - b)
    a^2 + b^2         ->  (b + a*i)(b - a*i)
    a^3 + b^3         ->  (a + b)(a^2 - ab + b^2)
    a^3 - b^3         ->  (a - b)(a^2 + ab + b^2)

Uses complex numbers for sums of squares, cube roots for non-cube terms,
and handles coefficients."
  (interactive)
  (let ((rules (list
                ;; Squares: a^2 - b^2
                "a^2 - b^2 := (a + b)(a - b)"
                ;; Squares: a^2 + b^2 (complex)
                "a^2 + b^2 := (b + a*i)(b - a*i)"
                ;; Squares: a^2 - b
                "a^2 - b := (a + sqrt(b))(a - sqrt(b))"
                ;; Squares: a^2 + b (complex)
                "a^2 + b := (a + sqrt(b)*i)(a - sqrt(b)*i)"
                ;; Squares: c*a^2 - b
                "c * a^2 - b := c * (a + sqrt(b/c))(a - sqrt(b/c))"
                ;; Squares: c*a^2 + b (complex)
                "c * a^2 + b := c * (a + sqrt(b/c)*i)(a - sqrt(b/c)*i)"
                ;; Squares: b - a^2 (reverse order)
                "b - a^2 := (sqrt(b) + a)(sqrt(b) - a)"
                ;; Squares: b - c*a^2 (reverse order with coefficient)
                "b - c*a^2 := (sqrt(b) + sqrt(c)*a)(sqrt(b) - sqrt(c)*a)"
                ;; Cubes: a^3 + b^3
                "plain(a^3 + b^3) := (a + b)(a^2 - a*b + b^2)"
                ;; Cubes: a^3 - b^3
                "plain(a^3 - b^3) := (a - b)(a^2 + a*b + b^2)"
                ;; Cubes: a^3 + b (with cube root)
                "plain(a^3 + b) := (a + b^1:3)(a^2 - a*b^1:3 + b^2:3)"
                ;; Cubes: a^3 - b (with cube root)
                "plain(a^3 - b) := (a - b^1:3)(a^2 + a*b^1:3 + b^2:3)"
                ;; Cubes: c*a^3 + b^3
                "plain(c*a^3 + b^3) := ((c^1:3)*a + b)((c^2:3)*a^2 - (c^1:3)*a*b + b^2)"
                ;; Cubes: c*a^3 - b^3
                "plain(c*a^3 - b^3) := ((c^1:3)*a - b)((c^2:3)*a^2 + (c^1:3)*a*b + b^2)"
                ;; Cubes: c*a^3 + d (both with cube roots)
                "plain(c*a^3 + d) := ((c^1:3)*a + d^1:3)((c^2:3)*a^2 - (c^1:3)*(d^1:3)*a + d^2:3)"
                ;; Cubes: c*a^3 - d (both with cube roots)
                "plain(c*a^3 - d) := ((c^1:3)*a - d^1:3)((c^2:3)*a^2 + (c^1:3)*(d^1:3)*a + d^2:3)"
                ;; Higher powers: a^n as difference of squares
                "plain(a^n - b) := ((a^(n/2)) + sqrt(b))((a^(n/2)) - sqrt(b)) :: n > 3"
                "plain(a^n + b) := ((a^(n/2)) + sqrt(b)*i)((a^(n/2)) - sqrt(b)*i) :: n > 3"
                ;; Higher powers: c*a^m - b^n as difference of squares
                "plain(c*a^m - b^n) := (sqrt(c)*a^(m/2) + b^(m/2))(sqrt(c)*a^(m/2) - b^(m/2)) :: m > 3"
                ;; Higher powers: c*a^m - d*b^n as difference of squares
                "plain(c*a^m - d*b^n) := (sqrt(c)*a^(m/2) + sqrt(d)*b^(n/2))(sqrt(c)*a^(m/2) - sqrt(d)*b^(n/2)) :: m > 3"
                )))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))
