;; -*- lexical-binding: t; -*-
;;
;; Core calc stack operations

(require 'my/calc/lib)

(defvar my/calc-stack-save-file
  (expand-file-name "calc-stack.el" user-emacs-directory))

(defvar my/calc-stack-restored nil)

(defun my/calc-save-stack ()
  "Save the calc stack to disk."
  (when-let ((buf (get-buffer "*Calculator*")))
    (with-current-buffer buf
      (let ((items (when (> (calc-stack-size) 0)
                     (mapcar #'car (cdr calc-stack)))))
        (with-temp-file my/calc-stack-save-file
          (prin1 items (current-buffer)))))))

(defun my/calc-restore-stack ()
  "Restore the calc stack from disk, once per session."
  (unless my/calc-stack-restored
    (setq my/calc-stack-restored t)
    (when (file-exists-p my/calc-stack-save-file)
      (let ((stack (with-temp-buffer
                     (insert-file-contents my/calc-stack-save-file)
                     (read (current-buffer)))))
        (let ((clean (delq nil stack)))
          (when clean
            (calc-push-list (reverse clean))))))))

(add-hook 'kill-emacs-hook  #'my/calc-save-stack)
(add-hook 'calc-mode-hook   #'my/calc-restore-stack)

(defun my/calc ()
  "Starts calc."
  (interactive)
  (cond ((or (string-match-p "^[*]" (buffer-name))
             (memq major-mode (list 'dired-mode 'magit-status-mode)))
         (calc nil t t))
        ((= (count-windows) 1)
         (split-window-right)
         (other-window 1)
         (calc nil t t))
        (t
         (call-interactively 'calc))))

(defun my/calc-no-simplify-mode ()
  "Like calc-no-simplify-mode, but retains point."
  (interactive)
  (my/preserve-point
   (call-interactively 'calc-no-simplify-mode)
   (my/flash-mode-line)))

(defun my/calc-next-cmd-no-simplify ()
  "Run the next command with simplification disabled for that command only."
  (interactive)
  (let ((key (read-key-sequence "No-simplify:")))
    (my/calc-without-simplification
      (call-interactively (key-binding key)))))

(defun my/calc-toggle-big-language ()
  "Toggle between big language and normal display mode."
  (interactive)
  (calc-wrapper
   (if (eq calc-language 'big)
       (calc-normal-language)
     (calc-big-language))))

(defun my/calc-reset (arg)
  "Like calc-reset, but also clears the trail."
  (interactive "P")
  (call-interactively 'calc-reset)
  (with-current-buffer (calc-trail-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (my/flash-mode-line))

(defun my/calc-reset-settings ()
  "Reset calc display settings and modes without clearing the stack."
  (interactive)
  (calc-reset 1)
  (my/flash-mode-line))

(defun my/calc-undo ()
  "Like calc-undo, but retains point position."
  (interactive)
  (my/preserve-point (call-interactively 'calc-undo)))

(defun my/calc-redo ()
  "Like calc-redo, but retains point position."
  (interactive)
  (my/preserve-point (call-interactively 'calc-redo)))

(defun my/calc-kill ()
  "Like calc-kill, but retains point position."
  (interactive)
  (my/preserve-point (call-interactively 'calc-kill)))

(defun my/calc-beginning-of-expression ()
  "Moves point to beginning of expression on current line.
If point is past the top stack item, calls calc-realign instead."
  (interactive)
  (if (<= (calc-locate-cursor-element (point)) 0)
      (calc-realign)
    (move-beginning-of-line nil)
    (when calc-line-numbering
      (condition-case nil
          (search-forward-regexp "^[0-9]+: *")
        (error nil)))))

(defun my/calc-forward-noun ()
  "Move point to the start of the next number or variable in the calc display.
Function names (identifiers immediately followed by '(') are skipped."
  (interactive)
  (when (looking-at "[a-zA-Z_][a-zA-Z0-9_]*\\|[0-9]+\\(\\.[0-9]+\\)?")
    (goto-char (match-end 0)))
  (let (done)
    (while (and (not done)
                (re-search-forward "[a-zA-Z_][a-zA-Z0-9_]*\\|[0-9]+\\(\\.[0-9]+\\)?" nil t))
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (unless (and (string-match-p "[a-zA-Z_]" (string (char-after beg)))
                     (eq (char-after end) ?\())
          (goto-char beg)
          (setq done t))))))

(defun my/calc-backward-noun ()
  "Move point to the start of the previous number or variable in the calc display.
Function names (identifiers immediately followed by '(') are skipped."
  (interactive)
  (skip-chars-backward "0-9")
  (when (and (> (point) (point-min)) (char-equal (char-before) ?.))
    (backward-char)
    (skip-chars-backward "0-9"))
  (skip-chars-backward "a-zA-Z_")
  (let (done)
    (while (and (not done)
                (re-search-backward "[a-zA-Z_][a-zA-Z0-9_]*\\|[0-9]+\\(\\.[0-9]+\\)?" nil t))
      (goto-char (match-beginning 0))
      ;; re-search-backward finds the rightmost-starting match, which may be a
      ;; suffix of the token (e.g. "t" instead of "sqrt"). Back up to the real start.
      (skip-chars-backward "a-zA-Z_0-9")
      (when (and (> (point) (point-min)) (char-equal (char-before) ?.))
        (backward-char)
        (skip-chars-backward "0-9"))
      (unless (looking-at "[a-zA-Z_][a-zA-Z0-9_]*(")
        (setq done t)))))


;; log(x, b) → \log_{b}\left( x \right) in LaTeX/MathJax output.
(with-eval-after-load 'calccomp
  (put 'calcFunc-log 'math-compose-latex
       (lambda (a _prec)
         (if (= (length a) 3)
             (list 'horiz
                   "\\log_{" (math-compose-expr (nth 2 a) 0) "}"
                   "\\left( " (math-compose-expr (nth 1 a) 0) " \\right)")
           (list 'horiz
                 "\\ln\\left( " (math-compose-expr (nth 1 a) 0) " \\right)")))))

(defun my/calc-kill-ring-save-dwim ()
  "Saves the region if region is active, else save the current line.
If point is at or past home with no active region, saves the top stack item.
Invoked twice in a row, saves as LaTeX/MathJax form instead."
  (interactive)
  (let ((latex-p (eq last-command 'my/calc-kill-ring-save-dwim))
        (at-home (and (not (use-region-p))
                      (<= (calc-locate-cursor-element (point)) 0))))
    (if latex-p
        (let* ((expr (if (use-region-p)
                         (math-read-expr (buffer-substring-no-properties (region-beginning) (region-end)))
                       (let* ((line (if at-home
                                        (save-excursion
                                          (calc-cursor-stack-index 1)
                                          (substring-no-properties (thing-at-point 'line)))
                                      (substring-no-properties (thing-at-point 'line))))
                              (text (replace-regexp-in-string "^[0-9]+:[[:space:]]*" "" (string-trim line))))
                         (math-read-expr text))))
               (latex (let ((save-lang calc-language)
                            (save-opt  calc-language-option))
                        (unwind-protect
                            (progn
                              (calc-set-language 'latex nil t)
                              (math-composition-to-string
                               (math-compose-expr expr 0)
                               (frame-width)))
                          (calc-set-language save-lang save-opt t)))))
          (kill-new latex)
          (message "%s" latex))
      (if (use-region-p)
          (call-interactively 'kill-ring-save)
        (let* ((line (if at-home
                         (save-excursion
                           (calc-cursor-stack-index 1)
                           (substring-no-properties (thing-at-point 'line)))
                       (substring-no-properties (thing-at-point 'line))))
               (text (replace-regexp-in-string "^[0-9]+:[[:space:]]*" ""
                       (string-trim line))))
          (kill-new text)))
      (message "%s" (string-trim (car kill-ring))))))

(defun my/calc-roll-to-top ()
  "Moves the current entry to the top of the stack."
  (interactive)
  (calc-wrapper
   (let ((n (calc-locate-cursor-element (point))))
     (when (and n (> n 0))
       (calc-roll-up n)))))

(defun my/calc-roll-to-bottom ()
  "Moves the stack item at point to the bottom of the stack."
  (interactive)
  (calc-wrapper
   (let ((m (calc-locate-cursor-element (point)))
         (n (calc-stack-size)))
     (let ((m (if (and m (> m 0)) m 1)))
       (when (> n 1)
         (calc-roll-up m)
         (calc-roll-down n))))))

(defun my/calc-pop ()
  "Delete subformula at point, or pop top of stack if cursor is at bottom.
Shows message if stack is empty."
  (interactive)
  (cond ((and (> (calc-stack-size) 0)
              (<= (calc-locate-cursor-element (point)) 0))
         (calc-pop 1))
        ((> (calc-stack-size) 0)
         (calc-del-selection))
        (t
         (deactivate-mark)
         (calc-align-stack-window)
         (message "Stack is empty."))))

(defun my/calc-ret ()
  "Duplicate the expression at point and move to home.
If selection is active, clear selections instead.
- At home: duplicate the top stack item.
- At end of line or before the expression (line-number prefix): push the entire entry.
- On a subexpression: push the sub-formula under point."
  (interactive)
  (cond
   ((my/calc-active-selection-p)
    (my/calc-clear-selections))
   ((my/calc-point-is-at-home-p)
    (calc-enter 1))
   (t
    (let* ((m (calc-locate-cursor-element (point)))
           (entry (nth m calc-stack))
           (subexpr (and (not (eolp)) (my/calc-subformula-at-point)))
           (expr (or subexpr (car entry))))
      (push-mark)
      (calc-wrapper
       (calc-push expr))
      (calc-align-stack-window)))))

(defun my/calc-duplicate ()
  "Duplicates the nearest entry at point. If region is active, duplicate
just the region."
  (interactive)
  (cond ((use-region-p)
         (calc-wrapper
          (calc-push (math-read-expr (buffer-substring-no-properties (region-beginning) (region-end)))))
         (goto-char (- (point-max) 2)))
        ((my/calc-at-stack-bottom-p)
         (call-interactively 'calc-enter))
        (t
         (let* ((line (string-trim (substring-no-properties (thing-at-point 'line))))
                (no-prefix-line (replace-regexp-in-string "^[0-9]+:[[:space:]]*" "" line)))
           (calc-wrapper
            (calc-push (math-read-expr no-prefix-line)))
           (goto-char (- (point-max) 2))))))

(defun my/calc-duplicate-no-move ()
  "Like my/calc-duplicate, but doesn't move point."
  (interactive)
  (let ((n (calc-locate-cursor-element (point)))
        (col (current-column)))
    (call-interactively 'my/calc-duplicate)
    (when (and n (> n 0))
      (calc-cursor-stack-index (1+ n))
      (move-to-column col))))

(defun my/calc-coordinate-toggle ()
  "Cycle coordinate forms: [1 2] -> [x=1 y=2] -> [h=1 k=2] -> [p=1 q=2] -> [x=1 y=2] -> ...
Also converts f(2) = 0 to [2 0]."
  (interactive)
  (let* ((xyzw '((var x var-x) (var y var-y) (var z var-z) (var w var-w)))
         (hklm '((var h var-h) (var k var-k) (var l var-l) (var m var-m)))
         (pq   '((var p var-p) (var q var-q) (var r var-r) (var s var-s)))
         (sets (list xyzw hklm pq)))
    (calc-wrapper
     (let* ((expr (calc-top-n 1))
            (items (and (eq (car-safe expr) 'vec) (cdr expr))))
       (cond
        ;; f(x) = y → [x y]
        ((and (eq (car-safe expr) 'calcFunc-eq)
              (= (length (nth 1 expr)) 2))
         (calc-enter-result 1 "crd"
           (list 'vec (nth 1 (nth 1 expr)) (nth 2 expr))))
        ;; [x=1 y=2] → next named set in cycle
        ((and items (cl-every (lambda (i) (eq (car-safe i) 'calcFunc-eq)) items))
         (let* ((first-var (nth 1 (car items)))
                (current   (cl-find-if (lambda (s) (member first-var s)) sets))
                (to        (or (cadr (member current sets)) (car sets))))
           (calc-enter-result 1 "crd"
             (cons 'vec
               (cl-mapcar (lambda (eq to-var)
                            (list 'calcFunc-eq to-var (nth 2 eq)))
                          items to)))))
        ;; [1 2] → [x=1 y=2]
        (items
         (calc-enter-result 1 "crd"
           (cons 'vec
             (cl-mapcar (lambda (var val) (list 'calcFunc-eq var val))
                        xyzw items)))))))))

(defun my/calc-duplicate-stack ()
  "Duplicates the entire calc stack."
  (interactive)
  (calc-wrapper
   (calc-push-list (reverse (mapcar #'car (cdr calc-stack))))))

(defun my/calc-store-stack ()
  "Stores the current calc stack in memory."
  (interactive)
  (setq my/calc-stored-stack (cdr calc-stack))
  (message "Stored calc stack.")
  (my/flash-mode-line))

(defun my/calc-recall-stack ()
  "Recalls the stored calc stack."
  (interactive)
  (if my/calc-stored-stack
      (progn
        (calc-wrapper
         (calc-push-list (reverse (mapcar #'car my/calc-stored-stack))))
        (message "Recalled calc stack."))
    (message "Stored stack not found.")))

(defun my/calc-quick-variable ()
  "Reads a character and pushes it as a variable onto the calc stack."
  (interactive)
  (let ((char (read-char-from-minibuffer "Enter a variable: ")))
    (if (or (and (>= char ?a) (<= char ?z))
            (and (>= char ?A) (<= char ?Z)))
        (my/preserve-point
         (let* ((var-name (intern (char-to-string char)))
                (var-symbol (intern (concat "var-" (char-to-string char)))))
           (calc-wrapper
            (calc-push (list 'var var-name var-symbol)))))
      (message "Invalid character. Must be a-z or A-Z."))))

(defun my/calc--identify-expr (x)
  "Try to identify X as a simple closed-form expression.
Tries in order: integer, fraction p/q (|q|≤20), (p/q)·√n (n≤30),
√n, n^(1/3), n^(1/4), (p/q)·π, (p/q)·e, ln(n) (n≤1000).
Signals an error if no candidate matches within 1e-8."
  (let* ((calc-symbolic-mode nil)
         (xf  (math-evaluate-expr x))
         (neg (math-negp xf))
         (ax  (if neg (math-neg xf) xf))
         (tol '(float 1 -8)))
    (cl-flet ((close-p (sym)
                (math-lessp
                 (math-abs (math-sub (math-evaluate-expr sym) ax)) tol))
              (maybe-neg (e) (if neg (math-neg e) e))
              (try-rat (af)
                (cl-loop for q from 1 to 20 thereis
                         (let* ((pf (math-mul af q))
                                (p  (calcFunc-round pf)))
                           (and (math-lessp (math-abs (math-sub pf p)) tol)
                                (if (= q 1) p
                                  (math-normalize (list 'frac p q))))))))
      (or
       ;; 1. Integer
       (let ((r (calcFunc-round ax)))
         (and (close-p r) (maybe-neg r)))
       ;; 2. Simple fraction p/q
       (let ((r (try-rat ax)))
         (and r (maybe-neg r)))
       ;; 3. (p/q)·√n  (ratio ≠ 1 to avoid duplicating case 4)
       (cl-loop for n from 2 to 30 thereis
                (let* ((sn (math-evaluate-expr (list 'calcFunc-sqrt n)))
                       (r  (try-rat (math-div ax sn))))
                  (and r (not (math-equal r 1))
                       (maybe-neg
                        (math-normalize (list '* r (list 'calcFunc-sqrt n)))))))
       ;; 4. √n
       (let* ((sq (math-evaluate-expr (list 'calcFunc-sqr ax)))
              (n  (calcFunc-round sq)))
         (and (close-p (list 'calcFunc-sqrt n))
              (math-posp n) (not (math-equal n 1))
              (maybe-neg (list 'calcFunc-sqrt n))))
       ;; 5. n^(1/3)
       (let* ((cb (math-evaluate-expr (list '^ ax 3)))
              (n  (calcFunc-round cb)))
         (and (close-p (list '^ n (list 'frac 1 3)))
              (math-posp n) (not (math-equal n 1))
              (maybe-neg (list '^ n (list 'frac 1 3)))))
       ;; 6. n^(1/4)
       (let* ((qt (math-evaluate-expr (list '^ ax 4)))
              (n  (calcFunc-round qt)))
         (and (close-p (list '^ n (list 'frac 1 4)))
              (math-posp n) (not (math-equal n 1))
              (maybe-neg (list '^ n (list 'frac 1 4)))))
       ;; 7. (p/q)·π
       (let ((r (try-rat (math-div ax (math-evaluate-expr '(var pi var-pi))))))
         (and r (maybe-neg (math-normalize (list '* r '(var pi var-pi))))))
       ;; 8. (p/q)·e
       (let ((r (try-rat (math-div ax (math-evaluate-expr '(var e var-e))))))
         (and r (maybe-neg (math-normalize (list '* r '(var e var-e))))))
       ;; 9. ln(n) — only for x > 0
       (and (not neg)
            (let* ((en (math-evaluate-expr (list 'calcFunc-exp ax)))
                   (n  (calcFunc-round en)))
              (and (close-p (list 'calcFunc-ln n))
                   (>= (math-compare n 2) 0)
                   (<= (math-compare n 1000) 0)
                   (list 'calcFunc-ln n))))
       (error "Cannot identify %s as a simple expression"
              (math-format-value xf))))))

(defun my/calc-evaluate (n)
  "Evaluate the target expression with symbolic mode disabled.
With inverse (I k k): identify the expression as a simple closed-form
(integer, fraction, sqrt, cbrt, π/e multiple, ln…), keeping the result
symbolic.  Works contextually: operates on selection, sub-formula at
point, or stack entry at level N (default 1)."
  (interactive "p")
  (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "eval") (m n))
    (unwind-protect
        (if (calc-is-inverse)
            (let ((calc-symbolic-mode t))
              (replace-expr (my/calc--identify-expr expr)))
          (let ((calc-symbolic-mode nil))
            (replace-expr (math-evaluate-expr expr))))
      (calc-set-mode-line))))

(defun my/calc-recall-quick ()
  "Like calc-recall-quick, but don't simplify."
  (interactive)
  (my/calc-without-simplification
    (call-interactively 'calc-recall-quick)))

(defun my/calc-recall ()
  "Like calc-recall, but don't simplify."
  (interactive)
  (my/calc-without-simplification
    (call-interactively 'calc-recall)))

(defun my/calc-recall-browse ()
  "Recall a Calc variable chosen from a completing-read annotated with values."
  (interactive)
  (require 'calc-ext)
  (let* ((excluded (list "CommuteRules" "Decls" "DistribRules" "EvalRules" "FactorRules"
                         "FitRules" "Holidays" "IntegAfterRules" "InvertRules" "JumpRules"
                         "MergeRules" "Modes" "NegateRules" "e" "gamma" "i" "phi" "pi"
                         "q0" "q1" "q2" "q3" "q4" "q5" "q6" "q7" "q8" "q9"
                         "γ" "π" "φ"))
         (all (let (acc)
                (mapatoms (lambda (sym)
                  (when (and (string-prefix-p "var-" (symbol-name sym))
                             (boundp sym)
                             (symbol-value sym))
                    (let ((name (substring (symbol-name sym) 4)))
                      (unless (member name excluded)
                        (push name acc))))))
                acc))
         (names (append
                 (sort (seq-filter (lambda (n) (not (string-prefix-p "eq-" n))) all) #'string<)
                 (sort (seq-filter (lambda (n) (string-prefix-p "eq-" n)) all) #'string<)))
         (mapping (mapcar (lambda (name)
                            (cons (if (string-prefix-p "eq-" name)
                                      (substring name 3)
                                    (concat "var-" name))
                                  name))
                          names))
         (annotate (lambda (disp)
                     (let ((val (symbol-value (intern (concat "var-" (cdr (assoc disp mapping)))))))
                       (concat "  " (math-format-value val 100)))))
         (chosen (let ((completion-extra-properties
                        (list :annotation-function annotate)))
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (keymap-local-set "TAB" (lambda ()
                                                   (interactive)
                                                   (if (>= ivy--index (1- ivy--length))
                                                       (ivy-beginning-of-buffer)
                                                     (ivy-next-line))))
                         (keymap-local-set "<backtab>" (lambda ()
                                                         (interactive)
                                                         (if (<= ivy--index 0)
                                                             (ivy-end-of-buffer)
                                                           (ivy-previous-line)))))
                     (completing-read "Recall: " (mapcar #'car mapping) nil t))))
         )
    (my/calc-without-simplification
      (calc-recall (intern (concat "var-" (cdr (assoc chosen mapping))))))))

(defun my/calc-plus (arg)
  "Add top two stack items. If both are equations, add both sides; else calc-plus."
  (interactive "P")
  (if (and (>= (calc-stack-size) 2)
           (eq (car-safe (calc-top-n 1)) 'calcFunc-eq)
           (eq (car-safe (calc-top-n 2)) 'calcFunc-eq))
      (calc-wrapper
       (let* ((eq1 (calc-top-n 1))
              (eq2 (calc-top-n 2))
              (lhs  (math-simplify (math-add (nth 1 eq2) (nth 1 eq1))))
              (rhs  (math-simplify (math-add (nth 2 eq2) (nth 2 eq1))))
              (result (list 'calcFunc-eq lhs rhs))
              (calc-simplify-mode nil))
         (calc-enter-result 2 "eq+" result)))
    (calc-plus arg)))

(defun my/calc-minus (arg)
  "Subtract top two stack items. If both are equations, subtract both sides; else calc-minus."
  (interactive "P")
  (if (and (>= (calc-stack-size) 2)
           (eq (car-safe (calc-top-n 1)) 'calcFunc-eq)
           (eq (car-safe (calc-top-n 2)) 'calcFunc-eq))
      (calc-wrapper
       (let* ((eq1 (calc-top-n 1))
              (eq2 (calc-top-n 2))
              (lhs  (math-simplify (math-sub (nth 1 eq2) (nth 1 eq1))))
              (rhs  (math-simplify (math-sub (nth 2 eq2) (nth 2 eq1))))
              (result (list 'calcFunc-eq lhs rhs))
              (calc-simplify-mode nil))
         (calc-enter-result 2 "eq-" result)))
    (calc-minus arg)))

(defun my/calc-equal-to (arg)
  "Like calc-equal-to, but with I prefix calls calc-not-equal-to."
  (interactive "P")
  (my/calc-without-simplification
    (if calc-inverse-flag
        (calc-not-equal-to arg)
      (calc-equal-to arg))))

(defun my/calc-change-sign ()
  "Negate the active selection, sub-formula at point, or top stack entry."
  (interactive)
  (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "chs"))
    (replace-expr (math-neg expr))))

(defun my/calc-inv ()
  "Invert the active selection, sub-formula at point, or top stack entry."
  (interactive)
  (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "inv"))
    (replace-expr (calcFunc-inv expr))))


(defun my/calc-poly-lcm ()
  "Compute the LCM of two polynomials on the stack."
  (interactive)
  (cl-labels
      ((int-content (expr)
         ;; GCD of integer coefficients.  calc-normalize distributes n*(sum+const)
         ;; so we extract content at the alist level, not via arithmetic.
         (cond ((math-integerp expr) (math-abs expr))
               ((memq (car-safe expr) '(+ -))
                (math-gcd (int-content (nth 1 expr))
                          (int-content (nth 2 expr))))
               ((eq (car-safe expr) '*)
                (if (math-integerp (nth 1 expr))
                    (math-abs (nth 1 expr))
                  1))
               (t 1)))
       (factorize (expr)
         ;; Return alist of (base . exponent).  For sums, extract integer
         ;; content as a separate numeric entry so the LCM algorithm sees the
         ;; true polynomial structure — bypassing calc-normalize's distribution.
         (cond
          ((eq (car-safe expr) '*)
           (append (factorize (nth 1 expr)) (factorize (nth 2 expr))))
          ((eq (car-safe expr) '^)
           (list (cons (nth 1 expr) (nth 2 expr))))
          ((memq (car-safe expr) '(+ -))
           (let* ((c        (int-content expr))
                  (prim     (if (math-equal c 1) expr (math-div expr c)))
                  (factored (calcFunc-factor prim))
                  (pf       (if (eq (car-safe factored) '*)
                                (factorize factored)
                              (list (cons factored 1)))))
             (if (math-equal c 1) pf (cons (cons c 1) pf))))
          (t
           (list (cons expr 1)))))
       (mul-factors (pairs)
         (cl-reduce (lambda (acc p) (list '* acc (list '^ (car p) (cdr p))))
                    pairs :initial-value 1)))
    (calc-wrapper
     (let* ((fa (factorize (calc-top-n 2)))
            (fb (factorize (calc-top-n 1)))
            (nums-a  (cl-remove-if-not (lambda (p) (math-numberp (car p))) fa))
            (nums-b  (cl-remove-if-not (lambda (p) (math-numberp (car p))) fb))
            (polys-a (cl-remove-if     (lambda (p) (math-numberp (car p))) fa))
            (polys-b (cl-remove-if     (lambda (p) (math-numberp (car p))) fb))
            (coeff-a (cl-reduce #'math-mul (mapcar #'car nums-a) :initial-value 1))
            (coeff-b (cl-reduce #'math-mul (mapcar #'car nums-b) :initial-value 1))
            (coeff   (calcFunc-lcm coeff-a coeff-b))
            (bases   (cl-remove-duplicates (mapcar #'car (append polys-a polys-b))
                                           :test #'math-equal))
            (poly-pairs
             (mapcar (lambda (base)
                       (let ((ea (or (cdr (cl-find base polys-a :key #'car :test #'math-equal)) 0))
                             (eb (or (cdr (cl-find base polys-b :key #'car :test #'math-equal)) 0)))
                         (cons base (max ea eb))))
                     bases))
            (poly-part (math-normalize (mul-factors poly-pairs))))
       (if (math-equal coeff 1)
           (calc-enter-result 2 "plcm" poly-part)
         ;; Use 'none simplification to prevent calc-normalize from distributing
         ;; the integer coefficient into a sum: 12*(x+1) → 12x+12.
         (my/calc-without-simplification
           (calc-enter-result 2 "plcm" (list '* coeff poly-part))))))))

(defun my/calc--sum-terms (expr)
  "Return a flat list of additive terms in EXPR."
  (if (eq (car-safe expr) '+)
      (append (my/calc--sum-terms (nth 1 expr))
              (my/calc--sum-terms (nth 2 expr)))
    (list expr)))

(defun my/calc-factor-by ()
  "Factors an expression by an argument.
With no selection: factors stack level 2 by stack level 1.
With selection active: factors the selected expression by the top of stack."
  (interactive)
  (my/calc-replace-expr-dwim (expr top replace-expr) ((m 2) (prefix "fctr") (simp -1) (pop-stack 1))
    (let* ((factor top)
           (divided (-> (calcFunc-div expr factor) calcFunc-expand calcFunc-nrat calcFunc-expand math-simplify))
           (factored (calcFunc-mul factor divided)))
      (replace-expr factored))))

(defun my/calc-factor-by-gcd ()
  "Factor the expression by the GCD of its additive terms.
Automatically computes the GCD of all terms and factors it out.
Works contextually: operates on selection, sub-formula at point,
or top stack entry."
  (interactive)
  (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "fctr"))
    (let* ((terms (my/calc--sum-terms expr))
           (factor (let ((calc-simplify-mode nil))
                     (cl-reduce #'calcFunc-pgcd terms)))
           (divided (-> (calcFunc-div expr factor) calcFunc-expand calcFunc-nrat calcFunc-expand math-simplify))
           (factored (my/calc-without-simplification (calcFunc-mul factor divided))))
      (replace-expr factored))))

(defun my/calc-sqrt ()
  "Take the square root of the target expression.
With calc-is-inverse, squares instead (like calc-sqrt).
Works contextually: operates on selection, sub-formula at point,
or top stack entry."
  (interactive)
  (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "sqrt"))
    (replace-expr (calc-normalize
                   (if (calc-is-inverse)
                       (list 'calcFunc-sqr expr)
                     (list 'calcFunc-sqrt expr))))))

(defun my/calc-square ()
  "Square the target expression.
With calc-is-inverse, takes the square root instead (like calc-sqrt).
Works contextually: operates on selection, sub-formula at point,
or top stack entry."
  (interactive)
  (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "^2"))
    (replace-expr (calc-normalize
                   (if (calc-is-inverse)
                       (list 'calcFunc-sqrt expr)
                     (list 'calcFunc-sqr expr))))))

(defun my/math-ref-angle (x)
  "Given an angle, gets its reference angle."
  (let ((result (calcFunc-mod x 360)))
    (cond ((= (calcFunc-geq result 270) 1) (calcFunc-sub 360 result))
          ((= (calcFunc-geq result 180) 1) (calcFunc-sub result 180))
          ((= (calcFunc-geq result 90) 1) (calcFunc-sub 180 result))
          (t result))))

(defun my/calc--expr-contains-pi (expr)
  "Return non-nil if EXPR contains pi as a symbolic variable."
  (cond ((equal expr '(var pi var-pi)) t)
        ((listp expr) (cl-some #'my/calc--expr-contains-pi (cdr expr)))
        (t nil)))

(defun my/calc-supplement ()
  "Compute the supplement of the angle on the stack (180 - x or pi - x).
Uses pi-based arithmetic when the expression contains pi; otherwise uses
the current angle mode."
  (interactive)
  (calc-wrapper
   (let* ((x (calc-top-n 1))
          (half-turn (if (or (my/calc--expr-contains-pi x)
                             (eq calc-angle-mode 'rad))
                         (list 'var 'pi 'var-pi)
                       180)))
     (calc-enter-result 1 "supp" (math-sub half-turn x)))))

(defun my/calc-complement ()
  "Compute the complement of the angle on the stack (90 - x or pi/2 - x).
Uses pi-based arithmetic when the expression contains pi; otherwise uses
the current angle mode."
  (interactive)
  (calc-wrapper
   (let* ((x (calc-top-n 1))
          (quarter-turn (if (or (my/calc--expr-contains-pi x)
                                (eq calc-angle-mode 'rad))
                            (list '/ (list 'var 'pi 'var-pi) 2)
                          90)))
     (calc-enter-result 1 "comp" (math-sub quarter-turn x)))))

(defun my/calc-ref-angle (arg)
  "Given an angle, gets its reference angle."
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "refa" 'my/math-ref-angle arg)))

(defun my/calc--combinations (lst k)
  (cond ((= k 0) '(()))
        ((null lst) '())
        (t (append
            (mapcar (lambda (rest) (cons (car lst) rest))
                    (my/calc--combinations (cdr lst) (1- k)))
            (my/calc--combinations (cdr lst) k)))))

(defun my/calc-unique-groups ()
  "Pop vector and n from stack; push all unique groups of size n.
If top of stack is a vector, n defaults to 2."
  (interactive)
  (calc-wrapper
   (let* ((top  (calc-top-n 1))
          (vec-only (eq (car-safe top) 'vec))
          (n   (if vec-only 2 top))
          (vec (if vec-only top (calc-top-n 2)))
          (nargs (if vec-only 1 2)))
     (unless (eq (car-safe vec) 'vec)
       (error "Expected a vector"))
     (unless (and (integerp n) (> n 0))
       (error "Expected a positive integer for n"))
     (let* ((combos (my/calc--combinations (cdr vec) n))
            (result (cons 'vec (mapcar (lambda (g) (cons 'vec g)) combos))))
       (calc-enter-result nargs "ugrp" result)))))

(defun my/calc-vector-flatten ()
  "Flattens a vector."
  (interactive)
  (calc-wrapper
   (let ((flat-vector (list 'calcFunc-arrange (calc-top-n 1) (prefix-numeric-value 0))))
     (calc-enter-result 1 "flat" flat-vector))))

(define-advice calcFunc-hypot (:around (orig a b) "symbolic-simplify")
  "When inputs are symbolic (e.g. sqrt(3)), compute sqrt(abssqr(a)+abssqr(b)).
The original calcFunc-hypot requires Math-scalarp on both args; it falls back to
the inert form (calcFunc-hypot a b) for symbolic inputs like (calcFunc-sqrt 3).
calcFunc-abssqr reduces sqrt(n)^2 → n, so the sum is often an integer and
calcFunc-sqrt can either simplify it (perfect square) or return sqrt(n) symbolically."
  (let ((result (funcall orig a b)))
    (if (and (consp result) (eq (car result) 'calcFunc-hypot))
        (let ((sum (math-add (calcFunc-abssqr a) (calcFunc-abssqr b))))
          (if (Math-numberp sum)
              (calcFunc-sqrt sum)
            result))
      result)))

(defun my/calc-cath ()
  "Compute a leg of a right triangle: pops leg (level 1) and hypotenuse (level 2), returns √(h²-a²)."
  (interactive)
  (save-excursion
    (calc-wrapper
     (let* ((leg (calc-top-n 1))
            (hyp (calc-top-n 2))
            (result (calcFunc-sqrt (calcFunc-sub (calcFunc-pow hyp 2) (calcFunc-pow leg 2)))))
       (calc-enter-result 2 "cath" result)))))

(defun my/calc-unit-cath ()
  "Like my/calc-cath but assumes hypotenuse=1 (unit circle): pops leg (level 1), returns √(1-a²)."
  (interactive)
  (save-excursion
    (calc-wrapper
     (let* ((leg (calc-top-n 1))
            (result (calcFunc-sqrt (calcFunc-sub 1 (calcFunc-pow leg 2)))))
       (calc-enter-result 1 "ucth" result)))))

(defun my/calc-substitute (oldname)
  "Like calc-substitute, but the new-name prompt starts empty."
  (interactive "sSubstitute old: ")
  (calc-slow-wrapper
   (let (old new (num 1) expr)
     (if (or (equal oldname "") (equal oldname "$") (null oldname))
         (setq new (calc-top-n 1)
               old (calc-top-n 2)
               expr (calc-top-n 3)
               num 3)
       (let ((newname (read-string (concat "Substitute old: " oldname ", new: "))))
         (if (or (equal newname "") (equal newname "$") (null newname))
             (setq new (calc-top-n 1)
                   expr (calc-top-n 2)
                   num 2)
           (setq new (math-read-expr newname))
           (if (eq (car-safe new) 'error)
               (error "Bad format in expression: %s" (nth 1 new)))
           (setq expr (calc-top-n 1)))
         (setq old (math-read-expr oldname))
         (if (eq (car-safe old) 'error)
             (error "Bad format in expression: %s" (nth 1 old)))
         (or (math-expr-contains expr old)
             (error "No occurrences found"))))
     (calc-enter-result num "sbst" (math-expr-subst expr old new)))))


(defun my/calc-swap-variables ()
  "Swap two variables in the top-of-stack expression.
Prompts for two variable names; defaults to the first two auto-detected vars."
  (interactive)
  (calc-slow-wrapper
   (my/calc-without-simplification
     (let* ((expr    (calc-top-n 1))
            (detected (my/calc-auto-solve--sorted-vars expr))
            (def1    (when (nth 0 detected) (symbol-name (nth 1 (nth 0 detected)))))
            (def2    (when (nth 1 detected) (symbol-name (nth 1 (nth 1 detected)))))
            (default (when (and def1 def2) (format "[%s %s]" def1 def2)))
            (prompt  (if default
                         (format "Swap [a b] (default: %s): " default)
                       "Swap [a b]: "))
            (input   (read-string prompt))
            (input   (if (string-empty-p (string-trim input)) (or default "") input))
            (parts   (split-string (replace-regexp-in-string "[][,]" " " input) nil t))
            (var1    (math-read-expr (nth 0 parts)))
            (var2    (math-read-expr (nth 1 parts)))
            (tmp     '(var --swap-tmp-- var---swap-tmp--)))
       (unless (and (eq (car-safe var1) 'var) (eq (car-safe var2) 'var))
         (error "Expected two variable names separated by a comma"))
       (let* ((r (math-expr-subst expr var1 tmp))
              (r (math-expr-subst r    var2 var1))
              (r (math-expr-subst r    tmp  var2)))
         (calc-enter-result 1 "swap" r))))))


;; calc-fancy-prefix-other-key clears calc-keep-args-flag before replaying
;; non-character keys (like C-<return>). Save it in a post-command-hook
;; right after K fires so we can recover it.
(defvar my/calc--saved-keep-flag nil)

(defun my/calc--save-keep-flag ()
  (when (eq this-command 'calc-keep-args)
    (setq my/calc--saved-keep-flag
          (buffer-local-value 'calc-keep-args-flag
                              (or (get-buffer "*Calculator*") (current-buffer))))))

(add-hook 'post-command-hook #'my/calc--save-keep-flag)

(defun my/calc-quick-substitution ()
  "Substitute the top-of-stack assignment into the second stack item.
Removes both items by default. With K prefix, keeps them."
  (interactive)
  (require 'calc-store)
  (let ((keep (if (eq last-command 'calc-fancy-prefix-other-key)
                  (prog1 my/calc--saved-keep-flag
                    (setq my/calc--saved-keep-flag nil))
                calc-keep-args-flag)))
    (calc-slow-wrapper
     (let* ((assignments (calc-is-assignments (calc-top 1)))
            (thing       (calc-top 2)))
       (when assignments
         (let ((result
                (let ((saved (mapcar (lambda (v)
                                      (cons (car v)
                                            (and (boundp (car v))
                                                 (symbol-value (car v)))))
                                    assignments)))
                  (unwind-protect
                      (progn
                        (dolist (v assignments)
                          (set (car v) (calc-normalize (cdr v)))
                          (calc-refresh-evaltos (car v)))
                        (let ((r (math-evaluate-expr thing)))
                          (if (and (eq (car-safe r) 'calcFunc-eq)
                                   (math-numberp (nth 1 r)))
                              (list 'calcFunc-eq (nth 2 r) (nth 1 r))
                            r)))
                    (dolist (v saved)
                      (if (cdr v)
                          (set (car v) (cdr v))
                        (makunbound (car v))))))))
           (if keep
               (calc-push-list (list result))
             (calc-enter-result 2 "let" result))))))))

(defun my/calc-trail--parse-line ()
  "Parse and return the calc expression on the current trail line."
  (save-excursion
    (beginning-of-line)
    (when (or (looking-at "Emacs Calc")
              (looking-at "----")
              (looking-at " ? ? ?[^ \n]* *$")
              (looking-at "..?.?$"))
      (error "Can't yank that line"))
    (when (looking-at ".*, \\.\\.\\., ")
      (error "Can't yank (vector was abbreviated)"))
    (forward-char 4)
    (search-forward " ")
    (let* ((next (save-excursion (forward-line 1) (point)))
           (str  (buffer-substring (point) (1- next)))
           (val  (math-read-plain-expr str)))
      (if (eq (car-safe val) 'error)
          (error "Can't yank that line: %s" (nth 2 val))
        val))))

(defun my/calc-trail-yank-at-point ()
  "Yank trail entry at point onto the calc stack and close the trail window."
  (interactive)
  (let ((val (my/calc-trail--parse-line)))
    (calc-wrapper (calc-enter-result 0 "yank" val))
    (calc-trail-display nil nil t)))

(defun my/calc-trail-yank-at-point-keep ()
  "Yank trail entry at point onto the calc stack, keeping the trail window."
  (interactive)
  (let ((val (my/calc-trail--parse-line)))
    (calc-wrapper (calc-enter-result 0 "yank" val))))

(defun my/calc-trail-beginning-of-entry ()
  "Move point to the beginning of the trail entry on the current line (after prefix)."
  (interactive)
  (beginning-of-line)
  (forward-char 5))


(defun my/calc-inverse-function ()
  "Find the inverse of a function on the stack.
Accepts y=f(x) equations or f(x) expressions, with any variable names.
Works contextually: operates on the entry at point or top of stack."
  (interactive)
  (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "inv") (line t))
    (let* ((var-y     '(var y var-y))
           (is-eq     (eq (car-safe expr) 'calcFunc-eq))
           (lhs       (and is-eq (nth 1 expr)))
           (rhs       (and is-eq (nth 2 expr)))
           (lhs-var-p (and is-eq (eq (car-safe lhs) 'var)))
           (rhs-var-p (and is-eq (eq (car-safe rhs) 'var)))
           (out-lhs   (cond ((not is-eq) var-y)
                            (lhs-var-p   lhs)
                            (rhs-var-p   rhs)
                            (t           lhs)))
           (fx        (cond ((not is-eq) expr)
                            (rhs-var-p   lhs)
                            (t           rhs)))
           (work-var  (if (eq (car-safe out-lhs) 'var) out-lhs var-y))
           (in-var    (or (seq-find (lambda (v) (not (equal v work-var)))
                                    (my/calc-auto-solve--sorted-vars fx))
                          '(var x var-x)))
           (fy        (math-expr-subst fx in-var work-var))
           (sol       (math-solve-eqn (list 'calcFunc-eq fy in-var) work-var nil))
           (result    (when (eq (car-safe sol) 'calcFunc-eq)
                        (list 'calcFunc-eq out-lhs (nth 2 sol)))))
      (if result
          (replace-expr result)
        (message "Can't find inverse")))))

(defun my/calc-auto-solve--sorted-vars (expr)
  "Return unique non-constant variables in EXPR.
Conventional input variables (x, y, z, t) sort first; remaining
variables sort alphabetically after them."
  (let (vars)
    (cl-labels ((collect (e)
                  (cond ((and (eq (car-safe e) 'var) (not (math-const-var e)))
                         (cl-pushnew e vars :test #'equal))
                        ((listp e) (mapc #'collect (cdr e))))))
      (collect expr))
    (let ((priority '("x" "y" "z" "t")))
      (sort vars (lambda (a b)
                   (let* ((na (symbol-name (nth 1 a)))
                          (nb (symbol-name (nth 1 b)))
                          (pa (or (cl-position na priority :test #'string=) 999))
                          (pb (or (cl-position nb priority :test #'string=) 999)))
                     (or (< pa pb)
                         (and (= pa pb) (string< na nb)))))))))

(defun my/calc-auto-solve--solved-for (expr)
  "Return the variable EXPR is solved for (plain var alone on one side), or nil."
  (when (eq (car-safe expr) 'calcFunc-eq)
    (let ((lhs (nth 1 expr))
          (rhs (nth 2 expr)))
      (cond ((eq (car-safe lhs) 'var) lhs)
            ((eq (car-safe rhs) 'var) rhs)))))

(defun my/calc-auto-solve ()
  "Solve top-of-stack for a variable.
With one variable, solves for it. With multiple, solves for the first
alphabetically, or cycles to the next if already solved for one."
  (interactive)
  (let* ((expr (calc-top-n 1))
         (vars (my/calc-auto-solve--sorted-vars expr))
         (n    (length vars)))
    (when (> n 0)
      (let* ((solved-for (and (> n 1) (my/calc-auto-solve--solved-for expr)))
             (var (if solved-for
                      (let* ((idx  (cl-position solved-for vars :test #'equal))
                             (next (mod (1+ (or idx -1)) n)))
                        (nth next vars))
                    (car vars)))
             (result (math-solve-eqn expr var nil)))
        (if result
            (calc-wrapper
             (calc-enter-result 1 "slv" result))
          (message "Can't solve for %s" (math-format-flat-expr var 0)))))))


(defun my/calc-poly-roots ()
  "Find roots of the polynomial or equation on top of stack.
Accepts expressions (f(x)) or equations (f(x) = 0).
With one variable, solves automatically. With multiple, prompts for the variable."
  (interactive)
  (cl-labels ((collect-factors (expr)
                (cond ((eq (car-safe expr) '*)
                       (append (collect-factors (nth 1 expr))
                               (collect-factors (nth 2 expr))))
                      ((and (eq (car-safe expr) '^)
                            (integerp (nth 2 expr))
                            (> (nth 2 expr) 0))
                       (let ((sub (collect-factors (nth 1 expr)))
                             (e   (nth 2 expr)))
                         (mapcar (lambda (fm) (cons (car fm) (* (cdr fm) e))) sub)))
                      (t (list (cons expr 1)))))
              (roots-of (poly var)
                (let* ((factors (collect-factors (calcFunc-factor poly)))
                       (all-roots
                        (cl-mapcan
                         (lambda (fm)
                           (let* ((r  (calcFunc-roots (car fm) var))
                                  (rs (when (eq (car-safe r) 'vec) (cdr r))))
                             (cl-mapcan (lambda (root) (make-list (cdr fm) root)) rs)))
                         factors)))
                  (cons 'vec all-roots))))
    (let* ((expr (calc-top-n 1))
           (poly (if (eq (car-safe expr) 'calcFunc-eq)
                     (let ((lhs (nth 1 expr)) (rhs (nth 2 expr)))
                       (if (and (= (length lhs) 2)
                                (string-prefix-p "calcFunc-" (symbol-name (car-safe lhs))))
                           rhs
                         (calcFunc-sub lhs rhs)))
                   expr))
           (vars (my/calc-auto-solve--sorted-vars poly))
           (n (length vars)))
      (cond
       ((= n 0) (message "No variables found"))
       ((= n 1)
        (calc-wrapper (calc-enter-result 1 "root" (roots-of poly (car vars)))))
       (t
        (let ((var (math-read-expr (read-string "Variable: "))))
          (calc-wrapper (calc-enter-result 1 "root" (roots-of poly var)))))))))


(defvar my/calc--sorting-poly nil)

(defun my/calc-poly-term-degree (term var)
  "Return the degree of TERM as a polynomial in VAR."
  (cond
   ((equal term var) 1)
   ((and (eq (car-safe term) '^)
         (equal (nth 1 term) var)
         (integerp (nth 2 term)))
    (nth 2 term))
   ((eq (car-safe term) '*)
    (+ (my/calc-poly-term-degree (nth 1 term) var)
       (my/calc-poly-term-degree (nth 2 term) var)))
   ((eq (car-safe term) 'neg)
    (my/calc-poly-term-degree (nth 1 term) var))
   (t 0)))

(defun my/calc-sum-to-list (expr &optional negated)
  "Flatten a +/- sum EXPR into a list of (term . positive-p) pairs."
  (cond
   ((eq (car-safe expr) '+)
    (append (my/calc-sum-to-list (nth 1 expr) negated)
            (my/calc-sum-to-list (nth 2 expr) negated)))
   ((eq (car-safe expr) '-)
    (append (my/calc-sum-to-list (nth 1 expr) negated)
            (my/calc-sum-to-list (nth 2 expr) (not negated))))
   (t (list (cons expr (not negated))))))

(defun my/calc-list-to-sum (pairs)
  "Rebuild a +/- sum from a list of (term . positive-p) pairs."
  (let* ((first (car pairs))
         (result (if (cdr first) (car first) (list 'neg (car first)))))
    (dolist (pair (cdr pairs) result)
      (setq result
            (if (cdr pair)
                (list '+ result (car pair))
              (list '- result (car pair)))))))

(defun my/calc-poly-sort-sum (expr)
  "If EXPR is a single-variable polynomial sum, sort terms by descending degree."
  (let ((vars (my/calc-auto-solve--sorted-vars expr)))
    (if (= (length vars) 1)
        (let* ((var (car vars))
               (terms (my/calc-sum-to-list expr))
               (sorted (sort (copy-sequence terms)
                             (lambda (a b)
                               (> (my/calc-poly-term-degree (car a) var)
                                  (my/calc-poly-term-degree (car b) var))))))
          (my/calc-list-to-sum sorted))
      expr)))

(with-eval-after-load 'calc
  (advice-add 'math-normalize :filter-return
    (lambda (result)
      (if (and (not my/calc--sorting-poly)
               (not (eq calc-simplify-mode 'none))
               (memq (car-safe result) '(+ -)))
          (let ((my/calc--sorting-poly t))
            (my/calc-poly-sort-sum result))
        result))))


(provide 'my/calc/stack)
