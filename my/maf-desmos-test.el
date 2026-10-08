;; -*- lexical-binding: t; -*-
;;
;; maf-desmos-test.el
;;
;; A maf step test for the maf-desmos module (my/maf-desmos.el): its
;; key claim, the LaTeX dialect, the URL contract, finding a browser,
;; and what a 2D calculator is sent. Run in a live Emacs with maf
;; loaded, as maf's own tests/ are.
(maf-step
  ;; Global state the test flips; restored at the end.
  (progn (load-file "~/.emacs.d/my/maf-desmos.el")
         (setq maf-desmos-test--mode maf-use-desmos-mode)
         nil)

  ;; A registered module, claiming g o while on; off, the key is
  ;; calc's again. The command is marked so the H flag survives the
  ;; g prefix.
  (cl-assert (assq 'maf-desmos maf-module-registry))
  (maf-use-desmos-mode 1)
  (cl-assert (eq (key-binding (kbd "g o")) 'maf-desmos-plot))
  (maf-use-desmos-mode -1)
  (cl-assert (not (eq (key-binding (kbd "g o")) 'maf-desmos-plot)))
  (cl-assert (get 'maf-desmos-plot 'maf-command))

  ;; Entries flatten per element: a point set is its pairs, a lone
  ;; [x, y] the one point, a vector of curves one expression each and
  ;; a plain entry itself.
  (cl-assert (equal (maf-desmos--expressions
                     (list (math-read-expr "[[0, 0], [12, 6]]")))
                    '("\\left(0,0\\right)" "\\left(12,6\\right)")))
  (cl-assert (equal (maf-desmos--expressions
                     (list (math-read-expr "[12, 6]")))
                    '("\\left(12,6\\right)")))
  (cl-assert (equal (maf-desmos--expressions
                     (list (math-read-expr "[sin(x), cos(x)]")
                           (math-read-expr "x^2")))
                    (list (math-read-expr "sin(x)")
                          (math-read-expr "cos(x)")
                          (math-read-expr "x^2"))))

  ;; Desmos reads a stricter LaTeX than calc writes: brace arguments
  ;; become parens (arcsin and its kin — the six trig calls of
  ;; `maf--latex-paren-calls' already arrive parenthesized from maf's
  ;; own composer, which Desmos reads as written), bare-pipe abs
  ;; becomes \left|...\right| (lifted at the tree, since nested pipes
  ;; cannot be re-paired in the string), and exp goes to e^x.
  ;; Desmos-native forms pass through untouched.
  (cl-assert (equal (maf-desmos--latex (math-read-expr "cos(x)"))
                    "\\cos(x)"))
  (cl-assert (equal (maf-desmos--latex (math-read-expr "sin(2 x)"))
                    "\\sin(2 x)"))
  ;; arcsin parenthesizes at the composer now, like sin and cos above,
  ;; so it arrives with the tight parens they do rather than the
  ;; \left( pair `maf-desmos--parenthesize' gave a braced
  ;; argument. Desmos reads both — the two assertions above are that
  ;; same tight form, and they are what it is sent today.
  (cl-assert (equal (maf-desmos--latex (math-read-expr "arcsin(x)"))
                    "\\arcsin(x)"))
  (cl-assert (equal (maf-desmos--latex
                     (math-read-expr "abs(abs(x) + 1)"))
                    "\\left|\\left|x\\right| + 1\\right|"))
  (cl-assert (equal (maf-desmos--latex (math-read-expr "abs(sin(x))"))
                    "\\left|\\sin(x)\\right|"))
  (cl-assert (equal (maf-desmos--latex (math-read-expr "exp(x)"))
                    "e^x"))
  (cl-assert (equal (maf-desmos--latex (math-read-expr "sqrt(x)"))
                    "\\sqrt{x}"))
  ;; The 10 the bare \log assumes stays unwritten, as in pretty.
  (cl-assert (equal (maf-desmos--latex (math-read-expr "log10(x)"))
                    "\\log\\left( x \\right)"))

  ;; Desmos graphs in x and y: a lone foreign variable is renamed to x
  ;; (sin(t) as sent would offer a slider, not a curve). Constants
  ;; Desmos knows don't count as foreign; x present, or two foreign
  ;; variables, leave the expression alone.
  (cl-assert (equal (maf-desmos--normalize (math-read-expr "sin(t)"))
                    (math-read-expr "sin(x)")))
  (cl-assert (equal (maf-desmos--normalize (math-read-expr "y = 2 t"))
                    (math-read-expr "y = 2 x")))
  (cl-assert (equal (maf-desmos--normalize (math-read-expr "pi t"))
                    (math-read-expr "pi x")))
  (cl-assert (equal (maf-desmos--normalize (math-read-expr "sin(x) + t"))
                    (math-read-expr "sin(x) + t")))
  (cl-assert (equal (maf-desmos--normalize (math-read-expr "a t"))
                    (math-read-expr "a t")))

  ;; A data vector goes to Desmos as points, preformatted.
  (cl-assert (equal (maf-desmos--expressions
                     (list (math-read-expr "[1, 3, 2]")))
                    (list "\\left(1,1\\right)" "\\left(2,3\\right)"
                          "\\left(3,2\\right)")))

  ;; The desmos URL: the fragment alone carries the graph — latex per
  ;; entry (relations whole), the angle mode, and the API key.
  (progn
    (setq maf-desmos-test--spec
          (let* ((url (maf-desmos--url
                       (list (math-read-expr "y = 2 sin(x + 1)"))))
                 (json (url-unhex-string
                        (substring url (1+ (string-match "#" url))))))
            (json-parse-string json :object-type 'alist)))
    nil)
  (cl-assert (equal (aref (alist-get 'e maf-desmos-test--spec) 0)
                    "y = 2 \\sin(x + 1)"))
  (cl-assert (eq (alist-get 'd maf-desmos-test--spec) t))
  (cl-assert (equal (alist-get 'k maf-desmos-test--spec)
                    maf-desmos-api-key))
  ;; No range given, no bounds sent — Desmos keeps its own viewport.
  (cl-assert (null (alist-get 'b maf-desmos-test--spec)))
  ;; A range becomes the b pair: the viewport's opening x bounds.
  (cl-assert (equal (let* ((url (maf-desmos--url
                                 (list (math-read-expr "sin(x)"))
                                 '(-5.0 . 5.0)))
                           (json (url-unhex-string
                                  (substring url (1+ (string-match "#" url))))))
                      (alist-get 'b (json-parse-string
                                     json :object-type 'alist)))
                    [-5.0 5.0]))
  ;; Radians mode turns degreeMode off.
  (calc-radians-mode)
  (cl-assert (eq (let* ((url (maf-desmos--url
                              (list (math-read-expr "sin(x)"))))
                        (json (url-unhex-string
                               (substring url (1+ (string-match "#" url))))))
                   (alist-get 'd (json-parse-string json :object-type 'alist)))
                 :false))
  (calc-degrees-mode 1)

  ;; The page the URL opens ships beside the file.
  (cl-assert (file-exists-p
              (expand-file-name "maf-desmos.html" maf-desmos--directory)))

  ;; --- finding a browser to open it with ---

  ;; An XDG entry names the browser by .desktop file, so the program
  ;; is read out of its Exec line: the field codes the spec appends
  ;; are not part of it, a quoted path survives whole, and an entry
  ;; that runs nothing answers nothing. The program word is taken by
  ;; parsing rather than by trimming blanks, so a program whose name
  ;; opens on a blank's own letter arrives intact.
  (progn
    (setq maf-desmos-test--xdg (make-temp-file "maf-desmos-xdg" t))
    (let ((apps (expand-file-name "applications" maf-desmos-test--xdg)))
      (make-directory apps)
      (dolist (entry '(("plain.desktop"  . "Exec=/usr/bin/browse %U")
                       ("blankish.desktop" . "Exec=thunderbird %U")
                       ("quoted.desktop" . "Exec=\"/opt/my browser/run\" %U")
                       ("none.desktop"   . "NoExec=nothing")))
        (with-temp-file (expand-file-name (car entry) apps)
          (insert "[Desktop Entry]\nName=T\n" (cdr entry) "\n"))))
    (setq maf-desmos-test--xdg-orig (getenv "XDG_DATA_HOME"))
    (setenv "XDG_DATA_HOME" maf-desmos-test--xdg))
  (cl-assert (equal (maf-desmos--browser-desktop-exec "plain.desktop")
                    "/usr/bin/browse"))
  (cl-assert (equal (maf-desmos--browser-desktop-exec "blankish.desktop")
                    "thunderbird"))
  (cl-assert (equal (maf-desmos--browser-desktop-exec "quoted.desktop")
                    "/opt/my browser/run"))
  (cl-assert (null (maf-desmos--browser-desktop-exec "none.desktop")))
  (cl-assert (null (maf-desmos--browser-desktop-exec "absent.desktop")))

  ;; `maf-desmos-browser' settles it, whatever else is around and
  ;; without the opener test an unset one applies.
  (cl-assert (equal (let ((maf-desmos-browser "/opt/chosen"))
                      (maf-desmos--browser))
                    "/opt/chosen"))
  (cl-assert (equal (let ((maf-desmos-browser "xdg-open"))
                      (maf-desmos--browser))
                    "xdg-open"))

  ;; Unset, a generic opener is passed over rather than returned: it
  ;; would open the page and drop the graph. The search goes on to the
  ;; next source instead — here the PATH probe, stubbed to one name.
  (cl-assert (equal (let ((maf-desmos-browser nil)
                          (browse-url-generic-program "xdg-open")
                          (maf-desmos--browser-candidates '("stub-browser"))
                          (process-environment (cons "BROWSER=gio"
                                                     process-environment)))
                      (cl-letf (((symbol-function 'executable-find)
                                 (lambda (p &rest _)
                                   (and (equal p "stub-browser") "/bin/stub")))
                                ((symbol-function
                                  'maf-desmos--browser-desktop-default)
                                 (lambda () nil)))
                        (maf-desmos--browser)))
                    "stub-browser"))

  ;; Nothing anywhere is nil — the caller reports that rather than
  ;; launching something that would lose the fragment.
  (cl-assert (null (let ((maf-desmos-browser nil)
                         (browse-url-generic-program nil)
                         (maf-desmos--browser-candidates '("stub-browser"))
                         (process-environment (cons "BROWSER="
                                                    process-environment)))
                     (cl-letf (((symbol-function 'executable-find)
                                (lambda (&rest _) nil))
                               ((symbol-function
                                 'maf-desmos--browser-desktop-default)
                                (lambda () nil)))
                       (maf-desmos--browser)))))

  (progn (if maf-desmos-test--xdg-orig
             (setenv "XDG_DATA_HOME" maf-desmos-test--xdg-orig)
           (setenv "XDG_DATA_HOME" nil))
         (delete-directory maf-desmos-test--xdg t)
         nil)

  ;; Desmos's calculator is 2D: a surface over x and y is left out of
  ;; what goes to it, and a lone one refuses toward g l and g g. One
  ;; over other unknowns is a curve with sliders there — a x^2 still
  ;; goes over whole.
  (progn
    (setq maf-desmos-test--send
          (lambda (expressions)
            (let ((url nil))
              (cl-letf (((symbol-function 'maf-desmos--browser)
                         (lambda () "stub-browser"))
                        ((symbol-function 'start-process)
                         (lambda (&rest args) (setq url (car (last args))) nil)))
                (condition-case err
                    (progn
                      (maf-desmos--show expressions)
                      (append (alist-get
                               'e (json-parse-string
                                   (url-unhex-string
                                    (substring url (1+ (string-match "#" url))))
                                   :object-type 'alist))
                              nil))
                  (user-error (error-message-string err)))))))
    nil)
  (cl-assert (string-match-p "g l or g g plots x \\+ y in 3D"
                             (funcall maf-desmos-test--send
                                      (list (math-read-expr "x + y")))))
  (cl-assert (equal (funcall maf-desmos-test--send
                             (list (math-read-expr "z = x^2 + y^2")
                                   (math-read-expr "sin(x)")
                                   (math-read-expr "a x^2")))
                    '("\\sin(x)" "a x^2")))

  ;; Restore what the test flipped.
  (progn (maf-use-desmos-mode (if maf-desmos-test--mode 1 -1)) nil))
