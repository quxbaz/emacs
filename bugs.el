;; A record of bugs.
;;
;;

;; Bug: eval-dwim
;; When point is at:
;;   'mouse-face 'highlight)|
;; eval-dwim should eval the entire list and not the line.
;; Don't eval lines, period. No point. Use syntax-ppss to find the opening parens and eval from there.
;; Be careful of point position. Always eval from where point began before invocation.
;; Make sure to check if you're in a list already before going down this path.
(propertize "hello world"
            'face '(:foreground "red" :weight "bold")
            'mouse-face 'highlight)

;; Both these cases should work.
|(setq-local my/list '(a b c))
|(setq-local alist '((a . 1)
                     (b . 2)
                     (c . 3)))
