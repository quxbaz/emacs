;; A record of bugs.
;;
;;

;; Bug: eval-dwim
;; When point is at:
;;   'mouse-face 'highlight)|
;; eval-dwim should eval the entire list and not the line.
;; Don't eval lines, period. No point. Use syntax-ppss to find the opening parens and eval from there.
;; Be careful of point position. Always eval from where point began before invocation.
(propertize "hello world"
            'face '(:foreground "red" :weight "bold")
            'mouse-face 'highlight)
