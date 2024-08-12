;; Keyboard macros
;;
;;


(defalias 'eval-last-expr
  (kmacro "M-: M-p" 4 "%d")
  "Executes the most recent eval-expression string.")

(defalias 'my/execute-last-mx-command
  (kmacro "M-x M-p <return>")
  "Executes the most recent M-x command. You should only invoke this with a binding.")
