;; Keyboard macros
;;
;;


(defalias 'eval-last-expr
  (kmacro "M-: M-p" 4 "%d"))

(defalias 'my/execute-last-mx-command
  (kmacro "M-x M-p <return>")
  "Executes the last M-x command. You should only invoke this with a binding.")
