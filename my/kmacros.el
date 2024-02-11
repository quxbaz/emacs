;; Keyboard macros


(defalias 'my/execute-prev-mx-command
  (kmacro "M-x M-p M-p <return>")
  "Executes the last M-x command. You should only invoke this with a binding.")
