;; Keyboard macros


(defalias 'my/execute-last-mx-command
  (kmacro "M-x M-p <return>")
  "Executes the last M-x command. You should only invoke this with a binding.")
