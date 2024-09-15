;; Scratch buffer for testing out code.
;;
;;


(defun my/sql-goto-sqli-window ()
  (let ((current-window (selected-window)))
    (walk-windows (lambda (window)
                    (with-current-buffer (window-buffer window)
                      (when (eq major-mode 'sql-interactive-mode)
                        (end-of-buffer)
                        (comint-kill-input)
                        (insert "status")
                        (comint-send-input)))))))

(my/sql-goto-sqli-window)

(defun my/sql-status ()
  (sql-show-sqli-buffer)
  (other-window)
  (sql-send-string "status"))

(defun my/sql-databases ()
  (sql-send-string "show databases"))

(defun my/sql-tables ()
  (sql-send-string "show tables"))

(defun my/sql-eval (command)
  (sql-send-string command))

(my/sql-eval "select * fr")

(my/sql-databases)
(my/sql-tables)
(my/sql-status)

(my/sql-select )
