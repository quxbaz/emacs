;; SQL library
;;
;; Collection of functions for interacting with SQL inside buffers; or really
;; anything SQL-related.


(define-minor-mode sql-lisp-mode
  "A mode for SQL interaction through evaluation of Emacs Lisp forms."
  :init-value nil
  :lighter " SQL-Lisp"
  :keymap (let ((map (make-sparse-keymap)))
            ;; (define-key map (kbd "KEY-SEQUENCE") 'COMMAND)
            map)
  :global nil
  :group 'sql-lisp-mode)

(defun my/sql-command (command)
  "Runs a special COMMAND usually only used in the interactive SQL shell."
  (if (memq command '(help status))
      ;; Find interactive SQL buffer.
      (walk-windows (lambda (window)
                      (with-current-buffer (window-buffer window)
                        (when (eq major-mode 'sql-interactive-mode)
                          (end-of-buffer)
                          (comint-kill-input)
                          (insert (symbol-name command))
                          (comint-send-input)))))
    (error "Not a valid command: %s" command)))

(defun my/sql-show-databases ()
  "Evaluates the SQL command: SHOW DATABASES"
  (sql-send-string "SHOW DATABASES;"))

(defun my/sql-show-tables ()
  "Evaluates the SQL command: SHOW TABLES"
  (sql-send-string "SHOW TABLES;"))

(defun my/sql-query (alist)
  "Builds a SQL query given an alist of SQL properties. Returns a
SQL query string."
  (let* ((columns (alist-get 'columns alist))
         (table (alist-get 'table alist))
         (where (alist-get 'where alist))
         (order-by (alist-get 'order-by alist))
         (order-by-column (if (eq (type-of order-by) 'cons) (car order-by) order-by))
         (order-by-sort (if (eq (type-of order-by) 'cons) (cadr order-by) 'asc))
         (limit (alist-get 'limit alist))
         (query nil))
    (setq query (format "SELECT %s FROM %s" columns table))
    (if order-by (setq query (concat query (format " ORDER BY %s %s" order-by-column order-by-sort))))
    (if limit (setq query (concat query (format " LIMIT %s" limit))))
    query))

(defun my/sql-select (alist)
  "Evaluates a SELECT command in the interactive SQL buffer."
  (sql-send-string (my/sql-query alist)))
