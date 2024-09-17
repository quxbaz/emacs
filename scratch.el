;; Scratch buffer for testing out code.
;;
;;

(defun my/sql-command (command)
  ""
  (if (memq command '(help status))
      (walk-windows (lambda (window)
                      (with-current-buffer (window-buffer window)
                        (when (eq major-mode 'sql-interactive-mode)
                          (end-of-buffer)
                          (comint-kill-input)
                          (insert (symbol-name command))
                          (comint-send-input)))))
    (error "Not a valid command: %s" command)))

(defun my/sql-eval (command)
  (sql-send-string command))

(defun my/sql-databases ()
  (sql-send-string "SHOW DATABASES;"))

(defun my/sql-tables ()
  (sql-send-string "SHOW TABLES;"))

(defun my/sql-select (alist)
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
    (my/sql-eval query)))

;;
;;

(sql-connect "wnmu-edu-db")
(my/sql-command 'help)
(my/sql-command 'status)
(my/sql-databases)
(my/sql-tables)

;; TODO: Create a snippet for this.
(my/sql-select '((columns . *)
                 (table . wp_10_postmeta)
                 (where . nil)
                 (order-by . meta_key)
                 (limit . 10)))

;; TODO: Support list in 'columns prop.
(my/sql-select '((columns . (meta_id meta_key))
                 (table . wp_10_postmeta)
                 (limit . 10)))

(prin1-to-string '(meta_id meta_key))

;; (my/sql-eval "SELECT * FROM wp_10_postmeta LIMIT 10")
