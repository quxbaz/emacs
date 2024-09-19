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

(defun my/sql-send-physical-command (command)
  "Enters input in an interactive SQL buffer and submits it. This
function is used internally."
  (when (eq major-mode 'sql-interactive-mode)
    (end-of-buffer)
    (comint-kill-input)
    (insert command)
    (comint-send-input)))

(defun my/sql-physical-eval (command)
  "Executes COMMAND by visiting the interactive SQL buffer,
inserting the command, and simulating <return>.

Unlike `sql-send-string`, this function actually navigates to the
interactive SQL buffer and inputs COMMAND manually, which can
lead to better formatting and handling of SQL output."
  (if (not (string-suffix-p ";" command))
      (setq command (concat command ";")))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (eq major-mode 'sql-interactive-mode)
          (my/sql-send-physical-command command)))))

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
    (concat query ";")))

(defun my/sql-select (alist)
  "Evaluates a SELECT command in the interactive SQL buffer."
  (my/sql-physical-eval (my/sql-query alist)))
