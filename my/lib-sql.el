;; SQL library
;;
;; Collection of functions for interacting with SQL inside buffers; or really
;; anything SQL-related.
;;
;; TODO
;; INSERT INTO, UPDATE, DELETE, CREATE TABLE, ALTER TABLE, DROP TABLE,
;; [BEGIN, COMMIT], ROLLBACK


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
         (table (car (alist-get 'table alist)))
         (where (car (alist-get 'where alist)))
         (order-by (alist-get 'order-by alist))
         (order-by-column (car order-by))
         (order-by-sort (cadr order-by))
         (limit (car (alist-get 'limit alist))))
    (concat (format "SELECT %s FROM %s" (mapconcat 'symbol-name columns ", ") table)
            (if order-by-column (format " ORDER BY %s" order-by-column))
            (if order-by-sort (format " %s" (upcase (symbol-name order-by-sort))))
            (if limit (format " LIMIT %s" limit))
            ";")))

(defun my/sql-select (alist)
  "Evaluates a SELECT command in the interactive SQL buffer."
  (my/sql-physical-eval (my/sql-query alist)))
