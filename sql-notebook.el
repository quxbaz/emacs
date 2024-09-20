;; Local Variables:
;; eval: (sql-lisp-mode)
;; End:

(sql-connect "wnmu-edu-db")
(my/sql-physical-eval "help")
(my/sql-physical-eval "status")
(my/sql-physical-eval "SHOW DATABASES")
(my/sql-physical-eval "SHOW TABLES")

(my/sql-query '((table wp_10_postmeta)
                (columns *)
                (where nil)
                (order-by meta_key)
                (limit 10)))

(my/sql-query '((table wp_10_postmeta)
                (columns meta_id meta_key)
                (where nil)
                (order-by meta_key)
                (limit 10)))

(my/sql-select '((table wp_10_postmeta)
                 (columns meta_id meta_key)
                 (where nil)
                 (order-by meta_id desc)
                 (limit 10)))

(my/sql-select '((table wp_10_postmeta)
                 (columns *)
                 (where nil)
                 (order-by meta_key)
                 (limit 10)))

(my/sql-select '((table wp_10_postmeta)
                 (columns meta_id meta_key)
                 (limit 10)))

(my/sql-insert '((table)
                 (values (COLUMN VALUE)
                         (COLUMN VALUE))))

(my/sql-physical-eval "SELECT * FROM wp_10_postmeta LIMIT 10")
(my/sql-physical-eval "SELECT * FROM wp_10_postmeta WHERE meta_id=9999999")
(my/sql-physical-eval "DELETE FROM wp_10_postmeta WHERE meta_id=9999999")
(my/sql-physical-eval "INSERT INTO wp_10_postmeta (meta_id) VALUES (9999999)")
