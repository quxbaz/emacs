;; Local Variables:
;; eval: (sql-lisp-mode)
;; End:

(sql-connect "wnmu-edu-db")
(my/sql-physical-eval "help")
(my/sql-physical-eval "status")
(my/sql-show-databases)
(my/sql-show-tables)

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

(sql-send-string "SELECT * FROM wp_10_postmeta LIMIT 10")
