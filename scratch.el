;; Scratch buffer for testing out code
;;
                                        ;

;; Fix this test case. When point is at:
;;     fo|o
;; my/eval-dwim and my/eval-here don't work.
(setq-local foo '(1 2 42 42 3 4))
foo
;;


;; Fix this test case. When point is at:
;;     '(1 2 |42 42 3 4)
;; my/wrap-sexp wraps the sexp prior to the current one.
(setq-local foo '(1 2 42 42 3 4))
;;
