;; A record of bugs.
;;
;;


;; When point is at:
;;   fo|o
;; my/eval-here doesn't work properly. It should eval the symbol foo.
(setq-local foo '(1 2 3))
foo
;;


;; When point is at:
;;   '(1 2 |42 42 3 4)
;; my/wrap-sexp wraps the sexp prior to the current one.
(setq-local foo '(1 2 42 42 3 4))
;;


;; When point is at:
;;   (|setq-local foo '(1 2 42 42 3 4))
;; my/wrap-sexp should wrap setq-local, not the entire list.
(setq-local foo '(1 2 42 42 3 4))
;;


;; When point is at
;;   (looking-back "[[:blan|k:]]")
;; my/open-new-round doesn't work.
(looking-back "[[:blank:]]")
;;
