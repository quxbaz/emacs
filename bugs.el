;; A record of bugs.
;;
;;


;; When point is at:
;;   fo|o
;; my/eval-here doesn't work properly. It should eval the symbol foo.
(setq-local foo '(1 2 3))
foo
;;
