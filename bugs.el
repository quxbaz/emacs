;; A record of bugs.
;;
;;


;; When point is at:
;;   fo|o
;; my/eval-here doesn't work properly. It should eval the symbol foo.
(setq-local foo '(1 2 3))
foo
;;


;; When point is at
;;   (looking-back "[[:blan|k:]]")
;; my/open-new-round doesn't work.
(looking-back "[[:blank:]]")
;;
