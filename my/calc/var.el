;; -*- lexical-binding: t; -*-
;;
;; Variable declarations


(defvar-local my/calc-edit-saved-point nil
  "Variable used to restore point in some cases after exiting edit-mode.")

(defvar-local my/calc-stored-stack nil
  "Used in my/calc-store-stack and my/calc-recall-stack.")

(provide 'my/calc/var)
