;; -*- lexical-binding: t; -*-
;;
;; Variable declarations


(defvar-local my/calc-edit-saved-point nil
  "Variable used to restore point in some cases after exiting edit-mode.")

(defvar-local my/calc-saved-stack nil
  "Used in my/calc-store-stack and my/calc-restore-stack.")

(provide 'my/calc/var)
