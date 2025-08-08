;; My string library
;;
;; String-related functions.


;; Source: https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
(defun my/string-match-all (regexp string)
  "Gets a list of all regexp matches in a string."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))
