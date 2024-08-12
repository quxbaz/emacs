;; My js library
;;
;;


(defun my/find-jsx ()
  "Finds all js[x] files starting from the current directory."
  (interactive)
  (find-dired "." (concat "! -regex './node_modules/.*' "
                          "! -regex './.next/.*' "
                          "-regex './.*.jsx?'")))

(defun my/comment-jsx (arg)
  (interactive "p")
  (let ((is-empty-line (my/is-line-empty)))
    (save-excursion
      (if (use-region-p)
          (let ((start (region-beginning))
                (end (region-end)))
            (goto-char end)
            (if (not (my/is-line-empty)) (insert " "))
            (insert "*/}")
            (goto-char start)
            (insert "{/* "))
        (beginning-of-line-text)
        (insert "{/* ")
        (end-of-line)
        (insert " */}")))
    (if (and is-empty-line (not (use-region-p)))
        (goto-char (+ (point) 4))))
  (message "** Comment JSX **"))

(defun my/uncomment-jsx (arg)
  (interactive "p")
  (if (use-region-p)
      (progn
        (save-excursion
          (let ((beginning (region-beginning))
                (end (region-end)))
            (replace-regexp "{/\\* " "" nil beginning end)
            (replace-regexp " \?\\*/}" "" nil beginning end))))
    (save-excursion
      (replace-regexp "{/\\* " "" nil (point-at-bol) (point-at-eol))
      (replace-regexp " \\*/}" "" nil (point-at-bol) (point-at-eol))))
  (message "** Uncomment JSX **"))

(defun my/toggle-jsx-comment (arg)
  (interactive "p")
  (let ((start-pos (if (use-region-p)
                       (region-beginning)
                     (save-excursion
                       (beginning-of-line-text) (point)))))
    (if (and (string= (my/string-at start-pos 0) "{")
             (string= (my/string-at start-pos 1) "/")
             (string= (my/string-at start-pos 2) "\*")
             (string= (my/string-at start-pos 3) " "))
        (my/uncomment-jsx arg)
      (my/comment-jsx arg))))
