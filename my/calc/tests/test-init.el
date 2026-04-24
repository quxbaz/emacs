;; -*- lexical-binding: t; -*-
;;
;; Bootstrap for batch test runs. Load before test files:
;;   emacs --batch -Q \
;;     --eval "(setq user-emacs-directory \"/home/david/.emacs.d/\")" \
;;     -l my/calc/tests/test-init.el \
;;     -l my/calc/tests/my-foo-tests.el \
;;     -f ert-run-tests-batch-and-exit

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(require 'package)
(package-initialize)
(require 'dash)
(require 's)
