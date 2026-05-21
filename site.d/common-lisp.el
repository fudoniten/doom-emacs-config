;;; common-lisp.el -*- lexical-binding: t; -*-
;; Common Lisp (SLY) configuration
;;
;; SLY docs: https://joaotavora.github.io/sly/
;; sly-quicklisp: https://github.com/joaotavora/sly-quicklisp
;; sly-asdf: https://github.com/mmgeorge/sly-asdf

;;; Code:

;; Configure SLY's lisp implementations.
;; Note: inferior-lisp-program is the old SLIME setting; SLY uses
;; sly-lisp-implementations instead.
(after! sly
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "2048"))))
  (setq sly-default-lisp 'sbcl))

;; sly-quicklisp: Quicklisp integration (ql:quickload etc. from SLY)
(use-package sly-quicklisp
  :after sly)

;; sly-asdf: ASDF system definition integration
(use-package sly-asdf
  :after sly)

;; SLY keybindings via local leader (C-c l in this config)
(after! sly
  (map! :map sly-mode-map
        :localleader
        :desc "SLY REPL"        "r" #'sly-mrepl
        :desc "Inspect"         "i" #'sly-inspect
        :desc "Describe symbol" "d" #'sly-describe-symbol
        :desc "Compile defun"   "c" #'sly-compile-defun
        :desc "Compile file"    "C" #'sly-compile-file
        :desc "Load file"       "l" #'sly-load-file
        :desc "Stickers"        "s" #'sly-stickers-dwim
        :desc "Macroexpand"     "m" #'sly-macroexpand-1
        :desc "Who calls"       "w" #'sly-who-calls))

(provide 'common-lisp-config)
;;; common-lisp.el ends here
