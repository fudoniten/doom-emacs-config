;;; lisp.el -*- lexical-binding: t; -*-
;;
;; Common settings for Lisp modes
;; Keywords: lisp, configuration, emacs, hooks, paredit, scheme, clojure, racket

;;; Code:

(defvar *lisp-mode-hooks* '(scheme-mode-hook
                            lisp-mode-hook
                            emacs-lisp-mode-hook
                            clojure-mode-hook
                            racket-mode-hook
                            clojurescript-mode-hook))

(defun bind-hook-to-modes (hook modes)
  "Bind HOOK to all MODES."
  (dolist (mode modes)
    (add-hook mode hook)))

(defun my-lisp-mode-hook ()
  (paredit-mode 1))

(bind-hook-to-modes 'my-lisp-mode-hook *lisp-mode-hooks*)

(provide 'lisp-config)
;;; lisp.el ends here
