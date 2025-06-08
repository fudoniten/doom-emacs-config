;;; lisp.el -*- lexical-binding: t; -*-
;;
;; Common settings for Lisp modes
;; Keywords: lisp, configuration, emacs, hooks, paredit, scheme, clojure, racket

(defvar *lisp-mode-hooks* '(scheme-mode-hook
                            lisp-mode-hook
                            emacs-lisp-mode-hook
                            clojure-mode-hook
                            racket-mode-hook
                            clojure-mode-hook
                            clojurescript-mode-hook))

(defun my-lisp-mode-hook ()
  (paredit-mode 1))
  
(bind-hook-to-modes 'my-lisp-mode-hook *lisp-mode-hooks*)

;;
