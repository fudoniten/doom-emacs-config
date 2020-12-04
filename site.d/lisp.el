;;; lisp.el -*- lexical-binding: t; -*-
;;
;; Common settings for Lisp modes

(defvar *lisp-mode-hooks* '(scheme-mode-hook
                            lisp-mode-hook
                            emacs-lisp-mode-hook
                            clojure-mode-hook
                            racket-mode-hook
                            clojure-mode-hook
                            clojurescript-mode-hook))

(defun my-lisp-mode-hook ()
  (paredit-mode 1)
  (local-set-key (kbd "C-c }") 'paredit-forward-barf-sexp)
  (local-set-key (kbd "C-c {") 'paredit-backward-barf-sexp)
  (local-set-key (kbd "C-}") 'paredit-forward-barf-sexp)
  (local-set-key (kbd "C-{") 'paredit-backward-barf-sexp)
  (local-set-key (kbd "C-c )") 'paredit-forward-slurp-sexp)
  (local-set-key (kbd "C-c (") 'paredit-backward-slurp-sexp)
  (local-set-key (kbd "C-)") 'paredit-forward-slurp-sexp)
  (local-set-key (kbd "C-(") 'paredit-backward-slurp-sexp))

(bind-hook-to-modes 'my-lisp-mode-hook *lisp-mode-hooks*)

;;
