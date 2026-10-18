;;; lisp.el -*- lexical-binding: t; -*-
;;
;; Common settings for Lisp modes: paredit everywhere, plus its bindings.
;; Keywords: lisp, configuration, emacs, hooks, paredit, scheme, clojure, racket

;;; Code:

(defvar *lisp-mode-hooks* '(scheme-mode-hook
                            lisp-mode-hook
                            emacs-lisp-mode-hook
                            clojure-mode-hook
                            clojurec-mode-hook
                            clojurescript-mode-hook
                            cider-repl-mode-hook
                            racket-mode-hook
                            sly-mrepl-mode-hook)
  "Hooks of the modes that get `paredit-mode'.")

(dolist (hook *lisp-mode-hooks*)
  (add-hook hook #'paredit-mode))

(after! paredit
  (map! :map paredit-mode-map
        "M-("   #'paredit-wrap-round
        "M-)"   #'paredit-close-round
        "C-c }" #'paredit-forward-barf-sexp
        "C-c {" #'paredit-backward-barf-sexp
        "C-}"   #'paredit-forward-barf-sexp
        "C-{"   #'paredit-backward-barf-sexp
        "C-c )" #'paredit-forward-slurp-sexp
        "C-c (" #'paredit-backward-slurp-sexp
        "C-)"   #'paredit-forward-slurp-sexp
        "C-("   #'paredit-backward-slurp-sexp))

(provide 'lisp-config)
;;; lisp.el ends here
