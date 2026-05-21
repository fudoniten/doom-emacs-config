;;; clojure.el -*- lexical-binding: t; -*-
;; Clojure configuration

;;; Code:

;; Use paredit exclusively — disable smartparens in all Clojure modes
(after! smartparens
  (dolist (hook '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook))
    (add-hook hook (lambda () (smartparens-mode -1)))))

;; Ensure paredit in ClojureC too (clojurec-mode-hook was missing from lisp.el)
(add-hook 'clojurec-mode-hook #'paredit-mode)

;; CIDER jack-in
(after! cider
  (map! :leader
        :desc "CIDER jack-in"       "o c" #'cider-jack-in
        :desc "CIDER jack-in (cljs)" "o C" #'cider-jack-in-cljs))

(provide 'clojure-config)
;;; clojure.el ends here
