;;; babashka.el -*- lexical-binding: t; -*-
;; Babashka — Clojure-compatible scripting
;;
;; Babashka book: https://book.babashka.org/
;; CIDER + Babashka: https://docs.cider.mx/cider/platforms/babashka.html
;; ob-babashka: https://github.com/coatl-dev/ob-babashka

;;; Code:

;; Treat .bb scripts as Clojure (activates clojure-mode, LSP, paredit, etc.)
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))

;; ob-babashka: run Babashka code blocks in org-mode
(use-package ob-babashka
  :after org
  :config (ob-babashka-activate))

;; To connect CIDER to a running Babashka nREPL:
;;   Start it with: bb --nrepl-server PORT
;;   Then: M-x cider-connect
(after! cider
  (setq cider-babashka-parameters "--nrepl-server"))

(provide 'babashka-config)
;;; babashka.el ends here
