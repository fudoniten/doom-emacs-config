;;; ruby.el -*- lexical-binding: t; -*-

;; (rx) lessons learned:
;;  - you can insert (rx) output in another (rx) with (eval rx-out)
(add-to-list 'hs-special-modes-alist
             `(ruby-mode
               ,(rx (or "{"
                        (seq bol
                             (zero-or-more (syntax whitespace))
                             "do"
                             (opt (seq "|"
                                       (one-or-more (or alphanumeric "," " " "_"))
                                       "|")))
                        (seq bol
                             (zero-or-more (syntax whitespace))
                             (or "def" "while" "class" "module" "case" "if")
                             " "
                             (one-or-more not-newline))))
               ,(rx (or "}"
                        (seq bol
                             (zero-or-more (syntax whitespace))
                             "end")))
               ,(rx (or "#" "=begin")) ; Comment start
               ruby-forward-sexp nil))

;; Configure Smart-Parens
; Only want this for non-lisp languages
;(smartparens-global-mode)
;(show-smartparens-global-mode)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))
