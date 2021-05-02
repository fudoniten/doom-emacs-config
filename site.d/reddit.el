;;; reddit.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Niten
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(when (require 'md4rd nil 'noerror)

  (setq md4rd-subs-active
        '(academicbiblical
          askhistorian
          askreddit
          askscience
          bitcoin
          changemyview
          clojure
          common_lisp
          compsci
          cryptocurrency
          emacs
          fire
          futurelings
          guile
          guix
          ipfs
          learnprogramming
          linux
          lisp
          neutralpolitics
          nixos
          outoftheloop
          personalfinance
          politics
          programming
          racket
          science
          todayilearned
          unpopularopinion
          worldnews))

  (defun consider-refresh-md4rd-login ()
    (when (and (boundp 'md4rd--oauth-client-id)
               (boundp 'md4rd--oauth-access-token)
               (boundp 'md4rd--oauth-refresh-token))
      (md4rd-refresh-login)))

  (run-with-timer 0 3540 'consider-refresh-md4rd-login)

  (add-hook 'md4rd-mode-hook 'md4rd-indent-all-the-lines))

(provide 'reddit)
;;; reddit.el ends here
