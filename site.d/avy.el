;;; avy.el --- Avy hydra actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Niten
;;
;; Author: Niten <niten@fudo.org>
;; Maintainer: Niten <niten@fudo.org>
;; Created: June 08, 2025
;; Modified: June 08, 2025
;; Version: 0.0.1
;; Keywords: avy, emacs, hydra, navigation, text manipulation, keybindings
;; Homepage: https://github.com/niten/avy
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Avy hydra actions for enhanced navigation and text manipulation
;;
;;; Code:

(defvar fudo--avy-hydra-func nil
  "A variable to hold the command to be executed by Avy.")

(defun kill-thing-at-point (thing)
  "Return a lambda to kill the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (kill-region start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t))

(defun move-thing-at-point (thing)
  "Return a lambda to move the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (kill-region start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    (yank)
    t))

(defun fudo--avy-wrap-action (f)
  "Take a function F and call it with the point specified by Avy."
  (message "calling with point %s!" fudo--avy-hydra-point)
  (funcall f fudo--avy-hydra-point)
  (setq fudo--avy-hydra-point nil))

(defun copy-thing-at-point (thing)
  "Return a lambda to copy the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (let ((obj (buffer-substring-no-properties start end)))
          (kill-new obj))))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t))

(defun yank-thing-at-point (thing)
  "Return a lambda to yank the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (let ((obj (buffer-substring-no-properties start end)))
          (kill-new obj))))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    (yank)
    t))

(defun comment-thing-at-point (thing)
  "Return a lambda to comment the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (comment-region start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t))

(defun mark-thing-at-point (thing)
  "Return a lambda to mark the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (push-mark start nil t)
        (goto-char end)
        (activate-mark)))
    t))

(defun swap-thing-at-point (thing)
  "Return a lambda to swap the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (let ((start (point-marker))
            (src)
            (tgt))
        (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
          (kill-region start end)
          (setq src (current-kill 0)))
        (goto-char pt)
        (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
          (kill-region start end)
          (setq tgt (current-kill 0))
          (insert src))
        (goto-char start)
        (insert tgt)))))

(defhydra hydra-action-kill (:color red :hint nil)
  "Avy kill"
  ("k" (lambda () (avy-action-kill-stay fudo--avy-hydra-point))      "kill and stay")
  ("d" (fudo--avy-wrap-action (kill-thing-at-point 'defun))     "defun")
  ("e" (fudo--avy-wrap-action (kill-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (kill-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (kill-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (kill-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (kill-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (kill-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (kill-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (kill-thing-at-point 'word))      "word"))

(defhydra hydra-action-copy (:color green :hint nil)
  "Avy copy"
  ("d" (fudo--avy-wrap-action (copy-thing-at-point 'defun))     "defun")
  ("e" (fudo--avy-wrap-action (copy-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (copy-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (copy-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (copy-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (copy-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (copy-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (copy-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (copy-thing-at-point 'word))      "word"))

(defhydra hydra-action-yank (:color green :hint nil)
  "Avy yank"
  ("d" (fudo--avy-wrap-action (yank-thing-at-point 'defun))     "defun")
  ("e" (fudo--avy-wrap-action (yank-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (yank-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (yank-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (yank-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (yank-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (yank-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (yank-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (yank-thing-at-point 'word))      "word"))

(defhydra hydra-action-move (:color green :hint nil)
  "Avy move"
  ("d" (fudo--avy-wrap-action (move-thing-at-point 'defun))     "defun")
  ("e" (fudo--avy-wrap-action (move-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (move-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (move-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (move-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (move-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (move-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (move-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (move-thing-at-point 'word))      "word"))

(defhydra hydra-action-comment (:color green :hint nil)
  "Avy comment"
  ("d" (fudo--avy-wrap-action (comment-thing-at-point 'defun))     "defun")
  ("l" (fudo--avy-wrap-action (comment-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (comment-thing-at-point 'paragraph)) "paragraph")
  ("S" (fudo--avy-wrap-action (comment-thing-at-point 'sexp))      "sexp")
  ("w" (fudo--avy-wrap-action (comment-thing-at-point 'word))      "word"))

(defun set-region (start end)
  "Set the region from START to END."
  (push-mark start nil t)
  (goto-char end)
  (activate-mark)
  t)

(defhydra hydra-action-region (:color green :hint nil)
  "Avy mark region"
  ("j" (fudo--avy-wrap-action (lambda (pt) (set-region (point) pt))) "to-point")
  ("e" (fudo--avy-wrap-action (mark-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (mark-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (mark-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (mark-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (mark-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (mark-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (mark-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (mark-thing-at-point 'word))      "word"))

(defhydra hydra-action-swap (:color green :hint nil)
  "Avy swap object"
  ("l" (fudo--avy-wrap-action (swap-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (swap-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (swap-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (swap-thing-at-point 'sexp))      "sexp")
  ("w" (fudo--avy-wrap-action (swap-thing-at-point 'word))      "word"))

(defun fudo--avy-call-with-point (f)
  "Return a lambda to call function F with the point specified by Avy."
  (lambda (pt)
    (message "Saving point: %s" pt)
    (setq fudo--avy-hydra-point pt)
    (funcall f)))

(defun zap-to-point (pt)
  "Kill the region from point to PT."
  (kill-region (point) pt))

(defun avy-action-comment (pt)
  "Invoke the comment hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-comment/body))

(defun avy-action-copy (pt)
  "Invoke the copy hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-copy/body))

(defun avy-action-kill (pt)
  "Invoke the kill hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-kill/body))

(defun avy-action-move (pt)
  "Invoke the move hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-move/body))

(defun avy-action-region (pt)
  "Invoke the region hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-region/body))

(defun avy-action-swap (pt)
  "Invoke the swap hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-swap/body))

(defun avy-action-yank (pt)
  "Invoke the yank hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-yank/body))

(setf (alist-get ?c avy-dispatch-alist) #'avy-action-comment
      (alist-get ?C avy-dispatch-alist) #'avy-action-copy
      (alist-get ?H avy-dispatch-alist) #'helpful-at-point
      (alist-get ?K avy-dispatch-alist) #'avy-action-kill
      (alist-get ?m avy-dispatch-alist) #'avy-action-move
      (alist-get ?r avy-dispatch-alist) #'avy-action-region
      (alist-get ?S avy-dispatch-alist) #'avy-action-swap
      (alist-get ?y avy-dispatch-alist) #'avy-action-yank
      (alist-get ?z avy-dispatch-alist) #'zap-to-point)

(provide 'avy-menus)
;;; avy.el ends here
