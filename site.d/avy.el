;;; avy.el --- Avy hydra actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Niten
;;
;; Author: Niten <niten@fudo.org>
;; Keywords: avy, emacs, hydra, navigation, text manipulation, keybindings
;;
;;; Commentary:
;;
;; Extra avy dispatch actions that act on a whole "thing" at the target.
;; Start any avy jump (e.g. `C-.' / `C-c .' = `avy-goto-char-timer'), and
;; once the candidate labels appear, press a dispatch key, then the
;; candidate's label.  A hydra then asks which thing to act on:
;;
;;   c  comment   C  copy   K  kill   m  move (to point)   r  mark/region
;;   S  swap (with the thing at point)   y  yank (copy to point)
;;   H  helpful docs for the symbol at the target
;;
;; e.g. `C-.' e  K  f  w  kills the word under candidate "f".
;; avy's own dispatch keys (x X t n Y i z, `?' for help) still work.
;;
;; The action functions are named `avy-action-fudo-*': avy ships its own
;; `avy-action-copy'/`avy-action-yank', and `?' help strips "avy-action-".
;;
;;; Code:

(require 'cl-lib)

(defvar fudo--avy-hydra-point nil
  "Buffer position chosen by Avy, consumed by the next hydra head.")

(defun fudo--avy-bounds (thing)
  "Return (START . END) of THING at point, or signal a `user-error'."
  (or (bounds-of-thing-at-point thing)
      (user-error "No %s here" thing)))

(defun fudo--avy-return ()
  "Select the window Avy was started from."
  (select-window (cdr (ring-ref avy-ring 0))))

(defun fudo--avy-do (f)
  "Call F with the position Avy saved for the current hydra, then forget it."
  (let ((pt fudo--avy-hydra-point))
    (setq fudo--avy-hydra-point nil)
    (unless pt
      (user-error "No avy target; start from an avy jump"))
    (funcall f pt)))

;;; Actions on THING at PT.  Each returns a function of PT for `fudo--avy-do'.

(defun fudo--avy-kill (thing)
  "Return a function that kills the THING at PT and returns to the origin."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (fudo--avy-bounds thing)
        (kill-region start end)))
    (fudo--avy-return)))

(defun fudo--avy-copy (thing)
  "Return a function that copies the THING at PT and returns to the origin."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (fudo--avy-bounds thing)
        (kill-new (buffer-substring-no-properties start end))))
    (fudo--avy-return)))

(defun fudo--avy-yank (thing)
  "Return a function that copies the THING at PT and yanks it at the origin."
  (let ((copy (fudo--avy-copy thing)))
    (lambda (pt)
      (funcall copy pt)
      (yank))))

(defun fudo--avy-move (thing)
  "Return a function that kills the THING at PT and yanks it at the origin."
  (let ((kill (fudo--avy-kill thing)))
    (lambda (pt)
      (funcall kill pt)
      (yank))))

(defun fudo--avy-comment (thing)
  "Return a function that comments the THING at PT and returns to the origin."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (fudo--avy-bounds thing)
        (comment-region start end)))
    (fudo--avy-return)))

(defun fudo--avy-mark (thing)
  "Return a function that moves to the THING at PT and marks it.
Unlike the others this deliberately leaves point (and the region) there."
  (lambda (pt)
    (goto-char pt)
    (cl-destructuring-bind (start . end) (fudo--avy-bounds thing)
      (push-mark start nil t)
      (goto-char end))))

(defun fudo--avy-mark-to-point (pt)
  "Mark the region from point (where Avy started) to PT."
  (push-mark (point) nil t)
  (goto-char pt))

(defun fudo--avy-swap (thing)
  "Return a function that swaps the THING at point with the THING at PT."
  (lambda (pt)
    (fudo--avy-return)
    ;; Markers, so the second region is still right after editing the first.
    (cl-destructuring-bind (here-start . here-end) (fudo--avy-bounds thing)
      (let* ((here-start (copy-marker here-start))
             (here-end (copy-marker here-end t))
             (there (save-excursion
                      (goto-char pt)
                      (let ((b (fudo--avy-bounds thing)))
                        (cons (copy-marker (car b)) (copy-marker (cdr b) t)))))
             (here-text (buffer-substring here-start here-end))
             (there-text (buffer-substring (car there) (cdr there))))
        (save-excursion
          (goto-char (car there))
          (delete-region (car there) (cdr there))
          (insert here-text)
          (goto-char here-start)
          (delete-region here-start here-end)
          (insert there-text))))))

;;; Hydras.  Heads are sexps, so hydra wraps them in commands.

(defhydra hydra-action-kill (:color blue :hint nil)
  "Avy kill"
  ("k" (fudo--avy-do #'avy-action-kill-stay)           "kill and stay")
  ("d" (fudo--avy-do (fudo--avy-kill 'defun))     "defun")
  ("e" (fudo--avy-do (fudo--avy-kill 'email))     "email")
  ("f" (fudo--avy-do (fudo--avy-kill 'filename))  "filename")
  ("l" (fudo--avy-do (fudo--avy-kill 'line))      "line")
  ("p" (fudo--avy-do (fudo--avy-kill 'paragraph)) "paragraph")
  ("s" (fudo--avy-do (fudo--avy-kill 'sentence))  "sentence")
  ("S" (fudo--avy-do (fudo--avy-kill 'sexp))      "sexp")
  ("u" (fudo--avy-do (fudo--avy-kill 'url))       "url")
  ("w" (fudo--avy-do (fudo--avy-kill 'word))      "word"))

(defhydra hydra-action-copy (:color blue :hint nil)
  "Avy copy"
  ("d" (fudo--avy-do (fudo--avy-copy 'defun))     "defun")
  ("e" (fudo--avy-do (fudo--avy-copy 'email))     "email")
  ("f" (fudo--avy-do (fudo--avy-copy 'filename))  "filename")
  ("l" (fudo--avy-do (fudo--avy-copy 'line))      "line")
  ("p" (fudo--avy-do (fudo--avy-copy 'paragraph)) "paragraph")
  ("s" (fudo--avy-do (fudo--avy-copy 'sentence))  "sentence")
  ("S" (fudo--avy-do (fudo--avy-copy 'sexp))      "sexp")
  ("u" (fudo--avy-do (fudo--avy-copy 'url))       "url")
  ("w" (fudo--avy-do (fudo--avy-copy 'word))      "word"))

(defhydra hydra-action-yank (:color blue :hint nil)
  "Avy yank"
  ("d" (fudo--avy-do (fudo--avy-yank 'defun))     "defun")
  ("e" (fudo--avy-do (fudo--avy-yank 'email))     "email")
  ("f" (fudo--avy-do (fudo--avy-yank 'filename))  "filename")
  ("l" (fudo--avy-do (fudo--avy-yank 'line))      "line")
  ("p" (fudo--avy-do (fudo--avy-yank 'paragraph)) "paragraph")
  ("s" (fudo--avy-do (fudo--avy-yank 'sentence))  "sentence")
  ("S" (fudo--avy-do (fudo--avy-yank 'sexp))      "sexp")
  ("u" (fudo--avy-do (fudo--avy-yank 'url))       "url")
  ("w" (fudo--avy-do (fudo--avy-yank 'word))      "word"))

(defhydra hydra-action-move (:color blue :hint nil)
  "Avy move"
  ("d" (fudo--avy-do (fudo--avy-move 'defun))     "defun")
  ("e" (fudo--avy-do (fudo--avy-move 'email))     "email")
  ("f" (fudo--avy-do (fudo--avy-move 'filename))  "filename")
  ("l" (fudo--avy-do (fudo--avy-move 'line))      "line")
  ("p" (fudo--avy-do (fudo--avy-move 'paragraph)) "paragraph")
  ("s" (fudo--avy-do (fudo--avy-move 'sentence))  "sentence")
  ("S" (fudo--avy-do (fudo--avy-move 'sexp))      "sexp")
  ("u" (fudo--avy-do (fudo--avy-move 'url))       "url")
  ("w" (fudo--avy-do (fudo--avy-move 'word))      "word"))

(defhydra hydra-action-comment (:color blue :hint nil)
  "Avy comment"
  ("d" (fudo--avy-do (fudo--avy-comment 'defun))     "defun")
  ("l" (fudo--avy-do (fudo--avy-comment 'line))      "line")
  ("p" (fudo--avy-do (fudo--avy-comment 'paragraph)) "paragraph")
  ("S" (fudo--avy-do (fudo--avy-comment 'sexp))      "sexp")
  ("w" (fudo--avy-do (fudo--avy-comment 'word))      "word"))

(defhydra hydra-action-region (:color blue :hint nil)
  "Avy mark region"
  ("j" (fudo--avy-do #'fudo--avy-mark-to-point)  "to-point")
  ("e" (fudo--avy-do (fudo--avy-mark 'email))     "email")
  ("f" (fudo--avy-do (fudo--avy-mark 'filename))  "filename")
  ("l" (fudo--avy-do (fudo--avy-mark 'line))      "line")
  ("p" (fudo--avy-do (fudo--avy-mark 'paragraph)) "paragraph")
  ("s" (fudo--avy-do (fudo--avy-mark 'sentence))  "sentence")
  ("S" (fudo--avy-do (fudo--avy-mark 'sexp))      "sexp")
  ("u" (fudo--avy-do (fudo--avy-mark 'url))       "url")
  ("w" (fudo--avy-do (fudo--avy-mark 'word))      "word"))

(defhydra hydra-action-swap (:color blue :hint nil)
  "Avy swap with thing at point"
  ("l" (fudo--avy-do (fudo--avy-swap 'line))      "line")
  ("p" (fudo--avy-do (fudo--avy-swap 'paragraph)) "paragraph")
  ("s" (fudo--avy-do (fudo--avy-swap 'sentence))  "sentence")
  ("S" (fudo--avy-do (fudo--avy-swap 'sexp))      "sexp")
  ("w" (fudo--avy-do (fudo--avy-swap 'word))      "word"))

;;; Dispatch actions: avy calls these with the target position.

(defun avy-action-fudo-comment (pt)
  "Pick a thing at PT to comment."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-comment/body))

(defun avy-action-fudo-copy (pt)
  "Pick a thing at PT to copy."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-copy/body))

(defun avy-action-fudo-kill (pt)
  "Pick a thing at PT to kill."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-kill/body))

(defun avy-action-fudo-move (pt)
  "Pick a thing at PT to move to point."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-move/body))

(defun avy-action-fudo-region (pt)
  "Pick a thing at PT to mark."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-region/body))

(defun avy-action-fudo-swap (pt)
  "Pick a thing at PT to swap with the same thing at point."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-swap/body))

(defun avy-action-fudo-yank (pt)
  "Pick a thing at PT to copy to point."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-yank/body))

(declare-function helpful-at-point "helpful")

(defun avy-action-fudo-helpful (pt)
  "Show `helpful' documentation for the symbol at PT."
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  t)

;; `avy-dispatch-alist' doesn't exist until avy loads.  These keys must not
;; be in `avy-keys' (default: the home row).  avy's own `z' (zap) is kept.
(after! avy
  (setf (alist-get ?c avy-dispatch-alist) #'avy-action-fudo-comment
        (alist-get ?C avy-dispatch-alist) #'avy-action-fudo-copy
        (alist-get ?H avy-dispatch-alist) #'avy-action-fudo-helpful
        (alist-get ?K avy-dispatch-alist) #'avy-action-fudo-kill
        (alist-get ?m avy-dispatch-alist) #'avy-action-fudo-move
        (alist-get ?r avy-dispatch-alist) #'avy-action-fudo-region
        (alist-get ?S avy-dispatch-alist) #'avy-action-fudo-swap
        (alist-get ?y avy-dispatch-alist) #'avy-action-fudo-yank))

(provide 'avy-config)
;;; avy.el ends here
