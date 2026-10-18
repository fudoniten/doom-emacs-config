;;; avy.el --- Avy verb/noun actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Niten
;;
;; Author: Niten <niten@fudo.org>
;; Keywords: avy, emacs, hydra, navigation, text manipulation, keybindings
;;
;;; Commentary:
;;
;; vi-style composition on top of avy: jump target, then VERB, then NOUN.
;;
;; Start any avy jump (e.g. `C-.' / `C-c .' = `avy-goto-char-timer').  When
;; the candidate labels appear, press a verb key, then the candidate's label;
;; a hydra then asks for the noun -- the thing at the target to act on.
;;
;;   C-. e  K f  w     kill the word under candidate "f"
;;   C-. (  y d  b     copy the (...) list under "d" to point
;;   C-. "  E a  Q     change the inside of the string under "a"
;;
;; Verbs (avy dispatch keys) and nouns are the two tables below; adding a
;; row to either one adds it to every menu.  avy's own dispatch keys
;; (x X t n Y i z, `?' for help) still work, except where a verb below
;; reuses the key (m, y).
;;
;; The dispatch functions are named `avy-action-fudo-*' so they can't
;; clash with avy's own `avy-action-*', and read well in avy's `?' help.
;;
;;; Code:

(require 'cl-lib)
(require 'thingatpt)

(eval-and-compile
  (defconst fudo--avy-verbs
    '((?K kill       "kill it; stay where you were")
      (?C copy       "copy it")
      (?y yank       "copy it to point")
      (?m move       "move it to point")
      (?p replace    "replace it with the latest kill")
      (?E change     "kill it and go there to type (vi c)")
      (?S swap       "swap it with the same thing at point")
      (?r mark       "go there and mark it")
      (?* mark-all   "multiple cursors on every occurrence")
      (?c comment    "comment/uncomment it")
      (?= indent     "reindent it")
      (?U upcase     "upcase it")
      (?L downcase   "downcase it")
      (?~ capitalize "capitalize it")
      (?e eval       "evaluate it"))
    "Avy dispatch keys: (CHAR VERB DESCRIPTION).
CHAR must not be in `avy-keys' (default: the home row).")

  (defconst fudo--avy-nouns
    '(("w" word          "word"          text)
      ("o" symbol        "symbol"        text)
      ("s" sentence      "sentence"      text)
      ("l" line          "line"          text)
      ("p" paragraph     "paragraph"     text)
      ("S" sexp          "sexp"          code)
      ("d" defun         "defun"         code)
      ("b" list          "(...)"         code)
      ("B" inner-list    "in (...)"      code)
      ("q" string        "\"...\""       code)
      ("Q" inner-string  "in \"...\""    code)
      (";" comment       "comment"       code)
      ("i" indent        "indent block"  block)
      ("I" indent-header "block+header"  block)
      ("n" number        "number"        other)
      ("u" url           "url"           other)
      ("e" email         "email"         other)
      ("f" filename      "filename"      other)
      ("$" to-eol        "to eol"        range)
      ("j" to-point      "to point"      range))
    "Hydra keys for the things a verb can act on: (KEY NOUN LABEL GROUP).
NOUN is a `thing-at-point' thing, or one handled in `fudo--avy-noun-bounds'.
GROUP is the row of the menu it's shown in.")

  (defconst fudo--avy-verb-excludes
    '((swap to-eol to-point))
    "Nouns that make no sense for a verb: (VERB NOUN...)."))

(defvar fudo--avy-hydra-point nil
  "Buffer position chosen by Avy, consumed by the next hydra head.")

;;; Nouns

(defun fudo--avy-shrink (bounds)
  "Return BOUNDS without its first and last character (vi's \"inner\")."
  (when bounds
    (cons (1+ (car bounds)) (1- (cdr bounds)))))

(defun fudo--avy-blank-line-p ()
  (save-excursion (beginning-of-line) (looking-at-p "[ \t]*$")))

(defun fudo--avy-indent-bounds (with-header)
  "Bounds of the lines around point indented at least as deep as this one.
Blank lines inside the block are included.  With WITH-HEADER, also include
the less-indented line that opens the block (e.g. a `def' or `if')."
  (save-excursion
    (beginning-of-line)
    (unless (fudo--avy-blank-line-p)
      (let ((indent (current-indentation))
            (start (line-beginning-position))
            (end (line-end-position))
            header)
        (save-excursion
          (catch 'done
            (while (zerop (forward-line -1))
              (cond ((fudo--avy-blank-line-p))
                    ((>= (current-indentation) indent)
                     (setq start (line-beginning-position)))
                    (t (setq header (line-beginning-position))
                       (throw 'done nil))))))
        (catch 'done
          (while (and (zerop (forward-line 1)) (not (eobp)))
            (cond ((fudo--avy-blank-line-p))
                  ((>= (current-indentation) indent)
                   (setq end (line-end-position)))
                  (t (throw 'done nil)))))
        (cons (if with-header (or header start) start)
              (min (point-max) (1+ end)))))))

(defun fudo--avy-comment-bounds ()
  "Bounds of the comment at point (delimiters included), or nil."
  (let* ((ppss (syntax-ppss))
         (start (cond ((nth 4 ppss) (nth 8 ppss))
                      ((and comment-start-skip (looking-at comment-start-skip))
                       (point)))))
    (when start
      (save-excursion
        (goto-char start)
        (forward-comment 1)
        (skip-chars-backward " \t\n")
        (cons start (point))))))

(defun fudo--avy-noun-bounds (noun origin)
  "Return (START . END) of NOUN at point.
ORIGIN is where avy was started, for the nouns that are ranges."
  (pcase noun
    ('inner-list    (fudo--avy-shrink (bounds-of-thing-at-point 'list)))
    ('inner-string  (fudo--avy-shrink (bounds-of-thing-at-point 'string)))
    ('indent        (fudo--avy-indent-bounds nil))
    ('indent-header (fudo--avy-indent-bounds t))
    ('comment       (fudo--avy-comment-bounds))
    ;; The stock `number' thing has no bounds function and falls back to
    ;; word-like bounds, so "list" counts as a number.
    ('number        (when (thing-at-point-looking-at
                           "\\(?:0x\\|#x\\)[[:xdigit:]]+\\|-?[0-9]+\\(?:\\.[0-9]+\\)?" 500)
                      (cons (match-beginning 0) (match-end 0))))
    ('to-eol        (cons (point) (line-end-position)))
    ('to-point      (cons (min (point) origin) (max (point) origin)))
    (_              (bounds-of-thing-at-point noun))))

(defun fudo--avy-noun-label (noun)
  (or (nth 2 (cl-find noun fudo--avy-nouns :key #'cadr)) noun))

;;; Verbs

(defun fudo--avy-return ()
  "Select the window Avy was started from."
  (select-window (cdr (ring-ref avy-ring 0))))

(declare-function +eval/region "ext:eval")
(declare-function mc/mark-all-like-this "ext:mc-mark-more")

(defun fudo--avy-edit (verb beg end)
  "Apply VERB to BEG..END in place (point is restored by the caller)."
  (pcase verb
    ((or 'kill 'move) (kill-region beg end))
    ((or 'copy 'yank) (kill-new (buffer-substring-no-properties beg end)))
    ('replace         (let ((text (current-kill 0 t)))
                        (goto-char beg)
                        (delete-region beg end)
                        (insert text)))
    ('comment         (comment-or-uncomment-region beg end))
    ('indent          (indent-region beg end))
    ('upcase          (upcase-region beg end))
    ('downcase        (downcase-region beg end))
    ('capitalize      (capitalize-region beg end))
    ('eval            (if (fboundp '+eval/region)
                          (+eval/region beg end)
                        (eval-region beg end t)))))

(defun fudo--avy-swap (beg end noun)
  "Swap BEG..END with the same NOUN at the point avy started from."
  (let ((there-text (buffer-substring beg end)))
    (fudo--avy-return)
    (pcase-let ((`(,here-beg . ,here-end)
                 (or (fudo--avy-noun-bounds noun (point))
                     (user-error "Nothing to swap with: no %s at point"
                                 (fudo--avy-noun-label noun)))))
      (when (and (eq (current-buffer) (marker-buffer beg))
                 (< here-beg end) (< beg here-end))
        (user-error "Those overlap"))
      ;; Markers, so the second region is still right after editing the first.
      (let ((here-beg (copy-marker here-beg))
            (here-end (copy-marker here-end t))
            (here-text (buffer-substring here-beg here-end)))
        (with-current-buffer (marker-buffer beg)
          (save-excursion
            (goto-char beg)
            (delete-region beg end)
            (insert here-text)))
        (save-excursion
          (goto-char here-beg)
          (delete-region here-beg here-end)
          (insert there-text))))))

(defun fudo--avy-apply (verb beg end noun)
  "Do VERB to the NOUN at BEG..END (markers in the target buffer)."
  (pcase verb
    ('mark     (push-mark beg nil t)
               (goto-char end))
    ('change   (kill-region beg end)
               (goto-char beg))
    ('mark-all (unless (require 'multiple-cursors nil t)
                 (user-error "mark-all needs multiple-cursors"))
               (push-mark beg nil t)
               (goto-char end)
               (mc/mark-all-like-this))
    ('swap     (fudo--avy-swap beg end noun))
    (_         (save-mark-and-excursion (fudo--avy-edit verb beg end))
               (fudo--avy-return)
               (when (memq verb '(yank move))
                 (yank)))))

(defun fudo--avy-run (verb noun)
  "Do VERB to the NOUN at the position avy saved for the current hydra."
  (let ((pt fudo--avy-hydra-point)
        (origin (ring-ref avy-ring 0)))
    (setq fudo--avy-hydra-point nil)
    (unless pt
      (user-error "No avy target; start from an avy jump"))
    (when (and (eq noun 'to-point)
               (not (eq (window-buffer (cdr origin)) (current-buffer))))
      (user-error "\"to point\" needs the target in the same buffer"))
    (pcase-let ((`(,beg . ,end)
                 (save-excursion
                   (goto-char pt)
                   (or (fudo--avy-noun-bounds noun (car origin))
                       (user-error "Nothing to act on: no %s at the target"
                                   (fudo--avy-noun-label noun))))))
      (fudo--avy-apply verb (copy-marker beg) (copy-marker end t) noun))))

;;; Generate one hydra and one avy dispatch action per verb.

(eval-and-compile
  (defun fudo--avy-menu (verb desc nouns)
    "Hydra docstring for VERB: one row of NOUNS per group."
    (concat
     ;; The leading newline tells hydra this is a laid-out docstring.
     (format "\nAvy %s: %s\n" verb desc)
     (mapconcat
      (lambda (group)
        (concat (format "%-6s" group)
                (mapconcat (lambda (n) (format "_%s_ %-13s" (nth 0 n) (nth 2 n)))
                           (cl-remove-if-not (lambda (n) (eq (nth 3 n) group)) nouns)
                           "")))
      (cl-remove-duplicates (mapcar (lambda (n) (nth 3 n)) nouns) :from-end t)
      "\n"))))

(defmacro fudo--avy-define-verbs ()
  `(progn
     ,@(cl-loop
        for (_char verb desc) in fudo--avy-verbs
        for hydra = (intern (format "fudo-avy-%s-hydra" verb))
        for excluded = (cdr (assq verb fudo--avy-verb-excludes))
        for nouns = (cl-remove-if (lambda (n) (memq (nth 1 n) excluded))
                                  fudo--avy-nouns)
        collect
        `(defhydra ,hydra (:color blue :hint nil)
           ,(fudo--avy-menu verb desc nouns)
           ,@(cl-loop for (key noun) in nouns
                      collect `(,key (fudo--avy-run ',verb ',noun)))
           ("C-g" nil))
        collect
        `(defun ,(intern (format "avy-action-fudo-%s" verb)) (pt)
           ,(format "Pick the thing at PT to %s." verb)
           (setq fudo--avy-hydra-point pt)
           (,(intern (format "%s/body" hydra)))))))

(fudo--avy-define-verbs)

(defvar mc/cmds-to-run-once)

;; The mark-all heads turn on multiple-cursors themselves; without this, mc
;; re-runs (or asks to re-run) the head at every new cursor.
(after! multiple-cursors-core
  (map-keymap (lambda (_key cmd)
                (when (symbolp cmd)
                  (add-to-list 'mc/cmds-to-run-once cmd)))
              fudo-avy-mark-all-hydra/keymap))

(declare-function helpful-at-point "helpful")

(defun avy-action-fudo-helpful (pt)
  "Show `helpful' documentation for the symbol at PT."
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  t)

;; `avy-dispatch-alist' doesn't exist until avy loads.
(after! avy
  (dolist (verb fudo--avy-verbs)
    (setf (alist-get (car verb) avy-dispatch-alist)
          (intern (format "avy-action-fudo-%s" (cadr verb)))))
  (setf (alist-get ?H avy-dispatch-alist) #'avy-action-fudo-helpful))

(provide 'avy-config)
;;; avy.el ends here
