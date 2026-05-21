;;; text-functions.el -*- lexical-binding: t; -*-
;; Text manipulation utilities
;; Keywords: text, editing, functions, emacs, utilities

;;; Code:

(defun duplicate-line()
  "Duplicate the current line."
  (interactive)
  (let ((text (thing-at-point 'line)))
    (save-excursion
      (end-of-line)
      (insert "\n" text))))

(defun toggle-comment-on-line ()
  "Toggle comment on the current line or region."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (if (use-region-p)
        (setq start (region-beginning)
              end (region-end)))
    (comment-or-uncomment-region start end)
    (message "Toggled comment on line")))

(defun open-line-below ()
  "Open a new line below the current one and move the cursor there."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun open-line-above ()
  "Open a new line above the current one and move the cursor there."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun kill-whitespace ()
  "Kill the whitespace from here to the next non-whitespace character."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (when (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

(defun copy-line (&optional arg)
  "Copy ARG lines to kill ring."
  (interactive "P")
  (save-excursion
    (beginning-of-line-text)
    (let ((start (point)))
      (end-of-line)
      (kill-ring-save start (point))
      (message "Copied line"))))

(defun copy-thing (begin end &optional arg)
  "Copy text between BEGIN and END into kill-ring."
  (save-excursion
    (let ((start (progn (funcall begin arg) (point)))
          (end (progn (funcall end arg) (point))))
      (kill-ring-save start end)
      (message "Copied text"))))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring."
  (interactive "P")
  (save-excursion
    (copy-thing 'backward-word 'forward-word arg)
    (message "Copied word")))

(defun move-text-internal (arg)
  "Move region or current line ARG lines."
  (cond
   ((and mark-active transient-mark-mode)
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)
      (message "Moved text")))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)
      (message "Moved line")))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun open-and-indent-line ()
  "Open a new line below the current one and indent."
  (interactive)
  (save-restriction
    (save-match-data
      (end-of-line)
      (newline-and-indent)
      (message "Opened and indented new line"))))

(defun open-and-indent-previous-line ()
  "Open a new line above the current one and indent."
  (interactive)
  (save-restriction
    (save-match-data
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (indent-according-to-mode)
      (message "Opened and indented previous line"))))

(defun mark-whole-sexp ()
  "Mark the entire sexp surrounding point."
  (interactive)
  (let ((start (progn (up-list 0) (backward-sexp) (point))))
    (goto-char start)
    (set-mark (point))
    (forward-sexp)))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1) (message "Moved to matching parenthesis"))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1) (message "Moved to matching parenthesis"))
        (t (self-insert-command (or arg 1)))))

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP."
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max)))
        (message "Killed lines matching regexp: %s" regexp))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    (flush-lines regexp rstart rend interactive)))

(defun avy-action-kill-whole-line (pt)
  "Kill the whole line at point PT."
  (save-excursion
    (goto-char pt)
    (kill-whole-line)
    (message "Killed whole line at point: %s" pt))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(provide 'text-functions)

;;; text-functions.el ends here
