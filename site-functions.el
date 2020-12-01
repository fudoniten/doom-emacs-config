;;; site-functions.el -*- lexical-binding: t; -*-

;;(require 'eshell)
;;(require 'em-dirs)

(provide 'site-functions)

;;; Code:

(defun kill-whitespace ()
  "Kill the whitespace from here to the next non-whitespace character."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

;; (defun split-line ()
;;   "Split the current line into two at the current point."
;;   (interactive "*")
;;   (save-excursion
;;     (save-restriction
;;       (save-match-data
;;         (progn (newline-and-indent))))))

(defun bind-hooks-to-modes (hooks modes)
  "Takes a list of HOOKS and a list of MODES, add all hooks to all modes."
  (dolist (hook hooks)
    (dolist (mode modes)
      (add-hook mode hook))))

(defun bind-hook-to-modes (hook modes)
  "Bind a single HOOK to many MODES."
  (bind-hooks-to-modes (list hook) modes))

(defun bind-hooks-to-mode (hooks mode)
  "Bind many HOOKS to a single MODE."
  (bind-hooks-to-modes hooks (list mode)))

;; (defun remote-shell (host)
;;   "Open a remote shell on the given host"
;;   (interactive "sRemote host name: \n")
;;   (let ((default-directory (concat "/" host ":/home/" (user-login-name) "/")))
;;     (shell (concat "*" host "*"))))


;; Emacs Lisp doesn’t come with a ‘filter’ function to keep elements that satisfy
;; a conditional and excise the elements that do not satisfy it. One can use ‘mapcar’
;; to iterate over a list with a conditional, and then use ‘delq’ to remove the ‘nil’
;; values.

(defun filter (condp lst)
  "Remove all elements not matching CONDP from LST."
  (delq nil
	      (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; Therefore
;;
;; (my-filter 'identity my-list)
;; is equivalent to
;; (delq nil my-list)
;;
;; For example:
;;(let ((num-list '(1 'a 2 "nil" 3 nil 4)))
;;  (my-filter 'numberp num-list))   ==> (1 2 3 4)
;;
;; Actually the package cl-seq contains the functions remove-if and remove-if-not.
;; The latter can be used instead of my-filter.

;;
;;  Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

;;
;;  Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn
	(copy-file filename newname 1)
	(delete-file filename)
	(set-visited-file-name newname)
	(set-buffer-modified-p nil)
	t))))

(defun join-dirs (&rest strings)
  "Connect a list of STRINGS with a path separator, /."
  (mapconcat 'identity strings "/"))

(defun my-copy-line (&optional arg)
  "Copy ARG lines to kill ring."
  (interactive "P")
  (let ((n (if arg
               arg
             1)))
    (kill-ring-save (line-beginning-position)
                    (line-beginning-position (+ n 1)))
    (message "%d line%s copied" n (if (= 1 n) "" "s"))))

(defvar my-time-format "%k:%M:%S")
(defvar my-date-format "%Y-%m-%d")

(defun my-run-command-on-this-line (cmd)
  "Run the CMD found on this line in an external shell."
  (beginning-of-line-text)
  (let ((start (point)))
    (end-of-line)
    (command start (end-of-line))))

(defun my-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (buffer-name))
  (set-name))

(defun my-forward-word ()
  "Move one word forward. Leave the pointer at start of word."
  (interactive)
  (forward-char 1)
  (backward-word 1)
  (forward-word 2)
  (backward-word 1)
  (backward-char 1)
  (forward-char 1))

(defun my-backward-word ()
  "Move one word backward. Leave the pointer at start of word."
  (interactive)
  (backward-word 1)
  (backward-char 1)
  (forward-char 1))

(defun smart-tab ()
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(dabbrev-expand nil))
    (if mark-active
	(indent-region (region-beginning)
		       (region-end))
      (indent-for-tab-command))))

;; (defun smart-tab ()
;;   "This smart tab is minibuffer compliant: it acts as usual in
;;     the minibuffer. Else, if mark is active, indents region. Else if
;;     point is at the end of a symbol, expands it. Else indents the
;;     current line."
;;   (interactive)
;;   (if (minibufferp)
;;       (unless (minibuffer-complete)
;;         (dabbrev-expand nil))
;;     (if mark-active
;;         (indent-region (region-beginning)
;;                        (region-end))
;;       (if (looking-at "\\_>")
;;           ;;(dabbrev-expand nil)
;; 	  (hippie-expand nil)
;;         (indent-for-tab-command)))))

;; (defun indent-or-expand (arg)
;;   "Either indent according to mode, or expand the word preceding point."
;;   (interactive "*P")
;;   (if (and
;;        (or (bobp) (= ?w (char-syntax (char-before))))
;;        (or (eobp) (not (= ?w (char-syntax (char-after))))))
;;       (hippie-expand arg)
;;     (indent-according-to-mode)))

(defun copy-line (&optional arg)
  (interactive)
  (save-excursion
    (beginning-of-line-text)
    (let ((start (point)))
      (end-of-line)
      (kill-ring-save start (point)))))

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (let ((beg (get-point begin-of-thing 1))
        (end (get-point end-of-thing arg)))
    (copy-region-as-kill beg end)))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (save-excursion
    (copy-thing 'backward-word 'forward-word arg)))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)

    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (ido-completing-read "Project file "
			 (tags-table-files)
			 nil t)))

(defun untabify-this-file ()
  (interactive)
  (save-excursion
    (end-of-buffer)
    (let ((pos (point)))
      (untabify 0 pos))))

(defun ensime-remote-connect (host port)
  (interactive (list
                (read-from-minibuffer "Host: " ensime-default-server-host)
                (read-from-minibuffer "Port: " (format "%d" ensime-default-port)
                                      nil t)))
  (let ((c (ensime-connect host port))
        (config (ensime-config-load "/Users/whunmr/lab/scala/.ensime")))
    (ensime-set-config c config)
    (setq ensime-buffer-connection c)))

(defun path-join (paths)
  (mapconcat 'identity paths ":"))

(defun strip-dup-paths (path)
  (path-join
   (delete-dups
    (split-string path ":"))))

(defun strip-invalid-paths (path)
  (path-join
   (filter (lambda (p)
             (string-match "^/" p))
           (split-string path ":"))))

(defun get-path ()
  (split-string (getenv "PATH") ":"))

(defun string-join (lst chr)
  (mapconcat 'identity lst chr))

(defun any-p (f lst)
  (reduce
   (lambda (a b) (or a b))
   (mapcar f lst)
   :initial-value '()))

(defun every-p (f lst)
  (reduce
   (lambda (a b) (and a b))
   (mapcar f lst)
   :initial-value t))

(defun odd-p (n)
  (= (% n 2) 1))

(defun even-p (n)
  (not (odd-p n)))

(defun subpath-p (parent child-p)
  (string-match (concat "^" parent ".+") child-p))

(defvar read-only-paths '())

(defun should-be-readonly (filepath)
  (any-p
   (lambda (path)
     (subpath-p path filepath))
   read-only-paths))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis,
   vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun yank-buffer-path ()
  "Yank the current buffer's path"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;; (save-excursion
;;   (save-restriction
;;     (save-match-data
;;       (progn
;;         (re-search-forward "[ \t\r\n]+" nil t)
;;         (replace-match "" nil nil)))))

(defun open-and-indent-line ()
  (interactive)
  (save-restriction
    (save-match-data
      (progn (end-of-line)
             (newline-and-indent)))))

(defun open-and-indent-previous-line ()
  (interactive)
  (save-restriction
    (save-match-data
      (progn (beginning-of-line)
             (newline)
             (forward-line -1)
             (indent-according-to-mode)))))

(defun foreach (f alist)
  (while alist
    (progn (funcall f (car alist))
           (setq alist (cdr alist)))))

;; site-functions.el ends here
