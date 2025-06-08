;;; site-functions.el -*- lexical-binding: t; -*-

;;(require 'eshell)
;;(require 'em-dirs)

;; Miscellaneous Useful Functions

(defun duplicate-line()
  "Duplicate the current line."
  (interactive)
  (let ((text (thing-at-point 'line)))
    (save-excursion
      (end-of-line)
      (insert "\n" text))
    (message "Duplicated line")))

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
  (newline-and-indent)
  (message "Opened line below"))

(defun open-line-above ()
  "Open a new line above the current one and move the cursor there."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode)
  (message "Opened line above"))

(provide 'site-functions)

;;; Code:

;; Utility Functions
(defun kill-whitespace ()
  "Kill the whitespace from here to the next non-whitespace character."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (when (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil)
          (message "Whitespace removed"))))))

(defun join-dirs (&rest strings)
  "Connect a list of STRINGS with a path separator, /."
  (message "Joining directories: %s" strings)
  (mapconcat 'identity strings "/"))

(defun path-join (paths)
  "Join PATHS with a colon separator."
  (message "Joining paths: %s" paths)
  (mapconcat 'identity paths ":"))

(defun strip-dup-paths (path)
  "Remove duplicate paths from PATH."
  (message "Stripping duplicate paths from: %s" path)
  (path-join (delete-dups (split-string path ":"))))

(defun strip-invalid-paths (path)
  "Remove invalid paths from PATH."
  (message "Stripping invalid paths from: %s" path)
  (path-join (filter (lambda (p) (string-match "^/" p)) (split-string path ":"))))

(defun filter (condp lst)
  "Remove all elements not matching CONDP from LST."
  (message "Filtering list with condition: %s" condp)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; Buffer and File Management
(defun rename-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW-NAME."
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
          (set-buffer-modified-p nil)
          (message "Renamed buffer and file to '%s'" new-name))))))

(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir (if (string-match dir "\\(?:/\\|\\\\)$") (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        (message "Moved buffer and file to '%s'" newname)))))

(defun untabify-this-file ()
  "Untabify the entire current buffer."
  (interactive)
  (save-excursion
    (end-of-buffer)
    (let ((pos (point)))
      (untabify 0 pos)
      (message "Untabified the current buffer"))))

(defun yank-buffer-path ()
  "Yank the current buffer's path."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message "Yanked buffer path: %s" filename))))

;; Text Manipulation
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

;; Environment and Shell
(defun bash-env-var (varname)
  "Get the value of a bash environment variable VARNAME."
  (message "Fetching bash environment variable: %s" varname)
  (replace-regexp-in-string "\n$" ""
                            (shell-command-to-string
                             (string-join (list ". ~/.bash_profile; echo $" varname)))))

(defun intern-bash-env-var (varname)
  "Intern a bash environment variable VARNAME into Emacs."
  (let ((val (bash-env-var varname)))
    (setenv varname val)
    (message "Interned bash environment variable: %s with value: %s" varname val)
    val))

(defun ido-find-file-in-tag-files ()
  "Find a file in tag files using ido."
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (ido-completing-read "Project file " (tags-table-files) nil t)))

(defun ensime-remote-connect (host port)
  "Connect to an Ensime server at HOST and PORT."
  (interactive (list
                (read-from-minibuffer "Host: " ensime-default-server-host)
                (read-from-minibuffer "Port: " (format "%d" ensime-default-port) nil t)))
  (let ((c (ensime-connect host port))
        (config (ensime-config-load "/Users/whunmr/lab/scala/.ensime")))
    (ensime-set-config c config)
    (setq ensime-buffer-connection c)
    (message "Connected to Ensime server at %s:%s" host port)))

;; Miscellaneous
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

(provide 'site-functions)

;; site-functions.el ends here
