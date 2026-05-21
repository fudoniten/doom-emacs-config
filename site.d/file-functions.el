;;; file-functions.el -*- lexical-binding: t; -*-
;; File and buffer management utilities
;; Keywords: files, buffers, functions, emacs, utilities

;;; Code:

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
  (untabify (point-min) (point-max))
  (message "Untabified the current buffer"))

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

(provide 'file-functions)

;;; file-functions.el ends here
