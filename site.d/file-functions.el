;;; file-functions.el -*- lexical-binding: t; -*-
;; File and buffer management utilities
;; Keywords: files, buffers, functions, emacs, utilities

;;; Code:

;; Renaming, moving and copying the visited file are covered by
;; `rename-visited-file' (Emacs 29), `doom/move-this-file' and
;; `+default/yank-buffer-path'; see the C-c bindings in bindings.el.

(defun untabify-this-file ()
  "Untabify the entire current buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (message "Untabified the current buffer"))

(provide 'file-functions)

;;; file-functions.el ends here
