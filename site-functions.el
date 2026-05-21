;;; site-functions.el -*- lexical-binding: t; -*-
;; Keywords: functions, emacs, utilities, paths, shell

;;; Code:

;; Path and Directory Utilities

(defun join-dirs (&rest strings)
  "Connect a list of STRINGS with a path separator, /."
  (mapconcat 'identity strings "/"))

(defun path-join (paths)
  "Join PATHS with a colon separator."
  (mapconcat 'identity paths ":"))

(defun strip-dup-paths (path)
  "Remove duplicate paths from PATH."
  (path-join (delete-dups (split-string path ":"))))

(defun strip-invalid-paths (path)
  "Remove invalid paths from PATH."
  (path-join (filter (lambda (p) (string-match "^/" p)) (split-string path ":"))))

;; Functional Utilities

(defun filter (condp lst)
  "Remove all elements not matching CONDP from LST."
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun flatmap (fn lst)
  (apply #'append (mapcar fn lst)))

;; Environment and Shell

(defun bash-env-var (varname)
  "Get the value of a bash environment variable VARNAME."
  (message "Fetching bash environment variable: %s" varname)
  (replace-regexp-in-string "\n$" ""
                            (shell-command-to-string
                             (string-join (list ". " (expand-file-name ".bash_profile" (getenv "HOME")) "; echo $" varname)))))

;; Configuration Loading Utilities
;; (moved here from config.el so they are available when load-all-configurations runs)

(defun filter-existing-dirs (dirs)
  "Remove any nonexistent directories from a list."
  (cl-remove-if-not #'file-directory-p dirs))

(defun cross-product-dirs (bases subs)
  "Build dirs from all combinations of [bases] x [subs], then filter for existing."
  (filter-existing-dirs
   (flatmap
    (lambda (base)
      (mapcar
       (lambda (sub)
         (expand-file-name sub base))
       subs))
    bases)))

(defun getenv-or-empty (var)
  "Return the value of environment variable VAR, or empty string if unset."
  (or (getenv var) ""))

(provide 'site-functions)

;; site-functions.el ends here
