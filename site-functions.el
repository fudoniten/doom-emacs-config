;;; site-functions.el -*- lexical-binding: t; -*-
;; Keywords: functions, emacs, utilities, paths, configuration loading

;; Helpers used by config.el to find and load site configuration
;; directories (this repo's site.d/ plus machine-local XDG directories).

;;; Code:

(require 'cl-lib)

(defun filter-existing-dirs (dirs)
  "Remove any nonexistent directories from a list."
  (cl-remove-if-not #'file-directory-p dirs))

(defun cross-product-dirs (bases subs)
  "Build dirs from all combinations of [bases] x [subs], then filter for existing."
  (filter-existing-dirs
   (mapcan
    (lambda (base)
      (mapcar
       (lambda (sub)
         (expand-file-name sub base))
       subs))
    bases)))

(defun getenv-or-empty (var)
  "Return the value of environment variable VAR, or empty string if unset."
  (or (getenv var) ""))

(defun load-configuration-directories (dirs)
  "Load all configuration files from the given list of `DIRS'.
Files are loaded in `directory-files' (alphabetical) order.  Dotfiles,
backups (foo~) and auto-saves (#foo#) are skipped, and an error in one
file is logged rather than aborting the rest."
  (dolist (dir dirs)
    (if (and (stringp dir) (file-directory-p dir))
        (let ((configs (seq-remove (lambda (name)
                                     (string-match-p "\\(?:~\\'\\|\\`[.#]\\)" name))
                                   (directory-files dir))))
          (dolist (file configs)
            (let ((full-file (expand-file-name file dir)))
              (if (or (file-regular-p full-file) (file-symlink-p full-file))
                  (progn (message "Loading file %s" full-file)
                         (condition-case err
                             (load full-file)
                           (error (message "Error loading file %s: %s" full-file err))))
                (message "Skipping invalid file %s" full-file)))))
      (message "skipping invalid or nonexistent directory: %s" dir))))

(defun load-all-configurations ()
  "Load configuration directories from various system and user-defined paths.
Machine-local directories (emacs.d, site-emacs.d, local-emacs.d, doom.d
under the XDG base directories) load first, then this repo's site.d/."
  (let* ((xdg-vars '("XDG_STATE_HOME" "XDG_DATA_HOME" "XDG_RUNTIME_DIR"))
         ;; macOS doesn't set XDG_CONFIG_HOME, but home-manager still
         ;; writes to ~/.config, so fall back to the spec's default.
         (xdg-config-home (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config")))
         (system-bases (filter-existing-dirs
                        (append (mapcar #'getenv-or-empty xdg-vars)
                                (list xdg-config-home)
                                (split-string (getenv-or-empty "XDG_CONFIG_DIRS") ":" t))))
         (system-subs '("emacs.d" "site-emacs.d" "local-emacs.d" "doom.d"))
         (system-conf-dirs (cross-product-dirs system-bases system-subs))
         (site-config (expand-file-name "site.d" doom-user-dir))
         (conf-dirs (append system-conf-dirs (list site-config))))
    (message "loading configuration directories: %s" (mapconcat #'identity conf-dirs ", "))
    (load-configuration-directories conf-dirs)))

(provide 'site-functions)

;; site-functions.el ends here
