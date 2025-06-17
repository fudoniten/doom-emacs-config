;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Keywords: configuration, emacs, doom emacs, user settings, environment, hooks

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; User Information
(setq user-full-name "Niten"
      user-mail-address "niten@fudo.org")

;; Compilation Settings
(setq native-comp-deferred-compilation-deny-list nil)

;; Appearance
(setq doom-theme 'doom-tokyo-night)
(setq display-line-numbers-type t)

;; Org Mode Directories
(let* ((org-dir (if-let ((org-env-dir (getenv "EMACS_ORG_DIRECTORY")))
                    (file-truename org-env-dir)
                  (file-truename "~/Notes")))
       (roam-dir (format "%s/roam" org-dir)))
  (make-directory roam-dir 'parents)
  (setq org-directory org-dir)
  (setq org-roam-directory roam-dir))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Packages


;; (use-package transient
;;   :ensure t)

;;;; Broken shit.
;; (use-package aider
;;   :ensure t
;;   :after transient
;;   :config
;;   (setq aider-args '("-4"))
;;   (require 'aider-doom))

(use-package aidermacs
  :ensure t
  :after transient)

(require 'cl)
(load! "site-functions.el")
(setq-default tab-width 2)
(setq inferior-lisp-program "sbcl")
(setq emerge-diff-options "--ignore-all-space")
(setq alert-default-style 'libnotify)
(setq sentence-end-double-space nil)
(setq diff-switches "-u")
(setq tab-always-indent t)

;; Functions
(defun filter (condp lst)
  "Filter list LST to only those elements matching CONDP."
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun get-bash-path ()
  "Return paths from the bash PATH."
  (let* ((bash-path (bash-env-var "PATH"))
         (path-dirs (split-string bash-path ":")))
    (filter #'file-directory-p path-dirs)))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun load-configuration-directories (dirs)
  "Load all configuration files from the given list of `DIRS'."
  (dolist (dir dirs)
    (if (and (stringp dir) (file-directory-p dir))
        (let ((configs (filter (lambda (name)
                                 (not (or (string-match "~$" name)
                                          (string-match "^[.]" name))))
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

;; Hooks
(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "PAGER" "cat")
            (setenv "EDITOR" "emacsclient")))
(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

;; System Specific Settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta))

(when (or (eq window-system 'x)
          (eq window-system 'darwin))
  (when (boundp 'edit-start-server)
    (edit-start-server)))

;; Global Modes
(global-prettify-symbols-mode 1)
(ivy-prescient-mode 1)
(global-subword-mode 1)

;; Scratch Buffer
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury the *scratch* buffer, but never kill it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(defvar *persistent-scratch-location*
  (expand-file-name "emacs/persistent-scratch.el" (getenv "XDG_STATE_HOME")))

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
`persistent-scratch-file-name'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((scratch-directory (file-name-directory *persistent-scratch-location*)))
      (when (not (file-directory-p scratch-directory))
        (make-directory scratch-directory t)))
    (write-region (point-min) (point-max) *persistent-scratch-location*)))

(defun load-persistent-scratch ()
  "Load the contents of `persistent-scratch-file-name' into the
  scratch buffer, clearing its contents first."
  (when (file-exists-p *persistent-scratch-location*)
    (with-current-buffer (get-buffer "*scratch*")
      (delete-region (point-min) (point-max))
      (insert-file-contents *persistent-scratch-location*))))

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
  (or (getenv var) ""))

(defun load-all-configurations ()
  "Load configuration directories from various system and user-defined paths."
  (let* ((xdg-vars '("XDG_STATE_HOME" "XDG_DATA_HOME" "XDG_RUNTIME_DIR" "XDG_CONFIG_HOME"))
         (system-bases (filter-existing-dirs
                        (append (mapcar #'getenv-or-empty xdg-vars)
                                (split-string (getenv-or-empty "XDG_CONFIG_DIRS")))))
         (system-subs '("emacs.d" "site-emacs.d" "local-emacs.d" "doom.d"))
         (system-conf-dirs (cross-product-dirs system-bases system-subs))
         (conf-dirs (append system-conf-dirs '("./site.d/"))))
    (message "loading configuration directories: %s" (mapconcat #'identity conf-dirs ", "))
    (load-configuration-directories conf-dirs)))

(load-all-configurations)

(provide 'config)

;;; config.el ends here
