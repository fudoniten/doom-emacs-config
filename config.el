;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Niten"
      user-mail-address "niten@fudo.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-snazzy)
(setq doom-theme 'doom-tokyo-night)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(let* ((org-dir (if-let ((org-env-dir (getenv "EMACS_ORG_DIRECTORY")))
                    (file-truename org-env-dir)
                    (file-truename "~/Notes")))
       (roam-dir (format "%s/roam" org-dir)))
  (make-directory roam-dir 'parents)
  (setq org-directory org-dir)
  (setq org-roam-directory roam-dir))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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

(require 'cl)

(load! "site-functions.el")

(setq-default tab-width 2)

(setq inferior-lisp-program "sbcl")

(setq emerge-diff-options "--ignore-all-space")

(setq alert-default-style 'libnotify)

(setq sentence-end-double-space nil)

(setq diff-switches "-u")

(setq tab-always-indent t)

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury the *scratch* buffer, but never kill it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
`persistent-scratch-file-name'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max) "~/.emacs.d/persistent-scratch.el")))

(defun load-persistent-scratch ()
  "Load the contents of `persistent-scratch-file-name' into the
  scratch buffer, clearing its contents first."
  (when (file-exists-p "~/.emacs.d/persistent-scratch.el")
    (with-current-buffer (get-buffer "*scratch*")
      (delete-region (point-min) (point-max))
      (insert-file-contents "~/.emacs.d/persistent-scratch.el"))))

(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta))

(when (or (eq window-system 'x)
          (eq window-system 'darwin))
  (when (boundp 'edit-start-server)
    (edit-start-server)))

(global-prettify-symbols-mode 1)

(ivy-prescient-mode 1)

(global-subword-mode 1)

(with-current-buffer (get-buffer "*scratch*")
  (emacs-lisp-mode))

(defun filter (condp lst)
  "Filter list LST to only those elements matching CONDP."
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(add-hook 'eshell-mode-hook
          (lambda ()
           (setenv "PAGER" "cat")
           (setenv "EDITOR" "emacsclient")))

(defun get-bash-path ()
  "Return paths from the bash PATH."
  (let* ((bash-path (bash-env-var "PATH"))
         (path-dirs (split-string bash-path ":")))
    (filter #'file-directory-p path-dirs)))

(setq exec-path (remove-duplicates (append (get-bash-path) exec-path)
                                   :test #'equal))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(let ((site-dir (if (getenv "DOOM_EMACS_SITE_PATH")
                    (getenv "DOOM_EMACS_SITE_PATH")
                    (expand-file-name ".doom.d/site.d/"))))
  (let ((configs (filter (lambda (name)
                           (not (or (string-match "~$" name)
                                    (string-match "^[.]" name))))
                         (directory-files site-dir))))
    (dolist (file configs)
      (let ((full-file (expand-file-name file site-dir)))
        (if (or (file-regular-p full-file) (file-symlink-p full-file))
          (progn (message "Loading file %s" full-file)
                 (load full-file))
          (message "Skipping invalid file %s" full-file))))))

(let ((local-dir (if (getenv "DOOM_EMACS_LOCAL_PATH")
                    (getenv "DOOM_EMACS_LOCAL_PATH")
                    (expand-file-name ".local/emacs.d/" (getenv "HOME")))))
  (let ((configs (filter (lambda (name)
                           (not (or (string-match "~$" name)
                                    (string-match "^[.]" name))))
                         (directory-files local-dir))))
    (dolist (file configs)
      (let ((full-file (expand-file-name file local-dir)))
        (if (or (file-regular-p full-file) (file-symlink-p full-file))
          (progn (message "Loading file %s" full-file)
                 (load full-file))
          (message "Skipping invalid file %s" full-file))))))

(provide 'config)

;;; config.el ends here
