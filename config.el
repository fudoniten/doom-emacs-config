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
(use-package doom-two-tone-themes)


(after! doom-themes
  (custom-set-faces!
    ;; “turn off” most categories
    '(font-lock-keyword-face       :inherit default)
    '(font-lock-builtin-face       :inherit default)
    '(font-lock-type-face          :inherit default)
    '(font-lock-variable-name-face :inherit default)
    '(font-lock-function-name-face :inherit default)
    '(font-lock-doc-face           :inherit default)
    '(font-lock-comment-face       :inherit default)

    ;; keep the “4 classes”
    ;;'(font-lock-string-face        :inherit default)   ; then add :foreground if you want
    ;;'(font-lock-comment-face       :inherit default)
    ;;'(font-lock-constant-face      :inherit default)
    ;;'(font-lock-doc-face           :inherit font-lock-comment-face)
    ))

(with-eval-after-load 'clojure-mode
  (font-lock-add-keywords 'clojure-mode
                          '(("^\\s-*\\(;;;+.*\\)$" 1 '(:inherit font-lock-comment-face :weight bold) t))
                          'append)
  (custom-set-faces!
    '(font-lock-comment-face      :inherit default)
    '(clojure-special-form-face   :inherit default)
    '(clojure-macro-face          :inherit default)
    '(clojure-keyword-face        :inherit default)
    '(clojure-interop-method-face :inherit default)
    '(clojure-namespace-face      :inherit default)
    '(clojure-symbol-face         :inherit default)))

(let* ((env-theme (getenv "DOOM_THEME"))
       (theme-name (if (and env-theme (not (string-empty-p env-theme)))
                       (intern env-theme)
                     'doom-snazzy)))
  (message "using doom theme: %s" theme-name)
  (setq doom-theme theme-name))

(setq display-line-numbers-type t)

;; Org Mode Directories
(let* ((org-dir (if-let ((org-env-dir (getenv "EMACS_ORG_DIRECTORY")))
                    (file-truename org-env-dir)
                  (expand-file-name "Notes" (getenv "HOME"))))
       (roam-dir (format "%s/roam" org-dir)))
  (make-directory roam-dir 'parents)
  (setq org-directory org-dir)
  (setq org-roam-directory roam-dir))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
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

(use-package elpher)
(use-package chatgpt-shell)
(use-package restclient)

(use-package org-roam)

(use-package transient)

(use-package aidermacs
  :ensure t
  :after transient)
(use-package edit-server)
(use-package eglot)
(use-package nix-mode)
(use-package kubernetes)
(use-package gptel)
(use-package ellama)
(use-package polymuse :if (locate-library "polymuse"))
(use-package canon   :if (locate-library "canon"))
;;;; Broken?
;; (use-package graphviz-dot-mode)

(use-package paredit
  :ensure nil
  :commands (paredit-mode)
  :hook ((clojure-mode . paredit-mode)
         (cider-repl-mode . paredit-mode))
  :config (map! :map paredit-mode-map
                "M-(" #'paredit-wrap-round
                "M-)" #'paredit-close-round))

(use-package bash-completion
  :commands bash-completion-dynamic-complete
  :hook ((shell-dynamic-complete-functions . bash-completion-dynamic-complete)
         (eshell-mode . my/eshell-mode-completion-hook))
  :config (defun my/eshell-mode-completion-hook ()
            (add-hook 'completion-at-point-functions
                      'bash-completion-capf-nonexclusive nil t)))

(require 'cl-lib)
(load! "site-functions.el")
(setq-default tab-width 2)
(setq inferior-lisp-program "sbcl")
(setq emerge-diff-options "--ignore-all-space")
(setq alert-default-style 'libnotify)
(setq sentence-end-double-space nil)
(setq diff-switches "-u")
(setq tab-always-indent t)

;; Eglot and Nix
(when (and (locate-library "eglot") (locate-library "nix-mode"))
  (after! eglot
    (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
    (add-hook 'nix-mode-hook 'eglot-ensure)))

;; TLS Advice
(defun tls-nocheck-error-advice (orig-fun &rest args)
  "Advise a function (with :around) not to check TLS errors.

ORIG-FUN - Function name to be advised
ARGS - Arguments to function

Usage: (advice-add 'my-function-for-advisement :around 'tls-nocheck-error-advice."
  (let ((gnutls-verify-error nil))
    (apply orig-fun args)))

(after! elpher
  (advice-add 'elpher-get-gemini-response :around 'tls-nocheck-error-advice))

;;;;
;; TRAMP
;;;;
;;
;; per: https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 1024 1024 2)
      tramp-verbose 2)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :machine "server")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(defun $magit-auto-revert-not-remote (orig-fun &rest args)
  (unless (and buffer-file-name (file-remote-p buffer-file-name))
    (apply orig-fun args)))

(advice-add 'magit-turn-on-auto-revert-mode-if-desired
            :around
            #'$magit-auto-revert-not-remote)

(setq magit-branch-direct-configure nil
      magit-refresh-status-buffer nil)

(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key
           (file-remote-p key))
      (if-let ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

;; Memoize magit top level
(defvar magit-toplevel-cache nil)
(defun memoize-magit-toplevel (orig &optional directory)
  (memoize-remote (or directory default-directory)
                  'magit-toplevel-cache orig directory))
(advice-add 'magit-toplevel :around #'memoize-magit-toplevel)

;; memoize vc-git-root
(defvar vc-git-root-cache nil)
(defun memoize-vc-git-root (orig file)
  (let ((value (memoize-remote (file-name-directory file) 'vc-git-root-cache orig file)))
    ;; sometimes vc-git-root returns nil even when there is a root there
    (when (null (cdr (car vc-git-root-cache)))
      (setq vc-git-root-cache (cdr vc-git-root-cache)))
    value))
(advice-add 'vc-git-root :around #'memoize-vc-git-root)


;;;;
;; Functions
;;;;
(defun get-bash-path ()
  "Return paths from the bash PATH."
  (let* ((bash-path (bash-env-var "PATH"))
         (path-dirs (split-string bash-path ":")))
    (filter #'file-directory-p path-dirs)))

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
(global-subword-mode 1)

;; Scratch Buffer
(advice-add 'kill-buffer :around
  (lambda (orig-fun &rest args)
    "Bury the *scratch* buffer, but never kill it."
    (let* ((buf (or (car args) (current-buffer)))
           (buf-name (if (bufferp buf) (buffer-name buf) buf)))
      (if (equal buf-name "*scratch*")
          (bury-buffer)
        (apply orig-fun args)))))

(defvar *persistent-scratch-location*
  (expand-file-name "emacs/persistent-scratch.el"
                    (or (getenv "XDG_STATE_HOME") "~/.local/state")))

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

(defun load-all-configurations ()
  "Load configuration directories from various system and user-defined paths."
  (let* ((xdg-vars '("XDG_STATE_HOME" "XDG_DATA_HOME" "XDG_RUNTIME_DIR" "XDG_CONFIG_HOME"))
         (system-bases (filter-existing-dirs
                        (append (mapcar #'getenv-or-empty xdg-vars)
                                (split-string (getenv-or-empty "XDG_CONFIG_DIRS")))))
         (system-subs '("emacs.d" "site-emacs.d" "local-emacs.d" "doom.d"))
         (system-conf-dirs (cross-product-dirs system-bases system-subs))
         (site-config (expand-file-name "site.d" (file-name-directory (or load-file-name buffer-file-name))))
         (conf-dirs (append system-conf-dirs (list site-config))))
    (message "loading configuration directories: %s" (mapconcat #'identity conf-dirs ", "))
    (load-configuration-directories conf-dirs)))

(load-all-configurations)

(provide 'config)

;;; config.el ends here
