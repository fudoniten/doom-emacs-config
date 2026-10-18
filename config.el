;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Keywords: configuration, emacs, doom emacs, user settings, environment, hooks

;; Global settings only.  Feature- and language-specific configuration lives
;; in site.d/*.el, which is loaded (alphabetically) at the end of this file.
;; Remember, you do not need to run 'doom sync' after modifying this file!

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
;;
;; Packages are autoloaded on first use; only load eagerly what something
;; else depends on at startup.  ellama is loaded now because hermes.el and
;; kirocrew.el define their ellama commands inside `(after! ellama ...)'.
(use-package ellama)

;; consult--customize-put calls (message ...) rather than (error ...) when it
;; receives a symbol that is neither a command nor a properly-formed source
;; plist.  This happens when Doom's vertico module and the installed consult
;; version disagree on source naming conventions (e.g. single-dash
;; consult-source-* vs double-dash consult--source-*).  Silently skip invalid
;; entries so the message doesn't appear and valid customizations still apply.
(after! consult
  (define-advice consult--customize-put
      (:filter-args (args) +consult--skip-invalid-sources)
    "Filter out symbols that are neither commands nor consult source plists."
    (cons (seq-filter
           (lambda (sym)
             (or (commandp sym)
                 (and (boundp sym)
                      (plist-get (symbol-value sym) :name))))
           (car args))
          (cdr args)))

  ;; Customize preview behavior using the stable command API.
  (consult-customize
   consult-buffer consult-bookmark consult-recent-file
   :preview-key "C-SPC"))

(use-package aidermacs
  :after transient)
(use-package polymuse :if (locate-library "polymuse"))
(use-package canon   :if (locate-library "canon"))

(load! "site-functions.el")
(setq-default tab-width 2)
(setq inferior-lisp-program "sbcl")
(setq emerge-diff-options "--ignore-all-space")
(setq alert-default-style 'libnotify)
(setq sentence-end-double-space nil)
(setq diff-switches "-u")
(setq tab-always-indent t)

;; TLS Advice
;; Declare it special so the `let' below binds it dynamically even if gnutls
;; hasn't been loaded yet (this file uses lexical-binding).
(defvar gnutls-verify-error)

(defun tls-nocheck-error-advice (orig-fun &rest args)
  "Advise a function (with :around) not to check TLS errors.

ORIG-FUN - Function name to be advised
ARGS - Arguments to function

Usage: (advice-add 'my-function-for-advisement :around 'tls-nocheck-error-advice."
  (let ((gnutls-verify-error nil))
    (apply orig-fun args)))

(after! elpher
  (advice-add 'elpher-get-gemini-response :around 'tls-nocheck-error-advice))

;; Hooks
(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)

;; System Specific Settings
;; Set the ns-* variables directly. The mac-* names are aliases that
;; term/ns-win.el only creates when the first GUI frame opens; in a daemon
;; that happens after this runs, and the aliasing discards the value.
(when (boundp 'ns-command-modifier)
  (setq ns-command-modifier 'meta
        ns-alternate-modifier 'meta))

;; FIXME: never runs -- `window-system' is never `darwin' (that's
;; `system-type'), and the function is `edit-server-start', not
;; `edit-start-server' (and it's a function, so `boundp' is always nil).
(when (or (eq window-system 'x)
          (eq window-system 'darwin))
  (when (boundp 'edit-start-server)
    (edit-start-server)))

;; Global Modes
(global-prettify-symbols-mode 1)
(global-subword-mode 1)

;; Scratch Buffer
;; Doom already refuses to kill *scratch* (`doom-protect-fallback-buffer-h').
;; This keeps its contents across restarts.
(defvar *persistent-scratch-location*
  (expand-file-name "emacs/persistent-scratch.el"
                    (or (getenv "XDG_STATE_HOME") "~/.local/state")))

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to `*persistent-scratch-location*'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((scratch-directory (file-name-directory *persistent-scratch-location*)))
      (when (not (file-directory-p scratch-directory))
        (make-directory scratch-directory t)))
    (write-region (point-min) (point-max) *persistent-scratch-location*)))

(defun load-persistent-scratch ()
  "Load `*persistent-scratch-location*' into *scratch*, replacing its contents."
  (when (file-exists-p *persistent-scratch-location*)
    (with-current-buffer (get-buffer-create "*scratch*")
      (delete-region (point-min) (point-max))
      (insert-file-contents *persistent-scratch-location*))))

;; Load machine-local config dirs, then this repo's site.d/ (see
;; site-functions.el).  Keep this last.
(load-all-configurations)

(provide 'config)

;;; config.el ends here
