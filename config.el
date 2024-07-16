;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Causing errors because it's missing
(setq native-comp-deferred-compilation-deny-list nil)

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

(setq yas-snippet-dirs '())

(defun load-configuration-directory (config-dir)
  "Load all configuration files from the given `CONFIG-DIR' if it exists."
  (when (stringp config-dir)
    (if (file-directory-p config-dir)
        (let ((configs (filter (lambda (name)
                                 (not (or (string-match "~$" name)
                                          (string-match "^[.]" name))))
                               (directory-files config-dir))))
          (dolist (file configs)
            (let ((full-file (expand-file-name file config-dir)))
              (if (or (file-regular-p full-file) (file-symlink-p full-file))
                  (progn (message "Loading file %s" full-file)
                         (load full-file))
                (message "Skipping invalid file %s" full-file)))))
      (message "Skipping nonexistent config directory %s" config-dir))))

(load-configuration-directory (expand-file-name "site.d/" (file-name-directory (or load-file-name (buffer-file-name)))))
(load-configuration-directory (getenv "DOOM_EMACS_SITE_PATH"))
(load-configuration-directory (getenv "DOOM_EMACS_LOCAL_PATH"))
(load-configuration-directory (expand-file-name "doom-local/" (getenv "XDG_CONFIG_HOME")))
(load-configuration-directory (expand-file-name "emacs-local/" (getenv "XDG_CONFIG_HOME")))

;;;;;;;;;;
;; AVY
;;;;;;;;;;

(defvar fudo--avy-hydra-point nil
  "A point, set by avy, for hydra to use when called.")

(defun kill-thing-at-point (thing)
  (lambda (pt)
    (save-excursion
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point pt))
      (kill-region start end))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t))

(defun move-thing-at-point (thing)
  (lambda (pt)
    (save-excursion
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point pt))
      (kill-region start end))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    (yank)
    t))

(defun fudo--avy-wrap-action (f)
  "Take a function F and call it with the point specified by Avy."
  (lambda () (funcall f fudo--avy-hydra-point)))

(defun copy-thing-at-point (thing)
  (lambda (pt)
    (save-excursion
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point pt))
      (let ((obj (buffer-substring-no-properties start end)))
        (kill-new obj)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t))

(defun yank-thing-at-point (thing)
  (lambda (pt)
    (save-excursion
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point pt))
      (let ((obj (buffer-substring-no-properties start end)))
        (kill-new obj)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    (yank)
    t))

(defun comment-thing-at-point (thing)
  (lambda (pt)
    (save-excursion
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point pt)
        (comment-region start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    (yank)
    t))

(defun mark-thing-at-point (thing)
  (lambda (pt)
    (cl-destructuring-bind (start . end) (bounds-of-thing-at-point pt)
      (push-mark start nil t)
      (goto-char end)
      (activate-mark))
    t))

(defhydra hydra-action-kill (:color red :hint nil)
  "Avy kill"
  ("k" (lambda () (avy-action-kill-stay fudo--avy-hydra-point))      "kill and stay")
  ("d" (fudo--avy-wrap-action (kill-thing-at-point 'defun))     "defun")
  ("e" (fudo--avy-wrap-action (kill-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (kill-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (kill-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (kill-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (kill-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (kill-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (kill-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (kill-thing-at-point 'word))      "word"))

(defhydra hydra-action-copy (:color green :hint nil)
  "Avy copy"
  ("d" (fudo--avy-wrap-action (copy-thing-at-point 'defun))     "defun")
  ("e" (fudo--avy-wrap-action (copy-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (copy-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (copy-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (copy-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (copy-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (copy-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (copy-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (copy-thing-at-point 'word))      "word"))

(defhydra hydra-action-yank (:color green :hint nil)
  "Avy yank"
  ("d" (fudo--avy-wrap-action (yank-thing-at-point 'defun))     "defun")
  ("e" (fudo--avy-wrap-action (yank-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (yank-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (yank-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (yank-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (yank-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (yank-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (yank-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (yank-thing-at-point 'word))      "word"))

(defhydra hydra-action-move (:color green :hint nil)
  "Avy move"
  ("d" (fudo--avy-wrap-action (move-thing-at-point 'defun))     "defun")
  ("e" (fudo--avy-wrap-action (move-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (move-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (move-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (move-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (move-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (move-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (move-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (move-thing-at-point 'word))      "word"))

(defhydra hydra-action-comment (:color green :hint nil)
  "Avy comment"
  ("d" (fudo--avy-wrap-action (comment-thing-at-point 'defun))     "defun")
  ("l" (fudo--avy-wrap-action (comment-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (comment-thing-at-point 'paragraph)) "paragraph")
  ("S" (fudo--avy-wrap-action (comment-thing-at-point 'sexp))      "sexp")
  ("w" (fudo--avy-wrap-action (comment-thing-at-point 'word))      "word"))

(defun set-region (start end)
  (push-mark start nil t)
  (goto-char end)
  (activate-mark)
  t)

(defhydra hydra-action-region (:color green :hint nil)
  "Avy mark region"
  ("j" (fudo--avy-wrap-action (lambda (pt) (set-region (point) pt))) "to-point")
  ("e" (fudo--avy-wrap-action (mark-thing-at-point 'email))     "email")
  ("f" (fudo--avy-wrap-action (mark-thing-at-point 'filename))  "filename")
  ("l" (fudo--avy-wrap-action (mark-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (mark-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (mark-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (mark-thing-at-point 'sexp))      "sexp")
  ("u" (fudo--avy-wrap-action (mark-thing-at-point 'url))       "url")
  ("w" (fudo--avy-wrap-action (mark-thing-at-point 'word))      "word"))

(defun fudo--avy-call-with-point (f)
  (lambda (pt)
    (unwind-protect
        (progn (setq fudo--avy-hydra-point pt)
               (funcall f))
      (setq fudo--avy-hydra-point nil))))

(defun zap-to-point (pt)
  (kill-region (point) pt))

(setf (alist-get ?j avy-dispatch-alist) #'avy-jump
      (alist-get ?c avy-dispatch-alist) (fudo--avy-call-with-point #'hydra-action-comment/body)
      (alist-get ?C avy-dispatch-alist) (fudo--avy-call-with-point #'hydra-action-copy/body)
      (alist-get ?h avy-dispatch-alist) #'helpful-at-point
      (alist-get ?k avy-dispatch-alist) (fudo--avy-call-with-point #'hydra-action-kill/body)
      (alist-get ?m avy-dispatch-alist) (fudo--avy-call-with-point #'hydra-action-move/body)
      (alist-get ?r avy-dispatch-alist) (fudo--avy-call-with-point #'hydra-action-region/body)
      (alist-get ?y avy-dispatch-alist) (fudo--avy-call-with-point #'hydra-action-yank/body)
      (alist-get ?z avy-dispatch-alist) #'zap-to-point)


(provide 'config)

;;; config.el ends here
