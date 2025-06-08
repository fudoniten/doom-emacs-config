;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
(use-package transient
  :ensure t)

(use-package aider
  :ensure t
  :after transient
  :config
  (setq aider-args '("-4"))
  (require 'aider-doom))

;; Environment
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
                         (condition-case err
                             (load full-file)
                           (error (message "Error loading file %s: %s" full-file err))))
                (message "Skipping invalid file %s" full-file)))))
      (message "Skipping nonexistent config directory %s" config-dir))))

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

;;;;;;;;;;
;; AVY
;;;;;;;;;;

(defvar fudo--avy-hydra-func nil
  "A variable to hold the command to be executed by Avy.")

(defun kill-thing-at-point (thing)
  "Return a lambda to kill the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (kill-region start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t))

(defun move-thing-at-point (thing)
  "Return a lambda to move the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (kill-region start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    (yank)
    t))

(defun fudo--avy-wrap-action (f)
  "Take a function F and call it with the point specified by Avy."
  (message "calling with point %s!" fudo--avy-hydra-point)
  (funcall f fudo--avy-hydra-point)
  (setq fudo--avy-hydra-point nil))

(defun copy-thing-at-point (thing)
  "Return a lambda to copy the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (let ((obj (buffer-substring-no-properties start end)))
          (kill-new obj))))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t))

(defun yank-thing-at-point (thing)
  "Return a lambda to yank the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (let ((obj (buffer-substring-no-properties start end)))
          (kill-new obj))))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    (yank)
    t))

(defun comment-thing-at-point (thing)
  "Return a lambda to comment the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (comment-region start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t))

(defun mark-thing-at-point (thing)
  "Return a lambda to mark the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
        (push-mark start nil t)
        (goto-char end)
        (activate-mark)))
    t))

(defun swap-thing-at-point (thing)
  "Return a lambda to swap the THING at point."
  (lambda (pt)
    (save-mark-and-excursion
      (let ((start (point-marker))
            (src)
            (tgt))
        (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
          (kill-region start end)
          (setq src (current-kill 0)))
        (goto-char pt)
        (cl-destructuring-bind (start . end) (bounds-of-thing-at-point thing)
          (kill-region start end)
          (setq tgt (current-kill 0))
          (insert src))
        (goto-char start)
        (insert tgt)))))

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
  "Set the region from START to END."
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

(defhydra hydra-action-swap (:color green :hint nil)
  "Avy swap object"
  ("l" (fudo--avy-wrap-action (swap-thing-at-point 'line))      "line")
  ("p" (fudo--avy-wrap-action (swap-thing-at-point 'paragraph)) "paragraph")
  ("s" (fudo--avy-wrap-action (swap-thing-at-point 'sentence))  "sentence")
  ("S" (fudo--avy-wrap-action (swap-thing-at-point 'sexp))      "sexp")
  ("w" (fudo--avy-wrap-action (swap-thing-at-point 'word))      "word"))

(defun fudo--avy-call-with-point (f)
  "Return a lambda to call function F with the point specified by Avy."
  (lambda (pt)
    (message "Saving point: %s" pt)
    (setq fudo--avy-hydra-point pt)
    (funcall f)))

(defun zap-to-point (pt)
  "Kill the region from point to PT."
  (kill-region (point) pt))

(defun avy-action-comment (pt)
  "Invoke the comment hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-comment/body))

(defun avy-action-copy (pt)
  "Invoke the copy hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-copy/body))

(defun avy-action-kill (pt)
  "Invoke the kill hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-kill/body))

(defun avy-action-move (pt)
  "Invoke the move hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-move/body))

(defun avy-action-region (pt)
  "Invoke the region hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-region/body))

(defun avy-action-swap (pt)
  "Invoke the swap hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-swap/body))

(defun avy-action-yank (pt)
  "Invoke the yank hydra at PT."
  (setq fudo--avy-hydra-point pt)
  (hydra-action-yank/body))

(setf (alist-get ?c avy-dispatch-alist) #'avy-action-comment
      (alist-get ?C avy-dispatch-alist) #'avy-action-copy
      (alist-get ?H avy-dispatch-alist) #'helpful-at-point
      (alist-get ?K avy-dispatch-alist) #'avy-action-kill
      (alist-get ?m avy-dispatch-alist) #'avy-action-move
      (alist-get ?r avy-dispatch-alist) #'avy-action-region
      (alist-get ?S avy-dispatch-alist) #'avy-action-swap
      (alist-get ?y avy-dispatch-alist) #'avy-action-yank
      (alist-get ?z avy-dispatch-alist) #'zap-to-point)

(provide 'config)

;;; config.el ends here
