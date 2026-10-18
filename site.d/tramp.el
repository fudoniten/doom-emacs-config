;;; tramp.el -*- lexical-binding: t; -*-
;; TRAMP performance tuning, and keeping Magit/VC fast on remote files.
;;
;; per: https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./

;;; Code:

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 1024 1024 2)
      tramp-verbose 2)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

;; NOTE: "server" is the example host name from the blog post above; this
;; only applies to a host literally named "server".
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

(provide 'tramp-config)
;;; tramp.el ends here
