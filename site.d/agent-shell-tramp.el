;;; site.d/agent-shell-tramp.el -*- lexical-binding: t; -*-

;; Run agent-shell agents on a remote host via TRAMP.
;;
;; acp.el starts the agent process with `:file-handler', so a shell whose
;; `default-directory' is a TRAMP path already runs its agent on that
;; host.  What's missing is path translation: the agent sees plain remote
;; paths (/home/me/src/foo.el) while Emacs needs TRAMP ones
;; (/ssh:host:/home/me/src/foo.el), both for the session's cwd and for
;; the fs/read_text_file and fs/write_text_file requests Emacs serves.
;;
;; `agent-shell-tramp-resolve-path' maps between the two.  It runs in the
;; shell buffer, so it keys off that buffer's `default-directory' and is
;; a no-op for shells running locally.

(require 'tramp)

(defvar agent-shell-path-resolver-function)

(defun agent-shell-tramp-resolve-path (path)
  "Map PATH between a remote agent's filesystem and TRAMP.

TRAMP paths are stripped to the remote-local path the agent
understands; absolute paths coming from the agent get the shell's
TRAMP prefix.  Local shells get PATH back unchanged."
  (let ((prefix (file-remote-p default-directory)))
    (cond ((or (null prefix) (null path)) path)
          ((file-remote-p path) (file-local-name path))
          ((file-name-absolute-p path) (concat prefix path))
          (t path))))

(defun agent-shell-tramp-prepare (dir)
  "Make TRAMP ready to run an agent on the host of remote DIR.

Turns on direct async processes for that host: the agent talks
JSON-RPC over stdio, which wants a plain ssh pipe rather than TRAMP's
shared pty connection."
  (when (file-remote-p dir)
    (let ((vec (tramp-dissect-file-name dir)))
      (connection-local-set-profile-variables
       'remote-direct-async-process
       '((tramp-direct-async-process . t)))
      (connection-local-set-profiles
       `(:application tramp
         :protocol ,(tramp-file-name-method vec)
         :machine ,(tramp-file-name-host vec))
       'remote-direct-async-process))))

;; Agents are often installed somewhere only the login shell's PATH
;; knows about (~/.local/bin, ~/.toolbox/bin, ...).
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(with-eval-after-load 'agent-shell
  (setq agent-shell-path-resolver-function #'agent-shell-tramp-resolve-path))
