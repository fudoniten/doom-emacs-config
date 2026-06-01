;;; site.d/ellama.el -*- lexical-binding: t; -*-

(defun ellama-setup--fetch-ollama-models (host)
  "Return model names available from Ollama at HOST (port 11434).
Returns nil and logs a message if HOST is unreachable or returns
unexpected data."
  (condition-case err
      (let* ((url (format "http://%s:11434/api/tags" host))
             (buf (url-retrieve-synchronously url t t 5)))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (re-search-forward "^$" nil t)
              (let* ((json-object-type 'alist)
                     (json-array-type  'list)
                     (data   (json-read))
                     (models (alist-get 'models data)))
                (mapcar (lambda (m) (alist-get 'name m)) models)))
          (when (buffer-live-p buf) (kill-buffer buf))))
    (error
     (message "ellama: could not reach Ollama at %s (%s)"
              host (error-message-string err))
     nil)))

(defun my/ellama-chat ()
  "Open an ellama chat buffer.
If `ellama-provider' is not configured, signals a user-error with
setup instructions rather than attempting a connection and failing
with a cryptic curl error.

To chat with a local Ollama instance, run `M-x ellama-setup-ollama'.
For other providers, set `ellama-provider' in your config:
  https://github.com/s-kostyaev/ellama#configuration"
  (interactive)
  (unless (bound-and-true-p ellama-provider)
    (user-error
     (concat "ellama provider not configured.  "
             "Run `M-x ellama-setup-ollama' for a local Ollama instance, "
             "or set `ellama-provider' in your config — "
             "see https://github.com/s-kostyaev/ellama#configuration")))
  (call-interactively #'ellama-chat))

(defun ellama-setup-ollama (host model)
  "Configure ellama to use an Ollama instance and open a chat buffer.
Prompts for HOST (e.g. localhost, 192.168.1.10) and MODEL name
(e.g. llama3.2, mistral, codellama).  Sets `ellama-provider' for
the current session; to persist across restarts, add the equivalent
`setq' to your personal config."
  (interactive
   (let* ((host   (read-string "Ollama host: " "localhost"))
          (models (ellama-setup--fetch-ollama-models host))
          (model  (if models
                      (completing-read "Ollama model: " models nil t)
                    (read-string "Ollama model (could not query host): " "llama3.2"))))
     (list host model)))
  (require 'llm-ollama)
  (setq ellama-provider (make-llm-ollama :host host :chat-model model))
  (message "ellama: using Ollama model %S on %s" model host)
  (call-interactively #'ellama-chat))

(provide 'ellama-config)
