;;; site.d/ellama.el -*- lexical-binding: t; -*-

(defun ellama-setup--fetch-ollama-models (base-url)
  "Return model names available from the Ollama instance at BASE-URL.
Returns nil and logs a message if the host is unreachable or the
response is not the expected JSON structure."
  (condition-case err
      (let* ((url (concat (string-remove-suffix "/" base-url) "/api/tags"))
             (buf (url-retrieve-synchronously url t t 5)))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (re-search-forward "^$" nil t)
              (let* ((json-object-type 'alist)
                     (json-array-type  'list)
                     (data (json-read)))
                (when (listp data)
                  (mapcar (lambda (m) (alist-get 'name m))
                          (alist-get 'models data)))))
          (when (buffer-live-p buf) (kill-buffer buf))))
    (error
     (message "ellama: could not fetch models from %s (%s)"
              base-url (error-message-string err))
     nil)))

(defun my/ellama-chat ()
  "Open an ellama chat buffer.
If `ellama-provider' is not configured, signals a user-error with
setup instructions rather than attempting a connection and failing
with a cryptic curl error.

To chat with an Ollama instance, run `M-x ellama-setup-ollama'.
For other providers, set `ellama-provider' in your config:
  https://github.com/s-kostyaev/ellama#configuration"
  (interactive)
  (unless (bound-and-true-p ellama-provider)
    (user-error
     (concat "ellama provider not configured.  "
             "Run `M-x ellama-setup-ollama' to connect to an Ollama instance, "
             "or set `ellama-provider' in your config — "
             "see https://github.com/s-kostyaev/ellama#configuration")))
  (call-interactively #'ellama-chat))

(defun ellama-setup-ollama (base-url model)
  "Configure ellama to use an Ollama instance and open a chat buffer.
Prompts for BASE-URL (e.g. http://localhost:11434 or
https://ollama.example.com) then queries it for available models.
Sets `ellama-provider' for the current session; to persist across
restarts, add the equivalent `setq' to your personal config."
  (interactive
   (let* ((url    (read-string "Ollama URL: " "http://localhost:11434"))
          (models (ellama-setup--fetch-ollama-models url))
          (model  (if models
                      (completing-read "Ollama model: " models nil t)
                    (read-string "Ollama model (could not query host): " "llama3.2"))))
     (list url model)))
  (require 'llm-ollama)
  (require 'url-parse)
  (let* ((parsed (url-generic-parse-url base-url))
         (scheme (or (url-type parsed) "http"))
         (host   (url-host parsed))
         (port   (let ((p (url-port parsed)))
                   (if (and p (> p 0)) p
                     (if (string= scheme "https") 443 11434)))))
    (setq ellama-provider
          (make-llm-ollama :scheme scheme :host host :port port
                           :chat-model model)))
  (message "ellama: using Ollama model %S at %s" model base-url)
  (call-interactively #'ellama-chat))

(provide 'ellama-config)
