;;; site.d/ellama.el -*- lexical-binding: t; -*-

(defun ellama-setup--parse-ollama-endpoint (input)
  "Return a plist (:scheme :host :port) for an Ollama endpoint INPUT.
INPUT may be a bare hostname (\"localhost\", \"192.168.1.10\") or a
full URL (\"http://host:11434\", \"https://ollama.example.com\")."
  (require 'url-parse)
  (let* ((looks-like-url (string-match-p "://" input))
         (parsed (url-generic-parse-url
                  (if looks-like-url input (concat "http://" input))))
         (scheme (or (url-type parsed) "http"))
         (host   (url-host parsed))
         (port   (let ((p (url-port parsed)))
                   ;; url-port returns the scheme default when not specified.
                   (cond ((and looks-like-url p (> p 0)) p)
                         (t 11434)))))
    (list :scheme scheme :host host :port port)))

(defun ellama-setup--fetch-ollama-models (endpoint)
  "Return model names available from the Ollama ENDPOINT plist.
Returns nil and logs a message if the host is unreachable or
returns unexpected data."
  (condition-case err
      (let* ((url (format "%s://%s:%d/api/tags"
                          (plist-get endpoint :scheme)
                          (plist-get endpoint :host)
                          (plist-get endpoint :port)))
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
     (message "ellama: could not reach Ollama at %s:%d (%s)"
              (plist-get endpoint :host)
              (plist-get endpoint :port)
              (error-message-string err))
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
  ;; `ellama-chat' references `ellama-context-format', defined in
  ;; ellama-context.el and not autoloaded.
  (require 'ellama-context)
  (call-interactively #'ellama-chat))

(defun ellama-setup-ollama (endpoint model)
  "Configure ellama to use an Ollama instance and open a chat buffer.
Prompts for ENDPOINT (a bare hostname like \"localhost\" or a full
URL like \"http://host:11434\") and MODEL name (e.g. llama3.2,
mistral, codellama).  Sets `ellama-provider' for the current
session; to persist across restarts, add the equivalent `setq' to
your personal config."
  (interactive
   (let* ((input    (read-string "Ollama host or URL: " "localhost"))
          (endpoint (ellama-setup--parse-ollama-endpoint input))
          (models   (ellama-setup--fetch-ollama-models endpoint))
          (model    (if models
                        (completing-read "Ollama model: " models nil t)
                      (read-string "Ollama model (could not query host): "
                                   "llama3.2"))))
     (list endpoint model)))
  (require 'llm-ollama)
  (when (equal (plist-get endpoint :scheme) "https")
    (message "ellama: llm-ollama only supports http; ignoring https scheme"))
  (setq ellama-provider
        (make-llm-ollama :host       (plist-get endpoint :host)
                         :port       (plist-get endpoint :port)
                         :chat-model model))
  (message "ellama: using Ollama model %S at %s:%d"
           model
           (plist-get endpoint :host)
           (plist-get endpoint :port))
  (require 'ellama-context)
  (call-interactively #'ellama-chat))

(provide 'ellama-config)
