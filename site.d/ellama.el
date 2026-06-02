;;; site.d/ellama.el -*- lexical-binding: t; -*-

(defun ellama-setup--parse-ollama-endpoint (input)
  "Return a plist (:scheme :host :port) for an Ollama endpoint INPUT.
INPUT may be a bare hostname (\"ollama.example.com\"), a host:port
pair (\"localhost:11434\"), or a full URL (\"https://host\",
\"http://host:11434\").  When no port is given, defaults to 443 —
suitable for ingress/virtual-host setups; specify a port explicitly
to override."
  (require 'url-parse)
  (let* ((has-scheme   (string-match-p "://" input))
         (with-scheme  (if has-scheme input (concat "http://" input)))
         (parsed       (url-generic-parse-url with-scheme))
         (scheme       (or (url-type parsed) "http"))
         ;; Detect an explicit port in the user's input (either
         ;; "host:port" or "scheme://host:port").  `url-port' falls back
         ;; to the scheme default, which we can't distinguish from an
         ;; explicit match.
         (host+rest    (if has-scheme
                           (substring input (+ (match-beginning 0) 3))
                         input))
         (explicit?    (string-match-p ":[0-9]+\\(/\\|\\'\\)" host+rest))
         (host         (url-host parsed))
         (port         (if explicit? (url-port parsed) 443)))
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
                     (data (json-read)))
                (when (listp data)
                  (mapcar (lambda (m) (alist-get 'name m))
                          (alist-get 'models data)))))
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
