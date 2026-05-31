;;; site.d/hermes.el -*- lexical-binding: t; -*-

(after! gptel
  (defun hermes-agent-connect ()
    "Open a gptel chat buffer connected to the local Hermes agent.
Reads connection details from environment variables:
  HERMES_AGENT_URL   - base URL, e.g. http://localhost:8080
  HERMES_AGENT_MODEL - model name
  HERMES_AGENT_TOKEN - API token (may be empty for unauthenticated agents)"
    (interactive)
    (require 'url-parse)
    (let* ((url      (or (getenv "HERMES_AGENT_URL")
                         (user-error "HERMES_AGENT_URL is not set")))
           (model    (or (getenv "HERMES_AGENT_MODEL")
                         (user-error "HERMES_AGENT_MODEL is not set")))
           (token    (or (getenv "HERMES_AGENT_TOKEN") ""))
           (parsed   (url-generic-parse-url url))
           (scheme   (url-type parsed))
           (host     (url-host parsed))
           (port     (url-port parsed))
           (path     (url-filename parsed))
           (hostport (if (and port (> port 0))
                         (format "%s:%d" host port)
                       host))
           (endpoint (if (and path
                              (not (string-empty-p path))
                              (not (string= path "/")))
                         path
                       "/v1/chat/completions"))
           (backend  (gptel-make-openai "Hermes"
                       :protocol scheme
                       :host     hostport
                       :endpoint endpoint
                       :key      token
                       :models   (list (intern model)))))
      (let ((gptel-backend backend)
            (gptel-model   (intern model)))
        (gptel "*Hermes*")))))

(provide 'hermes)
