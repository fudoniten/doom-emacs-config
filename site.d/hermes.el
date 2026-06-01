;;; site.d/hermes.el -*- lexical-binding: t; -*-

(defcustom hermes-agent-url nil
  "Base URL for the Hermes agent, e.g. \"http://localhost:8080\".
Overridden by the HERMES_AGENT_URL environment variable if set."
  :type '(choice (const nil) string)
  :group 'hermes)

(defcustom hermes-agent-model nil
  "Model name to use with the Hermes agent.
Overridden by the HERMES_AGENT_MODEL environment variable if set."
  :type '(choice (const nil) string)
  :group 'hermes)

(defcustom hermes-agent-token ""
  "API token for the Hermes agent. May be empty for unauthenticated agents.
Overridden by the HERMES_AGENT_TOKEN environment variable if set."
  :type 'string
  :group 'hermes)

(defcustom hermes-local-config-file
  (expand-file-name "doom/hermes.el" (or (getenv "XDG_DATA_HOME")
                                         (expand-file-name ".local/share" "~")))
  "Path to a local Elisp file loaded at startup to configure Hermes variables.
The file may contain plain `setq' calls for `hermes-agent-url',
`hermes-agent-model', and `hermes-agent-token'."
  :type 'file
  :group 'hermes)

(when (and hermes-local-config-file
           (file-readable-p hermes-local-config-file))
  (load hermes-local-config-file nil :nomessage))

(defun hermes-agent--resolve (env-var custom-var label)
  "Return the value for a Hermes config key, or signal an error.
Tries ENV-VAR first, then CUSTOM-VAR; signals a user error using LABEL if
neither is set."
  (or (getenv env-var)
      (and (stringp custom-var) (not (string-empty-p custom-var)) custom-var)
      (user-error "Hermes: %s is not set (tried env var %s and `%s')"
                  label env-var
                  (symbol-name
                   (pcase label
                     ("URL"   'hermes-agent-url)
                     ("model" 'hermes-agent-model)
                     (_       'hermes-agent-token))))))

(after! gptel
  (defun hermes-agent-connect ()
    "Open a gptel chat buffer connected to the local Hermes agent.
Connection details are resolved in order: environment variable, then the
corresponding `hermes-agent-*' custom variable.  The config file at
`hermes-local-config-file' is loaded at startup and may set those variables."
    (interactive)
    (require 'url-parse)
    (let* ((url      (hermes-agent--resolve "HERMES_AGENT_URL"   hermes-agent-url   "URL"))
           (model    (hermes-agent--resolve "HERMES_AGENT_MODEL" hermes-agent-model "model"))
           (token    (or (getenv "HERMES_AGENT_TOKEN") hermes-agent-token ""))
           (parsed   (url-generic-parse-url url))
           (scheme   (url-type parsed))
           (host     (url-host parsed))
           (port     (url-port parsed))
           (path     (url-filename parsed))
           (hostport (if (and port (numberp port) (> port 0))
                         (format "%s:%d" host port)
                       host))
           (endpoint (if (and path
                              (not (string-empty-p path))
                              (not (string= path "/")))
                         path
                       "/v1/chat/completions"))
           (model-sym (intern model))
           (backend  (gptel-make-openai "Hermes"
                       :protocol scheme
                       :host     hostport
                       :endpoint endpoint
                       :key      (lambda () token)
                       :models   (list model-sym)))
           (buf (gptel "*Hermes*")))
      ;; gptel sets `gptel-backend' / `gptel-model' buffer-locally from the
      ;; global values during mode init; dynamic `let' bindings don't survive
      ;; that, so explicitly assign them in the new buffer.
      (with-current-buffer buf
        (setq-local gptel-backend backend
                    gptel-model   model-sym))
      (pop-to-buffer buf))))

(after! ellama
  (defun hermes-agent-connect-ellama ()
    "Open an ellama chat with the local Hermes agent.
Connection details are resolved in order: environment variable, then the
corresponding `hermes-agent-*' custom variable.  The config file at
`hermes-local-config-file' is loaded at startup and may set those variables."
    (interactive)
    (require 'llm-openai)
    ;; `ellama-chat' references `ellama-context-format', defined in
    ;; ellama-context.el and not autoloaded.
    (require 'ellama-context)
    (let* ((url      (hermes-agent--resolve "HERMES_AGENT_URL"   hermes-agent-url   "URL"))
           (model    (hermes-agent--resolve "HERMES_AGENT_MODEL" hermes-agent-model "model"))
           (token    (or (getenv "HERMES_AGENT_TOKEN") hermes-agent-token ""))
           (base-url (if (string-suffix-p "/" url) url (concat url "/"))))
      (setq ellama-provider
            (make-llm-openai-compatible
              :key        token
              :chat-model model
              :url        base-url))
      (call-interactively #'ellama-chat))))

(provide 'hermes)
