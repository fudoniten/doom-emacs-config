;;; site.d/kirocrew.el -*- lexical-binding: t; -*-

;; Talk to a local Kiro Crew gateway from Emacs.
;;
;; Kiro Crew's dashboard gateway exposes an OpenAI-compatible endpoint at
;; POST <base>/v1/chat/completions.  The `model' field is overloaded as the
;; AGENT/crew-member selector: set it to a crew name (e.g. "default") to
;; reach that member -- the one carrying its own persistent memory and DM
;; thread -- rather than a stateless model.  Other agent names like "router"
;; or "lite" are also valid.  This is distinct from the agent-shell/ACP path,
;; which drives bare kiro-cli and gets none of the crew layer.
;;
;; Auth: the gateway requires a dashboard bearer token on every request,
;; even on loopback.  Mint one yourself in a terminal with `kirocrew token'
;; (that command is a credential mint -- keep its output out of this repo).
;; Tokens expire, so expect to refresh the value periodically.
;;
;; Configuration resolves in this order, matching hermes.el:
;;   1. environment variable  (KIROCREW_AGENT_URL / _TOKEN / _MODEL)
;;   2. the matching `kirocrew-agent-*' custom variable
;;   3. `kirocrew-local-config-file', loaded at startup -- the recommended
;;      home for the token.  It lives under $XDG_DATA_HOME/doom/, OUTSIDE
;;      this (version-controlled) repo, so a secret never lands in git.

(defcustom kirocrew-agent-url "http://localhost:5477"
  "Base URL for the Kiro Crew LOCAL REFRESH PROXY.
The OpenAI-compatible endpoint is <url>/v1/chat/completions.

This points at the loopback refresh proxy (default port 5477), NOT the
gateway directly (5476).  The gateway's auth is browser-shaped: it reads
the credential from a `?token=' query param or the `mc_token_<port>'
cookie (never an Authorization: Bearer header), and `kirocrew token'
mints only a 5-minute link token, not a durable API key.  The proxy
holds a real session, auto-rotates it via POST /api/auth/refresh, and
re-exposes a stable, unauthenticated, loopback-only OpenAI endpoint.
See ~/.config/kirocrew-proxy/proxy.py.

Overridden by the KIROCREW_AGENT_URL environment variable if set."
  :type 'string
  :group 'kirocrew)

(defcustom kirocrew-agent-model "default"
  "Kiro Crew agent/crew name to talk to (sent as the OpenAI `model' field).
\"default\" reaches the default crew member with its persistent memory;
other registered agent names (e.g. \"router\", \"lite\") also work.
Overridden by the KIROCREW_AGENT_MODEL environment variable if set."
  :type 'string
  :group 'kirocrew)

(defcustom kirocrew-agent-token ""
  "Dashboard token for the Kiro Crew gateway.  OPTIONAL with the proxy.
When `kirocrew-agent-url' points at the local refresh proxy (the
default), NO token is needed -- the proxy holds and rotates the real
session itself, and this value is ignored.  Only set it if you repoint
the URL straight at the gateway, in which case it must be a currently
valid session token (not a 5-minute `kirocrew token' link token).
Overridden by the KIROCREW_AGENT_TOKEN environment variable if set.

Do NOT commit a real token: prefer the environment variable or
`kirocrew-local-config-file', both of which live outside this repo."
  :type 'string
  :group 'kirocrew)

(defcustom kirocrew-local-config-file
  (expand-file-name "doom/kirocrew.el" (or (getenv "XDG_DATA_HOME")
                                           (expand-file-name ".local/share" "~")))
  "Path to a local Elisp file loaded at startup to configure Kiro Crew.
The file may contain plain `setq' calls for `kirocrew-agent-url',
`kirocrew-agent-model', and `kirocrew-agent-token'.  It is deliberately
OUTSIDE the doom-emacs-config repo so the token stays out of version
control."
  :type 'file
  :group 'kirocrew)

(when (and kirocrew-local-config-file
           (file-readable-p kirocrew-local-config-file))
  (load kirocrew-local-config-file nil :nomessage))

(defun kirocrew-agent--resolve (env-var custom-var label)
  "Return the value for a Kiro Crew config key, or signal an error.
Tries ENV-VAR first, then CUSTOM-VAR; signals a user error using LABEL if
neither is set."
  (or (getenv env-var)
      (and (stringp custom-var) (not (string-empty-p custom-var)) custom-var)
      (user-error "Kiro Crew: %s is not set (tried env var %s and `%s')"
                  label env-var
                  (symbol-name
                   (pcase label
                     ("URL"   'kirocrew-agent-url)
                     ("model" 'kirocrew-agent-model)
                     (_       'kirocrew-agent-token))))))

(defun kirocrew-agent--token ()
  "Return the Kiro Crew bearer token, or signal a user error if unset.
The gateway rejects an empty token, so -- unlike Hermes -- refuse to open
a doomed connection and point the user at `kirocrew token' instead."
  (let ((tok (or (getenv "KIROCREW_AGENT_TOKEN")
                 (and (stringp kirocrew-agent-token) kirocrew-agent-token)
                 "")))
    ;; The local refresh proxy (the default target) is UNAUTHENTICATED on
    ;; loopback and ignores whatever key gptel sends, so an empty token is
    ;; fine -- return a harmless placeholder to satisfy gptel's `:key'
    ;; contract.  A real token only matters if `kirocrew-agent-url' is
    ;; repointed straight at the gateway via `?token=' (not the default).
    (if (string-empty-p tok) "unused-local-proxy" tok)))

(defun kirocrew-agent--endpoint (url)
  "Return the chat-completions endpoint path for base URL.
Accepts a bare base (\"http://host:port\"), one already ending in a path,
or one already pointing at /chat/completions."
  (let* ((parsed (url-generic-parse-url url))
         (path   (or (url-filename parsed) ""))
         (base   (if (string-empty-p path)
                     ""
                   (replace-regexp-in-string "/+\\'" "" path))))
    (cond
     ((string-suffix-p "/chat/completions" base) base)
     ((string-empty-p base) "/v1/chat/completions")
     (t (concat base "/chat/completions")))))

(after! gptel
  (defun kirocrew-agent-connect ()
    "Open a gptel chat buffer connected to the local Kiro Crew gateway.
Connection details resolve in order: environment variable, then the
corresponding `kirocrew-agent-*' custom variable.  The config file at
`kirocrew-local-config-file' is loaded at startup and may set those
variables.  The `model' is the crew/agent name (default: \"default\")."
    (interactive)
    (require 'url-parse)
    (let* ((url       (kirocrew-agent--resolve "KIROCREW_AGENT_URL"
                                               kirocrew-agent-url "URL"))
           (model     (kirocrew-agent--resolve "KIROCREW_AGENT_MODEL"
                                               kirocrew-agent-model "model"))
           ;; Resolve the token now so a missing one errors before we open a
           ;; buffer, but capture it in the closure below (gptel calls `:key'
           ;; lazily on each request).
           (parsed    (url-generic-parse-url url))
           (scheme    (url-type parsed))
           (host      (url-host parsed))
           (port      (url-port parsed))
           (hostport  (if (and port (numberp port) (> port 0))
                          (format "%s:%d" host port)
                        host))
           (endpoint  (kirocrew-agent--endpoint url))
           (model-sym (intern model))
           (backend   (gptel-make-openai "KiroCrew"
                        :protocol scheme
                        :host     hostport
                        :endpoint endpoint
                        :stream   t
                        :key      #'kirocrew-agent--token
                        :models   (list model-sym)))
           (buf       (gptel "*KiroCrew*")))
      ;; Fail fast if the token is unset, rather than on first send.
      (kirocrew-agent--token)
      ;; gptel sets `gptel-backend' / `gptel-model' buffer-locally from the
      ;; global values during mode init; dynamic `let' bindings don't survive
      ;; that, so explicitly assign them in the new buffer.
      (with-current-buffer buf
        (setq-local gptel-backend backend
                    gptel-model   model-sym))
      (pop-to-buffer buf))))

(after! ellama
  (defun kirocrew-agent-connect-ellama ()
    "Open an ellama chat connected to the local Kiro Crew gateway.
Connection details resolve in order: environment variable, then the
corresponding `kirocrew-agent-*' custom variable.  The config file at
`kirocrew-local-config-file' is loaded at startup and may set those
variables."
    (interactive)
    (require 'llm-openai)
    ;; `ellama-chat' references `ellama-context-format', defined in
    ;; ellama-context.el and not autoloaded.
    (require 'ellama-context)
    (let* ((url      (kirocrew-agent--resolve "KIROCREW_AGENT_URL"
                                              kirocrew-agent-url "URL"))
           (model    (kirocrew-agent--resolve "KIROCREW_AGENT_MODEL"
                                              kirocrew-agent-model "model"))
           (token    (kirocrew-agent--token))
           (base-url (if (string-suffix-p "/" url) url (concat url "/"))))
      (setq ellama-provider
            (make-llm-openai-compatible
              :key        token
              :chat-model model
              :url        base-url))
      (call-interactively #'ellama-chat))))

(provide 'kirocrew)
