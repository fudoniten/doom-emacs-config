;;; site.d/ellama.el -*- lexical-binding: t; -*-

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

(defun ellama-setup-ollama (model)
  "Configure ellama to use a local Ollama instance and open a chat buffer.
Prompts for MODEL name (e.g. llama3.2, mistral, codellama).
Sets `ellama-provider' for the current session; to persist across
restarts, add the equivalent `setq' to your personal config."
  (interactive (list (read-string "Ollama model: " "llama3.2")))
  (require 'llm-ollama)
  (setq ellama-provider (make-llm-ollama :chat-model model))
  (message "ellama: using Ollama model %S" model)
  (call-interactively #'ellama-chat))

(provide 'ellama-config)
