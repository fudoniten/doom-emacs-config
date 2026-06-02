;;; site.d/gptel.el -*- lexical-binding: t; -*-

;; Force-load gptel and its OpenAI backend module before configuring,
;; rather than relying on `after!' / `with-eval-after-load' (which can
;; silently fail to fire depending on how gptel was installed).
(require 'gptel)
(require 'gptel-openai)

;; Replace the built-in OpenAI/ChatGPT default with OpenRouter, which
;; exposes the same API but gives access to many models, including free
;; ones (those with the :free suffix).  Set OPENROUTER_API_KEY in your
;; environment.
(defvar my/gptel-openrouter-backend
  (gptel-make-openai "OpenRouter"
    :host     "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :key      (lambda ()
                (or (getenv "OPENROUTER_API_KEY")
                    (user-error
                     (concat "No OpenRouter API key found.  "
                             "Set OPENROUTER_API_KEY in your environment."))))
    :models   '(;; Free tier
                meta-llama/llama-3.1-8b-instruct:free
                meta-llama/llama-3.3-70b-instruct:free
                google/gemma-3-27b-it:free
                mistralai/mistral-7b-instruct:free
                deepseek/deepseek-r1:free
                deepseek/deepseek-chat-v3-0324:free
                microsoft/phi-4-reasoning:free
                ;; Paid
                anthropic/claude-sonnet-4-5
                anthropic/claude-3-5-haiku
                openai/gpt-4o
                openai/gpt-4o-mini
                google/gemini-2.0-flash-001
                meta-llama/llama-3.3-70b-instruct))
  "OpenRouter backend for gptel, registered at load time.")

(setq-default gptel-backend my/gptel-openrouter-backend
              gptel-model   'meta-llama/llama-3.3-70b-instruct:free)

(message "gptel: default backend set to OpenRouter (%s)"
         (gptel-backend-name gptel-backend))

(provide 'gptel-config)
