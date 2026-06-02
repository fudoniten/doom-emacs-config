;;; site.d/gptel.el -*- lexical-binding: t; -*-

(after! gptel
  ;; Replace the built-in OpenAI/ChatGPT default with OpenRouter, which
  ;; exposes the same API but gives access to many models, including free
  ;; ones (those with the :free suffix).  Set OPENROUTER_API_KEY in your
  ;; environment; HERMES_AGENT_TOKEN is tried as a fallback since it is
  ;; the same key if you are already using OpenRouter for the Hermes agent.
  (setq gptel-backend
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
        gptel-model 'meta-llama/llama-3.3-70b-instruct:free))

(provide 'gptel-config)
