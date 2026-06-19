;;; agent-shell.el -*- lexical-binding: t; -*-

(require 'acp)
(require 'agent-shell)

(setq agent-shell-anthropic-claude-environment
  (agent-shell-make-environment-variables
    "ANTHROPIC_API_KEY" (get-secret 'anthropic-api-key)))

(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :api-key (get-secret 'anthropic-api-key)))

(map!
  :map agent-shell-mode-map
  :n "<return>" 'agent-shell-ui-toggle-fragment
  :n "C-<return>" 'agent-shell-ui-toggle-all-fragments)
