;;; agent-shell.el -*- lexical-binding: t; -*-

(require 'acp)
(require 'agent-shell)

(setq
  agent-shell-inhibit-system-sleep nil
  agent-shell-anthropic-claude-environment (agent-shell-make-environment-variables "ANTHROPIC_API_KEY" (get-secret 'anthropic-api-key))
  agent-shell-anthropic-authentication (agent-shell-anthropic-make-authentication :api-key (get-secret 'anthropic-api-key)))

(map!
  :map agent-shell-mode-map
  :n "<return>" 'agent-shell-ui-toggle-fragment
  :n "C-<return>" 'agent-shell-ui-toggle-all-fragments)

;; agent-shell buffers abort the daemon with a FATAL in src/bidi.c (the bidi
;; iterator cache losing its 1:1 correspondence with the buffer text) while
;; redisplaying streamed markdown output.  Emacs only skips the bidi engine when
;; `bidi-display-reordering' is nil -- see init_iterator in xdisp.c:
;;   it->bidi_p = (!redisplay--inhibit-bidi
;;                 && !NILP (BVAR (current_buffer, bidi_display_reordering))
;;                 && it->multibyte_p);
;; `left-to-right' + `bidi-inhibit-bpa' are the values Doom already sets
;; globally, so setting them here changes nothing and does NOT avoid the abort.
;; Trade-off: RTL text in these buffers is displayed in logical order.
(defun agent-shell-buffer-ltr-only ()
  "Skip the bidi reordering engine in agent-shell/ACP buffers.
Works around a FATAL abort in src/bidi.c during redisplay of streamed output."
  (setq-local bidi-display-reordering nil)
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t))
(add-hook 'agent-shell-mode-hook #'agent-shell-buffer-ltr-only)
