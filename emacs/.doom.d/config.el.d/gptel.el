;;; gptel.el --- Description -*- lexical-binding: t; -*-

(gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '(mistral:latest))          ;List of models

(setq
 gptel-model 'mistral:latest
 gptel-default-mode 'text-mode
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '(mistral:latest))
 ; This fixes a bug that prevents the gptel command from executing
 gptel-display-buffer-action nil
 )
