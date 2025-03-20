;;; gptel.el --- Description -*- lexical-binding: t; -*-

(gptel-make-ollama "Ollama" :host "localhost:11434" :stream t :models '(mistral:latest))
(gptel-make-deepseek "DeepSeek" :stream t :key (get-secret 'deepseek-api-key) :models '(deepseek-reasoner))

(setq
; gptel-model 'mistral:latest
 gptel-default-mode 'markdown-mode
; gptel-backend "Ollama"
 gptel-display-buffer-action nil)

; TODO
;(transient-suffix-put 'gptel-menu (kbd "-m") :key "M")

(defvar my/gptel-presets
  ;; PRESET NAME                   MODEL               BACKEND NAME   DIRECTIVE
  '(("Ollama:mistral:latest"       'mistral:latest     "Ollama"       "My local minstral llm")
    ("DeepSeek:deepseek-reasoner"  'deepseek-reasoner  "DeepSeek"     "Default directive for Deepseek")
    ("ChatGPT:gpt-3.5-turbo"       'gpt-3.5-turbo      "ChatGPT"      "Default directive for ChatGPT"))
  "List of preset combinations for gptel.")

(defun my/gptel-switch-preset (preset)
  (interactive (list (completing-read
                      "Select preset: "
                      (mapcar #'car my/gptel-presets)
                      nil t)))
  (let ((combination (assoc preset my/gptel-presets)))
    (setq gptel-model (nth 1 combination)
          gptel-backend (cdr (assoc (nth 2 combination) gptel--known-backends))
          gptel--system-message (nth 3 combination))
    (message "Selected gptel preset: %s" preset)))

(defun my/llm-from-anywhere ()
  (interactive)
  ; This loads the frame without the dashboard...
  ;; (let ((buffer (generate-new-buffer "*gptel")))
  ;; (set-buffer-major-mode buffer)
  ;; (display-buffer buffer
  ;;   '(display-buffer-pop-up-frame . ( (pop-up-frame-parameters
  ;;                                        (name . "emacs-gptel-popup")
  ;;                                        (width . 150)
  ;;                                        (height . 40)
  ;;                                        (minibuffer . f))
  ;;                                     ) )))

  (make-frame '((name . "emacs-gptel-popup") (width . 150) (height . 40) (minibuffer . f)))
  (select-frame-by-name "emacs-gptel-popup")

  (call-interactively 'my/gptel-switch-preset)

  (add-hook 'gptel-post-response-hook (lambda () (goto-char (point-max))))
  (gptel "*gptel" gptel-api-key nil)
  (switch-to-buffer "*gptel")
  (delete-other-windows)
  ; TODO: Delete the session on destroy.. I think
)
