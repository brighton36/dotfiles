;;; gptel.el --- Description -*- lexical-binding: t; -*-

(gptel-make-ollama "Ollama" :host "localhost:11434" :stream t :models '(mistral:latest))
(gptel-make-deepseek "DeepSeek" :stream t :key (get-secret 'deepseek-api-key) :models '(deepseek-reasoner))
(gptel-make-anthropic "Anthropic" :stream t :key (get-secret 'anthropic-api-key) :models '(claude-3-7-sonnet-20250219))
(gptel-make-openai "Groq" :host "api.groq.com" :endpoint "/openai/v1/chat/completions" :stream t :key (get-secret 'groq-api-key) :models '(llama-3.1-8b-instant))
(gptel-make-gemini "Gemini" :key (get-secret 'gemini-api-key) :stream t :models '(gemini-2.0-flash))
(gptel-make-kagi "Kagi" :key (get-secret 'kagi-api-key)  :models '(fastgpt)) ; NOTE: stream not supported

(setq
 gptel-default-mode 'markdown-mode
 gptel-api-key (get-secret 'openai-api-key)
 ; Default to Ollama:mistral for now
 gptel-model 'mistral:latest
 gptel-backend (cdr (assoc "Ollama" gptel--known-backends))
 gptel-display-buffer-action nil)

; NOTE: It's probable we want gptel-menu bound to something convenient. I think... this command also
; splits the window sometimes, not sure whats up with that
;(transient-suffix-put 'gptel-menu (kbd "-m") :key "M")

(defvar my/gptel-presets
  ;; PRESET NAME                              MODEL                        BACKEND NAME   DIRECTIVE
  '(("Ollama:mistral:latest"                  'mistral:latest              "Ollama"       "My local minstral llm")
    ("DeepSeek:deepseek-reasoner"             'deepseek-reasoner           "DeepSeek"     "Default directive for Deepseek")
    ("Anthropic:claude-3-7-sonnet-20250219"   'claude-3-7-sonnet-20250219  "Anthropic"    "The aiderchat champion")
    ("Groq:llama-3.1-8b-instant"              'llama-3.1-8b-instant        "Groq"         "Meta's biggest's model on Groq")
    ("Gemini:gemini-2.0-flash"                'gemini-2.0-flash            "Gemini"       "Google's flagship Gemini model")
    ("Kagi:fastgpt"                           'fastgpt                     "Kagi"         "Kagi's non-streaming general model")
    ("ChatGPT:gpt-4-turbo"                    'gpt-4-turbo                 "ChatGPT"      "Default directive for ChatGPT"))
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

; Copy pasta'd from: https://www.emacswiki.org/emacs/frame-fns.el
(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame  (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

(defun get-a-frame (frame)
  "Return a frame, if any, named FRAME (a frame or a string).
If none, return nil.
If FRAME is a frame, it is returned."
  (cond ((framep frame) frame)
        ((stringp frame)
         (catch 'get-a-frame-found
           (dolist (fr (frame-list))
             (when (string= frame (get-frame-name fr))
               (throw 'get-a-frame-found fr)))
           nil))
        (t (error
            "Function `get-frame-name': Arg neither a string nor a frame: `%s'"
            frame))))

(defun my/llm-from-anywhere ()
  (interactive)
  ; This loads the frame without the dashboard, probably useful elsewhere.... see if it works better with
  ; minibuffer enabled?
  ;; (let ((buffer (generate-new-buffer "*gptel")))
  ;; (set-buffer-major-mode buffer)
  ;; (display-buffer buffer
  ;;   '(display-buffer-pop-up-frame . ( (pop-up-frame-parameters
  ;;                                        (name . "emacs-gptel-popup")
  ;;                                        (width . 150)
  ;;                                        (height . 40)
  ;;                                        (minibuffer . f))
  ;;                                     ) )))

  (if (not (get-a-frame "emacs-gptel-popup"))
    (make-frame '((name . "emacs-gptel-popup") (width . 150) (height . 40) (minibuffer . f))))
  (select-frame-by-name "emacs-gptel-popup")
  ; TODO: Maybe exec the hypr-helper to assign the window to the active workspace?
  (generate-new-buffer "*gptel*")

  (call-interactively 'my/gptel-switch-preset)

  (add-hook 'gptel-post-response-hook (lambda () (goto-char (point-max))))
  (gptel "*gptel*" gptel-api-key nil)
  (switch-to-buffer "*gptel*")
  (delete-other-windows)
)
