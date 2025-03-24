;;; gptel.el --- Description -*- lexical-binding: t; -*-

(setq
  google-translate-default-source-language "en"
  google-translate-default-target-language "es"
  ;google-translate-input-method-auto-toggling t

  google-translate-show-phonetic t
  google-translate-output-destination nil
  google-translate-pop-up-buffer-set-focus nil
  )

(defun google-translate-from-anywhere ()
  (interactive)

  (if (not (get-a-frame "emacs-google-translate-popup"))
    (make-frame '((name . "emacs-google-translate-popup") (width . 150) (height . 40) (minibuffer . f))))
  (select-frame-by-name "emacs-google-translate-popup")
  ; TODO: Maybe exec the hypr-helper to assign the window to the active workspace?

  (let ((input-buffer (generate-new-buffer "*google-translate*")) (buffer-name "*Google Translate*")   )
    ; TODO Can I roll this into the above let?
    (display-buffer input-buffer)
    (with-current-buffer input-buffer
      (set (make-local-variable 'interactive-google-translate-mode-exec-timer) nil)
      (set (make-local-variable 'after-change-functions) nil)
      (add-hook 'after-change-functions 'interactive-google-translate-mode-set-timer))
    ;(add-hook 'text-mode-hook #'the-function-to-add nil t)
    (let ((initial-input-window (selected-window)))
      (select-window (split-window-vertically))
      (with-output-to-temp-buffer buffer-name (select-window (display-buffer buffer-name)))
      (select-window initial-input-window))
    )
  )

; TODO Now let's do a translate-anywhere, with a language selection menu at kickoff
;
;; ; TODO fancier top-bar, like gptel, also put the google mode from Fundamental to Google Translate


; This was copy-pasta'd from https://github.com/kawamuray/google-translate-mode/blob/master/google-translate-mode.el
; apparently, at that time of writing, there was not Google-translate-mode provided by
; google-translate. And now, since there is, there's  a collision in the names
(defconst interactive-google-translate-mode-version "0.01"
  "Google translate mode version number.")

(defvar interactive-google-translate-mode-hook nil
  "Hook run when entering Google translate mode")

(defcustom interactive-google-translate-mode-idle-wait-time 0.5
  "Seconds to wait on idle until execute translate")

(defun interactive-google-translate-mode-exec-translate()
  "Execute translate on current buffer keepking state."
  (setq interactive-google-translate-mode-exec-timer nil)
  (save-excursion
    (mark-whole-buffer)
    (google-translate-at-point)
    )
  )

(defun interactive-google-translate-mode-set-timer(a b c) ; all unused
  "Setup timer for execute translate if it is still not set."
  (unless interactive-google-translate-mode-exec-timer
    (setq interactive-google-translate-mode-exec-timer
          (run-with-idle-timer interactive-google-translate-mode-idle-wait-time nil
                               'interactive-google-translate-mode-exec-translate)))
  )

(defun interactive-google-translate-mode-enable()
  "Enable interactive-google-translate-mode."
  ; TODO If I don't use the -local, then we workish...
  (set (make-local-variable 'interactive-google-translate-mode-exec-timer) nil)
  (set (make-local-variable 'after-change-functions) nil)
  (add-hook 'after-change-functions 'interactive-google-translate-mode-set-timer)
  )

(defun interactive-google-translate-mode-disable()
  "Disable interactive-google-translate-mode."
  (kill-local-variable 'interactive-google-translate-mode-exec-timer)
  (kill-local-variable 'after-change-functions)
  (remove-hook 'after-change-functions 'interactive-google-translate-mode-set-timer)
  )

(define-minor-mode interactive-google-translate-mode
  "Minor mode for displaying translated text using Google Translate in real time."
  :lighter " GoogleTranslate"
  (if interactive-google-translate-mode
      (interactive-google-translate-mode-enable)
    (interactive-google-translate-mode-disable))
  )
