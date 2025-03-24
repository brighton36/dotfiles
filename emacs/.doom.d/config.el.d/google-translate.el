;;; google-translate.el --- Description -*- lexical-binding: t; -*-

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

  ; TODO Now let's do a translate-anywhere, with a language selection menu at kickoff
  ; TODO Let's customize the faces a bit maybe too

  (let ((input-buffer (generate-new-buffer "*google-translate*")) (buffer-name "*Google Translate*")   )
    (display-buffer input-buffer)
    (with-current-buffer input-buffer
      (set (make-local-variable 'google-translate-from-anywhere-exec-timer) nil)
      (set (make-local-variable 'after-change-functions) nil)
      (add-hook 'after-change-functions 'google-translate-from-anywhere-set-timer))
    (let ((initial-input-window (selected-window)))
      (select-window (split-window-vertically))
      (with-output-to-temp-buffer buffer-name (select-window (display-buffer buffer-name)))
      (select-window initial-input-window))
    )
  )

; This was copy-pasta from https://github.com/kawamuray/google-translate-mode/blob/master/google-translate-mode.el
; apparently, at that time of writing, there was not Google-translate-mode provided by google-translate. And
; now, since there is, there's  a collision in the names. In any case, I just wanted these functions:
(defcustom google-translate-from-anywhere-idle-wait-time 0.5
  "Seconds to wait on idle until execute translate")

(defun google-translate-from-anywhere-exec-translate()
  "Execute translate on current buffer keepking state."
  (setq google-translate-from-anywhere-exec-timer nil)
  (save-excursion
    (mark-whole-buffer)
    (google-translate-at-point)
    )
  )

(defun google-translate-from-anywhere-set-timer(a b c) ; all unused
  "Setup timer for execute translate if it is still not set."
  (unless google-translate-from-anywhere-exec-timer
    (setq google-translate-from-anywhere-exec-timer
          (run-with-idle-timer google-translate-from-anywhere-idle-wait-time nil
                               'google-translate-from-anywhere-exec-translate)))
  )

