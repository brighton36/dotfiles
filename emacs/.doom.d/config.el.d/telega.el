;-*- mode: elisp -*-

; These settings are needed to support : https://zevlg.github.io/telega.el/#settings-for-emacs-as-daemon
(setq telega-use-images 1
      telega-emoji-font-family "Noto Color Emoji"
      telega-emoji-use-images 1
      telega-online-status-function #'telega-focus-state)

; This line was needed in order for the systemd based daemon to work
(require 'telega) ; This has to load after the setq, or else we don't seem to use-images...

; TODO Let's maybe show the age in a separate column, using marginalia? Use the file entry as a guide
; https://github.com/minad/marginalia/blob/main/marginalia.el#L87 maybe use  annotation-function in
; the completing-read... or see if telega-completing-read supports annotation (pretty sure it does)
; More details here: https://kisaragi-hiu.com/emacs-completion-metadata
; and here: https://emacs.stackexchange.com/questions/74547/completing-read-search-also-in-annotations
(defun my-read-screenshot ()
  (let ((screenshotspath "~/Pictures/Screenshots"))
    (let ((filename (telega-completing-read "Select a screenshot: "
                    (cl-remove-if (lambda (k) (string-match-p "^\\." k))
                      (reverse
                        (mapcar #'car
                            (sort (directory-files-and-attributes screenshotspath)
                                  #'(lambda (x y) (time-less-p (nth 6 x) (nth 6 y))))))))
         ))
      (when filename (concat screenshotspath "/" filename))
    )
  )
)

(defun my-telega-chatbuf-attach-screenshot (&optional n chat)
  (interactive (list (or current-prefix-arg 1) telega-chatbuf--chat))

    (let ((tmpfile (my-read-screenshot)))
      (when (file-exists-p tmpfile)
        (x-focus-frame (window-frame (get-buffer-window)))
        (telega-chatbuf-attach-media tmpfile)))
  )

(assoc-delete-all "screenshot" telega-chat-attach-commands)
(add-to-list 'telega-chat-attach-commands '("screenshot" (my-permission :can_send_photos) my-telega-chatbuf-attach-screenshot) )

(map!
  :g "C-c t" telega-prefix-map

  :after telega
      :map telega-chat-mode-map
      "C-S-r" 'telega-msg-reply
      "C-S-e" 'telega-msg-edit
      :n "C-g" 'telega-chatbuf-cancel-aux
      :n "q" 'telega

      :map telega-msg-button-map 
      "e" 'telega-msg-edit)

(setq telega-server-libs-prefix "/usr"
      telega-use-images 1
      telega-filter-button-width 10)

(telega-notifications-mode 1)
; If you decide you want the systray icon, uncomment this:
;(telega-appindicator-mode 1)

;; This sets our messages to wrap by default
(add-hook 'telega-chat-mode-hook 'visual-line-mode)

(set-face-attribute 'telega-msg-inline-reply nil :foreground "#556b72")
(set-face-attribute 'telega-shadow nil :foreground "#556b72")
