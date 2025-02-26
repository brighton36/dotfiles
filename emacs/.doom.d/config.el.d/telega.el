;-*- mode: elisp -*-

; These settings are needed to support : https://zevlg.github.io/telega.el/#settings-for-emacs-as-daemon
(setq telega-use-images 1
      telega-emoji-font-family "Noto Color Emoji"
      telega-emoji-use-images 1
      telega-online-status-function #'telega-focus-state)

; This line was needed in order for the systemd based daemon to work
(require 'telega)

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
