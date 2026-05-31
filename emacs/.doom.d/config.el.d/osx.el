;;; osx.el --- Description -*- lexical-binding: t; -*-

; Native OSX full screen
(setq ns-use-native-fullscreen t)

; Hide the titlebar:
(add-to-list 'default-frame-alist '(undecorated . t))

; GPG seems to need this
(setenv "GPG_AGENT_INFO" nil)
(setq epg-gpg-program "/opt/homebrew/opt/gnupg@2.2/bin/gpg")
