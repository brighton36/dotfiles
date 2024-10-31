;-*- mode: elisp -*-

; These frame params were all necessary to get the xmonad doCenterFloat to work:
(setq emacs-everywhere-frame-name-format "Emacs Everywhere"
      emacs-everywhere-frame-parameters '(
                                          (name . "Emacs Everywhere")
                                          (width . 120)
                                          (height . 40)
                                          (minibuffer . t)
                                          (menu-bar-lines . t))
      emacs-everywhere-paste-command (list "/usr/bin/xdotool" 
                                           "key" "--clearmodifiers" "Shift+Insert"
                                           "sleep" "0.25" 
                                           "keyup" "Meta_L" "Meta_R" "Alt_L" "Alt_R" "Super_L" "Super_R")
)

(remove-hook 'emacs-everywhere-init-hooks 'emacs-everywhere-set-frame-position)
