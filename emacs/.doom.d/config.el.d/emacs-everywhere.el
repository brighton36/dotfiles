;-*- mode: elisp -*-

(require 'json)
(defun emacs-everywhere--app-info-linux-hyprland ()
  "Return information on the current active window, on a Linux Hyprland session."
  (let* ((json-string (emacs-everywhere--call "hyprctl" "-j" "activewindow"))
         (json-object (json-read-from-string json-string))
         (window-id (cdr (assoc 'address json-object)))
         (app-name (cdr (assoc 'class json-object)))
         (window-title (cdr (assoc 'title json-object)))
         (window-geometry (list (aref (cdr (assoc 'at json-object)) 0)
                                (aref (cdr (assoc 'at json-object)) 1)
                                (aref (cdr (assoc 'size json-object)) 0)
                                (aref (cdr (assoc 'size json-object)) 1))))
    (make-emacs-everywhere-app
     :id window-id
     :class app-name
     :title window-title
     :geometry window-geometry)))

; These frame params were all necessary to get the xmonad doCenterFloat to work:
(setq emacs-everywhere-frame-name-format "Emacs Everywhere"
      emacs-everywhere-window-focus-command (list "hyprctl" "dispatch" "focuswindow" "address:%w")
      emacs-everywhere-app-info-function #'emacs-everywhere--app-info-linux-hyprland

      emacs-everywhere-frame-parameters '(
                                          (name . "Emacs Everywhere")
                                          (width . 120)
                                          (height . 40)
                                          (minibuffer . t)
                                          (menu-bar-lines . t))
      ;emacs-everywhere-paste-command (list "/usr/bin/xdotool" 
      ;                                     "key" "--clearmodifiers" "Shift+Insert"
      ;                                     "sleep" "0.25" 
      ;                                     "keyup" "Meta_L" "Meta_R" "Alt_L" "Alt_R" "Super_L" "Super_R")
      emacs-everywhere-paste-command (list "/usr/bin/ydotool" "key" "29:1" "42:1" "47:1" "47:0" "42:0" "29:0")
)

(remove-hook 'emacs-everywhere-init-hooks 'emacs-everywhere-set-frame-position)
