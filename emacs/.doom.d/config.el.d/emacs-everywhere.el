;;; emacs-everywhere.el --- Description -*- lexical-binding: t; -*-

(require 'emacs-everywhere)
(require 'json)

; For reasons I can't explain, emacs seems to get stale environment variables when
; run in systemd. Which, cause us problems below. This will manually set the
; correct hyprland instance:
(let ((instances (shell-command-to-string "/usr/bin/hyprctl instances")))
  (save-match-data
    (and (string-match "\\`instance \\([^:\n]+\\):" instances)
        (setenv "HYPRLAND_INSTANCE_SIGNATURE" (match-string 1 instances) )))
  )

(defun emacs-everywhere--app-info-linux-hyprland ()
  "Return information on the current active window, on a Linux Hyprland session."
  (let* ((json-string (emacs-everywhere--call "/usr/bin/hyprctl" "-j" "activewindow"))
         (json-object (json-read-from-string json-string))
         (window-id (cdr (assoc 'address json-object)))
         (app-class (cdr (assoc 'class json-object)))
         (window-title (cdr (assoc 'title json-object)))
         (window-geometry (list (aref (cdr (assoc 'at json-object)) 0)
                                (aref (cdr (assoc 'at json-object)) 1)
                                (aref (cdr (assoc 'size json-object)) 0)
                                (aref (cdr (assoc 'size json-object)) 1))))
    (make-emacs-everywhere-app
     :id window-id
     :class app-class
     :title window-title
     :geometry window-geometry)))

(add-to-list 'emacs-everywhere-system-configs
  '((wayland . Hyprland)
     :paste-command ("/usr/bin/ydotool" "key" "58:1" "42:1" "52:1" "52:0" "42:0" "58:0")
     :focus-command ("/usr/bin/hyprctl" "dispatch" "focuswindow" "address:%w")
     :info-function emacs-everywhere--app-info-linux-hyprland))

(setq emacs-everywhere-frame-name-format "Emacs Everywhere"
      emacs-everywhere-frame-parameters '(
                                          (name . "Emacs Everywhere")
                                          (width . 120)
                                          (height . 40)
                                          (minibuffer . t)
                                          (menu-bar-lines . t))
      emacs-everywhere-major-mode-function #'text-mode
      ; NOTE: this was working in xmonad, and ... I'm keeping it here for now...
      ;emacs-everywhere-paste-command (list "/usr/bin/xdotool" 
      ;                                     "key" "--clearmodifiers" "Shift+Insert"
      ;                                     "sleep" "0.25" 
      ;                                     "keyup" "Meta_L" "Meta_R" "Alt_L" "Alt_R" "Super_L" "Super_R")
      ; /usr/include/linux/input-event-codes.h:
      ; 29 is leftctl, 42 is leftshift, 47 is v. The number to right of the colon is the state (on or off)
      ; For us, we want KEY_DOT @ 52 as the Dvorak v, and KEY_CAPSLOCK @ 58 for ctrl
)

; NOTE: These lines were all necessary to get the xmonad doCenterFloat to work:
;(setq-default auto-fill-function 'do-auto-fill)

;(remove-hook 'emacs-everywhere-init-hooks 'emacs-everywhere-set-frame-position)
