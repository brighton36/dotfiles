;-*- mode: elisp -*-

; TODO: I prefer this over dired, but it's not working...
(defun emacs-dirvish-popup ()
  "Open dirvish in a popup frame."
  (interactive)
  (with-selected-frame
    (make-frame '((name . "emacs-dirvish-popup")
                  (minibuffer . only)
                  (fullscreen . 0) ; no fullscreen
                  (undecorated . t) ; remove title bar
                  ;;(auto-raise . t) ; focus on this frame
                  ;;(tool-bar-lines . 0)
                  ;;(menu-bar-lines . 0)
                  ;(internal-border-width . 10)
                  (width . 80)
                  (height . 11)))

                  (command-execute 'dirvish)
                  ))
