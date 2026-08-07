;;; my-functions.el --- Description -*- lexical-binding: t; -*-

;(defun emacs-dirvish-popup ()
;  "Open dirvish in a popup frame."
;  (interactive)
;  (with-selected-frame
;    (make-frame '((name . "emacs-dirvish-popup")
;                  (minibuffer . only)
;                  (fullscreen . 0) ; no fullscreen
;                  (undecorated . t) ; remove title bar
;                  ;;(auto-raise . t) ; focus on this frame
;                  ;;(tool-bar-lines . 0)
;                  ;;(menu-bar-lines . 0)
;                  ;(internal-border-width . 10)
;                  (width . 80)
;                  (height . 11)))
;
;                  (command-execute 'dirvish)
;                  ))

;; From : https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(defun mp-toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
    (not (window-dedicated-p (selected-window)))))

(defun vterm-new (&optional arg)
  "Open a new instance of eshell."
  (interactive)
  (vterm--internal #'switch-to-buffer arg)
  )

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
          (filename (buffer-file-name))
          (dir
            (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
          (newname (concat dir "/" name)))

    (if (not filename)
      (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)    t))))
