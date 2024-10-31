(setq explain-pause-top-auto-refresh-interval 1)

(defun my/get-frame-by-name (fname)
  "If there is a frame named FNAME, return it, else nil."
  (require 'dash)                       ; For `-some'
  (-some (lambda (frame)
           (when (equal fname (frame-parameter frame 'name))
             frame))
         (frame-list)))

(defun open-explain-pause-top-in-new-frame ()
  "Open `explain-pause-top` in a new frame."
  (interactive)
  (setq topframe (my/get-frame-by-name "Emacs top"))
  (if topframe
    (select-frame-set-input-focus topframe)
    (let ((new-frame (make-frame '((name . "Emacs top")) )))
      (select-frame new-frame)
      (call-interactively #'explain-pause-top))
    )
  )

