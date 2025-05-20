;;; ruby.el --- Description -*- lexical-binding: t; -*-

(add-hook 'compilation-finish-functions
          (lambda (buf strg)
            (switch-to-buffer-other-window "*compilation*")
            (read-only-mode)
            (goto-char (point-max))
            (local-set-key (kbd "q")
                           (lambda () (interactive) (quit-restore-window)))))
