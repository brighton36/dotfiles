;-*- mode: elisp -*-
(setq eshell-save-history-on-exit t
      eshell-scroll-to-bottom-on-input 'all
      eshell-prefer-lisp-functions nil
      eshell-plain-grep-behavior t
      eshell-hist-ignoredups t
      eshell-banner-message ""
      eshell-highlight-prompt nil
      eshell-prompt-function (lambda nil
                               (concat
                                 (propertize (user-login-name) 'face `(:foreground ,(doom-color 'green)))
                                 "@"
                                 (system-name)
                                 " "
                                 (propertize
                                   (if (string-prefix-p (getenv "HOME") (eshell/pwd))
                                     (concat "~" (substring (eshell/pwd) (length (getenv "HOME"))))
                                     (eshell/pwd))
                                   'face `(:foreground ,(doom-color 'green)))
                                 (if (= (user-uid) 0) "# " "> ")
                                 ))
)

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N)
  )

(defun my-rename-buffer-to-curdir (&optional _string)
    "Change Shell buffer's name to current directory."
    (rename-buffer (concat "*eshell:" default-directory "*")))

(add-hook 'eshell-mode-hook 'my-rename-buffer-to-curdir)
(add-hook 'eshell-directory-change-hook 'my-rename-buffer-to-curdir)

(add-hook 'eshell-mode-hook (lambda ()
    (eshell/alias "ledger" "ledger --no-pager $*")))

(map! :after eshell
      :map eshell-mode-map
      :i "C-p" 'eshell-previous-matching-input-from-input
      :i "C-n" 'eshell-next-matching-input-from-input
      :ni "M-n" nil
      :ni "M-p" nil
      :niv "M-m" 'eshell-bol)

(map! :after eshell
      :map eshell-hist-mode-map
      "M-n" #'+workspace/switch-right
      "M-p" #'+workspace/switch-left)
