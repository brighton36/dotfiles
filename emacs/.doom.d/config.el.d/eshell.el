;-*- mode: elisp -*-
(setq eshell-save-history-on-exit nil
      eshell-scroll-to-bottom-on-input 'all
      eshell-prefer-lisp-functions nil
      eshell-plain-grep-behavior t
      eshell-hist-ignoredups t
      eshell-history-append t
      eshell-banner-message ""
      eshell-highlight-prompt nil
      eshell-prompt-function (lambda nil
                               (concat
                                 (propertize (user-login-name) 'face `(:foreground ,(doom-color 'blue)))
                                 "@"
                                 (system-name)
                                 " "
                                 (propertize
                                   (if (string-prefix-p (getenv "HOME") (eshell/pwd))
                                     (concat "~" (substring (eshell/pwd) (length (getenv "HOME"))))
                                     (eshell/pwd))
                                   'face `(:foreground ,(doom-color 'blue)))
                                 (if (= (user-uid) 0) "# " "> ")
                                 ))
)

;(when (and (executable-find "fish")
;           (require 'fish-completion nil t))
;  (global-fish-completion-mode))


;; copied from https://stackoverflow.com/questions/13009908/eshell-search-history
(defun my-eshell-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
  (interactive "p")
  (if (not (memq last-command '(eshell-previous-matching-input-from-input
                eshell-next-matching-input-from-input)))
      ;; Starting a new search
      (setq eshell-matching-input-from-input-string
        (buffer-substring (save-excursion (eshell-bol) (point))
                  (point))
        eshell-history-index nil))
  (eshell-previous-matching-input
   (regexp-quote eshell-matching-input-from-input-string)
   arg))

;; override eshell-previous-matching-input-from-input, because it limits the search is from the beginning.
(advice-add 'eshell-previous-matching-input-from-input :override #'my-eshell-previous-matching-input-from-input)

; TODO This is what's causing our dashboard issues!
(setq doom-unreal-buffer-functions '(minibufferp))

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

; Note sure if this actually works...
; The add-hook does, but, the eshell-write-last-dir-ring is void, and I think the newest approach is better
;(add-hook 'eshell-exit-hook (lambda () (eshell-write-history eshell-write-last-dir-ring t)))
(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to `t'."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

(add-hook 'eshell-pre-command-hook #'eshell-append-history)

(add-hook 'eshell-mode-hook (lambda ()
    (eshell/alias "ledger" "ledger --no-pager $*")))

; This fixes evince not starting. Probably other programs too
(setenv "XDG_DATA_DIRS" "/usr/share")

(map! :after eshell
      :map eshell-mode-map
      :i "<tab>" 'company-complete-common-or-cycle
      :i "C-p" 'eshell-previous-matching-input-from-input
      :i "C-n" 'eshell-next-matching-input-from-input
      :ni "M-n" nil
      :ni "M-p" nil
      :niv "M-m" 'eshell-bol)

(map! :after eshell
      :map eshell-hist-mode-map
      "M-n" #'+workspace/switch-right
      "M-p" #'+workspace/switch-left)
