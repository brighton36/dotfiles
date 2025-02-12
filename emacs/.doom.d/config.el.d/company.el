;-*- mode: elisp -*-

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 0
        company-show-quick-access t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort) ;; make aborting less annoying.
)

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(set-company-backend!
  '(text-mode markdown-mode gfm-mode)
  '(:seperate company-ispell company-files company-yasnippet))

