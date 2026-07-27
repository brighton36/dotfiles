;-*- mode: elisp -*-

(after! company
  (setq company-idle-delay 0.4
        company-minimum-prefix-length 0
        company-show-quick-access t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort) ;; make aborting less annoying.
)

(setq
  company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend company-echo-metadata-frontend)
  company-tooltip-flip-when-above t)

;(set-company-backend!
;  '(text-mode markdown-mode gfm-mode)
;  '(:seperate company-ispell company-files company-yasnippet))

; completion-preview-mode
;(add-hook 'prog-mode-hook #'completion-preview-mode)
;(add-hook 'text-mode-hook #'completion-preview-mode)
;; and in \\[shell] and friends
;(with-eval-after-load 'comint
;  (add-hook 'comint-mode-hook #'completion-preview-mode))

;(with-eval-after-load 'completion-preview
  ;; Show the preview already after two symbol characters
;  (setq completion-preview-minimum-symbol-length 2))

;  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
;  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
;  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

;(custom-set-faces!
;  `(completion-preview :foreground ,"#93a1a1", :background "#eee8d5")) ; Solarized base1, base2

