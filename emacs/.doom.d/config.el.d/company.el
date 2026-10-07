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

; Not sure about some of this...
;(set-company-backend!
;  '(text-mode markdown-mode gfm-mode)
;  '(:seperate company-ispell company-files company-yasnippet))

(company-quickhelp-mode)

; Seems like we arrive here before the themes load...
(add-hook 'doom-load-theme-hook
  (lambda () (setq company-quickhelp-color-background (doom-color 'base2))))


; eshell history:
(defun company-eshell-history (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-history))
    (prefix (and (eq major-mode 'eshell-mode)
              (let ((line (buffer-substring-no-properties
                            (save-excursion (eshell-bol) (point))
                            (point))))
                (and (not (string-empty-p line)) line))))
    (candidates (cl-remove-duplicates
                  (->> (ring-elements eshell-history-ring)
                    (cl-remove-if-not (lambda (item) (s-prefix-p arg item)))
                    (mapcar 's-trim))
                  :test 'string=))
    (sorted t)))

(eval-after-load 'company '(push '(company-eshell-history :with company-capf) company-backends))
