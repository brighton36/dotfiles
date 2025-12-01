; javascript.el --- Description -*- lexical-binding: t; -*-
;

;(defvaralias 'js-indent-level 'tab-width)
;(defvaralias 'indent-tabs-mode nil)
(add-to-list 'doom-detect-indentation-excluded-modes 'web-mode)
(add-to-list 'doom-detect-indentation-excluded-modes 'typescript-tsx-mode)

(setq web-mode-code-indent-offset 2)

(after! typescript-mode
  (setq typescript-indent-level 2))

(add-hook 'json-mode-hook
  (lambda ()
    (make-local-variable 'js-indent-level)
    (setq tab-width 2)
    (setq js-indent-level 2)))
