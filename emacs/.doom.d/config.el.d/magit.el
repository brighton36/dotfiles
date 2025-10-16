; magit.el --- Description -*- lexical-binding: t; -*-

(require 'forge-commands)

(setq git-commit-summary-max-length 512)

; TODO: See if we need an with-eval-after-load on these... We probably do...
(define-key magit-mode-map (kbd "M-n") nil)
(define-key magit-mode-map (kbd "M-p") nil)
;(map!
;  :map magit-mode-map
;    :nvi "M-n" nil
;    :nvi "M-p" nil
;)
