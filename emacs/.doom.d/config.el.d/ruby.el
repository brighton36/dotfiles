;;; ruby.el --- Description -*- lexical-binding: t; -*-

; ruby
(global-robe-mode)
(eval-after-load 'company '(push 'company-robe company-backends))

; Auto-start robe for rdoc and completion-at-point
(add-hook 'ruby-mode-hook #'robe-start)
