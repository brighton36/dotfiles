;-*- mode: elisp -*-

(map! :after eshell
      :map eshell-mode-map
      :ni "C-p" 'eshell-previous-matching-input-from-input
      :ni "C-n" 'eshell-next-matching-input-from-input
      :niv "M-m" 'eshell-bol)

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))
