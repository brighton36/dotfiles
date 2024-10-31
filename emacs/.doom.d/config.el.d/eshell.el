;-*- mode: elisp -*-

(map! :after eshell
      :map evil-insert-state-local-map
      "C-p" 'eshell-previous-matching-input-from-input
      "C-n" 'eshell-next-matching-input-from-input

      :map evil-insert-state-map
      "C-p" 'eshell-previous-matching-input-from-input
      "C-n" 'eshell-next-matching-input-from-input

      :map eshell-mode-map
      :niv "M-m" 'eshell-bol)

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))
