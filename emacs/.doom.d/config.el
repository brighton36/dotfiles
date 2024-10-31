;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

; This seems to be needed, since switching to guix:
(add-to-list 'custom-theme-load-path "~/.doom.d/themes")

(setq user-full-name "Chris DeRose"
      user-mail-address "cderose@derosetechnologies.com"

      load-prefer-newer t         ; Tells us to recompile the source files, when the cache is older..
      delete-by-moving-to-trash t ; Delete files to trash
      x-stretch-cursor t          ; Stretch cursor to the glyph width
      undo-limit 80000000         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t       ; By default while in insert all changes are one big blob. Be more granular
      which-key-idle-delay 0.5    ; faster which-key menu

      auth-sources '((:source "~/.authinfo.gpg")) ; This is used in a few places...
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow) ; Just a prettier indication that we're wrapping:

      ; doom
      doom-theme 'doom-solarized-light-cderose
      doom-font (font-spec :family "Cousine NF" :size 14)            ; the primary font to use
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 14) ; a non-monospace font (where applicable)
      doom-big-font (font-spec :family "Cousine NF" :size 14)        ; use this for presentations or streaming
      ; NOTE: These fonts are also available:
      ; - `doom-unicode-font' -- for unicode glyphs
      ; - `doom-serif-font' -- for the `fixed-pitch-serif' face
      
      ; vterm
      shell-file-name "/bin/fish" 
      vterm-max-scrollback 10000

      ; emojify
      emojify-display-style 'unicode
      emojify-emoji-styles '(unicode)

      ; ispell
      ispell-dictionary "en-custom"

      ; Outline 
      outline-regexp "[#\f]+"

      ; highlight-indent
      highlight-indent-guides-method 'bitmap
      highlight-indent-guides-auto-character-face-perc 25
)

;; Themes and Colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces!
  `(line-number :background ,(doom-color 'bg-alt)) ; Background color of the line numbers column, and highlight current line color:
  `(line-number-current-line :background ,(doom-color 'grey))

  `(link :weight normal) ; Mostly this seems to affect org-mode. But, in general, I don't want links in bold
)

; auto-dim
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
'(auto-dim-other-buffers-face ((t (:background "#EEE8D5"))))
(custom-set-faces '(auto-dim-other-buffers-face ((t (:background "#EEE8D5")))))
(custom-set-faces '(auto-dim-other-buffers-hide-face ((t (:extend t :background "#EEE8D5")))))

; avy
; TODO: Let's maybe change the highlight color from grey to ... red?
`(avy-background-face ((t (:foreground ,"#ff0000" :background  ,"#ff00ff"))))
'(avy-background-face ((t (:background "#750000" :foreground "#BD9800"))))
'(avy-lead-face ((t (:background "#750000" :foreground "#BD9800" :weight bold))))

;; Random mode Preferences  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vertico-posframe-mode 1) ; In general, we seem to like these modes
(global-subword-mode 1)   ; Iterate through CamelCase words

; more reasonable window close/resize behaviors
(customize-set-variable 'display-buffer-base-action '(
    (display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)
  )
)

(customize-set-variable 'even-window-sizes nil) ; avoid resizing

; TODO: This seems... not so efficient...
(defun my-web-mode-hook () "Hooks for Web mode." (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(require 'epa)
(epa-file-enable)

(setq avy-all-windows 'all-frames)

;; Key Bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! 
  ;; Evil bindings for all modes
  :n "C-+" #'text-scale-increase
  :n "C-=" nil

  ;; Window Splits:
  :n "C-\\" #'(lambda() (interactive) (split-window-right)(other-window 1))
  :n "C--" #'(lambda() (interactive) (split-window)(other-window 1))

  ;; Otherwise, In normal mode, ctrl-s will 'search' for an open buffer
  :n "C-s" #'+vertico/switch-workspace-buffer

  :nv "M-n" #'+workspace/switch-right
  :nv "M-p" #'+workspace/switch-left

  ;; Avy
  :nv "C-n" #'evil-avy-goto-char-timer
  :nv "C-g" #'evil-avy-goto-line

  ;; Emoji's
  :i "C-z" #'emoji-insert

  ;; Explain:
  :n "C-t" #'open-explain-pause-top-in-new-frame

  ;; Globals:
  :g "C-c c" #'org-capture

  :map global-map 
    ;; Alt-[ and Alt-] Window Cycle:
    "M-]" #'(lambda() (interactive) (other-window 1))
    "M-[" #'(lambda() (interactive) (other-window -1))

    ;; Alt-Shift-[ an Alt-Shift-] Move Window Cycle
    "M-}" #'(lambda() (interactive) (rotate-windows -1))
    "M-{" #'(lambda() (interactive) (rotate-windows 1))

    ;; Window Resize Up/down:
    "C-S-j" #'shrink-window
    "C-S-k" #'enlarge-window

    ;; Window Resize left/right. I guess this works..
    "C-S-h" #'shrink-window-horizontally
    "C-S-l" #'enlarge-window-horizontally

    ; Window Close:
    "C-<backspace>" #'delete-window
  )

;; Mode specific mappings. These each seem to need separate map! calls. I think
;; the :after parameter doesn't overwrie the prior after, and instead chains.
(map! :after undo-fu 
      :map undo-fu-mode-map 
      "C-_" #'text-scale-decrease)

(map! :after vterm 
      :map vterm-mode-map
      "M-]" nil
      "C-<backspace>" nil)

(map! :after eshell 
      :map evil-insert-state-local-map
      "C-p" 'eshell-previous-matching-input-from-input
      "C-n" 'eshell-next-matching-input-from-input

      :map evil-insert-state-map
      "C-p" 'eshell-previous-matching-input-from-input
      "C-n" 'eshell-next-matching-input-from-input

      :map eshell-mode-map
      :niv "M-m" 'eshell-bol)

;; config.el.d ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load! "config.el.d/my-functions.el")
(load! "config.el.d/org.el")
(load! "config.el.d/mu4e.el")
(load! "config.el.d/company.el")
(load! "config.el.d/telega.el")
(load! "config.el.d/explain-pause-top.el")
(load! "config.el.d/projectile.el")
(load! "config.el.d/emacs-everywhere.el")
(load! "config.el.d/dumb-jump.el")
