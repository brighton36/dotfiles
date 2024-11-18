;-*- mode: elisp -*-

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
      doom-variable-pitch-font (font-spec :family "Ubuntu Nerd Font" :size 14) ; a non-monospace font (where applicable)
      doom-big-font (font-spec :family "Cousine Nerd Font" :size 14)        ; use this for presentations or streaming
      ; NOTE: These fonts are also available:
      doom-symbol-font (font-spec :family "Noto Color Emoji" :size 14)        ; for unicode glyphs
      ; - `doom-serif-font' -- for the `fixed-pitch-serif' face
      fancy-splash-image (concat doom-private-dir "dashboard-cat.png")
      
      ; avy
      avy-all-windows 'all-frames

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

; dired - Seems like we need to set these after the mode loads
(after! dired (setq dired-listing-switches "-lt"))

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

; This fixes a problem in the emacs --daemon mode, where evil-collection-debug 
; crashes the init, due to this function not being defined. At some point
; we'll upgrade to an emacs where this is compiled, and this hack should be 
; removed
(defun treesit-available-p () nil)

(require 'epa)
(epa-file-enable)

; ellama
(require 'ellama)
(setopt ellama-provider (make-llm-ollama :chat-model "mistral"))

; Doom Dashboard:
;(assoc-delete-all "Open project" +doom-dashboard-menu-sections) ; Just for reference, if we want to delete something
(add-to-list '+doom-dashboard-menu-sections
             '("Start Telega"
               :icon (nerd-icons-faicon "nf-fae-telegram" :face 'doom-dashboard-menu-title)
               ; TODO
               ;:when (modulep! telega)
               :action telega))
(add-to-list '+doom-dashboard-menu-sections
             '("Start mu4e"
               :icon (nerd-icons-codicon "nf-cod-mail" :face 'doom-dashboard-menu-title)
               ; TODO
               ;:when (modulep! mu4e)
               :action mu4e))
(add-to-list '+doom-dashboard-menu-sections
             '("Start ellama chat"
               :icon (nerd-icons-faicon "nf-fa-rocketchat" :face 'doom-dashboard-menu-title)
               ; TODO
               ;:when (modulep! mu4e)
               :action ellama-chat))
(add-to-list '+doom-dashboard-menu-sections
             '("New Blank Buffer"
               :icon (nerd-icons-faicon "nf-fa-file" :face 'doom-dashboard-menu-title)
               :action +default/new-buffer))

      :desc "New blank buffer" :n "o n" #'+default/new-buffer
;; Key Bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! 
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

  ;; popup
  :n "C-e" #'+popup/toggle
  :n "C-`" nil

  ;; Globals:
  :g "C-c c" #'org-capture
  :g "C-c e" #'ellama-transient-main-menu ; See: https://willschenk.com/labnotes/2024/ai_in_emacs/

  ; Seemingly, :g doesn't adjust this map?
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

(map! :map global-map "C-`" nil) ; We moved this to C-e

;; The SPC o ... customizations. Which, is kinda like my 'start' menu
(map! :leader 
      :n "o b" nil
      :desc "Org Capture" :n "o c" #'org-capture
      :desc "Toggle eshell popup" :n "o e" #'eshell-toggle
      :desc "Open eshell here" :n "o E" #'eshell
      :desc "Start an ellama chat" :n "o l" #'ellama-chat
      :desc "magit" :n "o g" #'magit
      :desc "haskell (ghci)" :n "o H" #'run-haskell
      :desc "javascript (node)" :n "o J" #'nodejs-repl
      :desc "Mu4e" :n "o m" #'mu4e
      :desc "New blank buffer" :n "o n" #'+default/new-buffer
      :n "o r" nil
      :desc "irb" :n "o R" #'inf-ruby
      :desc "Start Telegram Client" :n "o t" #'telega
      :n "o T" nil
      :desc "Web browser popup" :n "o w" #'eww
      :desc "Toggle vterm popup" :n "o V" #'vterm
      :desc "Open vterm here" :n "o v" #'vterm-mode
      :desc "ipython " :n "o Y" #'run-python
      )

;; Mode specific mappings. These each seem to need separate map! calls. I think
;; the :after parameter doesn't overwrie the prior after, and instead chains...
(map! :after undo-fu 
      :map undo-fu-mode-map 
      "C-_" #'text-scale-decrease)

(map! :after vterm 
      :map vterm-mode-map
      "M-]" nil
      "C-<backspace>" nil)

;; load config.el.d ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc 'load (file-expand-wildcards "~/.doom.d/config.el.d/*.el"))
