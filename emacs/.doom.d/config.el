;;; config.el --- Description -*- lexical-binding: t; -*-

; This seems to be needed, since switching to guix:
(add-to-list 'custom-theme-load-path "~/.doom.d/themes")

(setq user-full-name (get-secret 'mu4e-from-fullname)
      user-mail-address (get-secret 'mu4e-from-address)

      load-prefer-newer t         ; Tells us to recompile the source files, when the cache is older..
      delete-by-moving-to-trash t ; Delete files to trash
      x-stretch-cursor t          ; Stretch cursor to the glyph width
      undo-limit 80000000         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t       ; By default while in insert all changes are one big blob. Be more granular
      which-key-idle-delay 0.5    ; faster which-key menu

      auth-sources '((:source "~/.authinfo.gpg")) ; This is used in a few places...
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow) ; Just a prettier indication that we're wrapping:
      text-scale-mode-step 1.05
      shell-file-name (executable-find "bash")

      ; doom

      ; This is what was causing the doom to randomly change windows to the dashboard
      doom-unreal-buffer-functions '(minibufferp)

      doom-theme 'doom-nord-light
      ; Seems like emacs only picks up fonts in ~/.local/share/fonts/
      doom-font (font-spec :family "UbuntuMono Nerd Font" :size 16)            ; the primary font to use
      doom-variable-pitch-font (font-spec :family "Ubuntu Nerd Font" :size 14) ; a non-monospace font (where applicable)
      doom-big-font (font-spec :family "UbuntuMono Nerd Font" :size 18)        ; use this for presentations or streaming

      vertico-posframe-font "Ubuntu Nerd Font 14"
      ; NOTE: These fonts are also available:
      ;doom-symbol-font (font-spec :family "Noto Color Emoji" :size 14)        ; for unicode glyphs
      ; - `doom-serif-font' -- for the `fixed-pitch-serif' face
      fancy-splash-image (concat doom-user-dir "dashboard-cat.png")
      nerd-icons-font-family "Symbols Nerd Fonts Mono"

      ; avy
      avy-all-windows 'all-frames

      ; vterm
      shell-file-name "/bin/bash" 
      vterm-max-scrollback 10000
      vterm-shell (executable-find "fish")

      ; emojify
      emojify-display-style 'unicode
      emojify-emoji-styles '(unicode)

      ; Outline
      outline-regexp "[#\f]+"

      ; minibuffer
      resize-mini-windows nil ; TODO I *think* this fixes the issue where the minibuffer doesn't
                              ; paint on initial draw, sometimes...

      ; highlight-indent
      highlight-indent-guides-method 'bitmap
      highlight-indent-guides-auto-character-face-perc 25

      ; ace-window
      aw-dispatch-when-more-than 1

      ; lisp
      lisp-indent-offset 2)

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
(after! auto-dim-other-buffers
  (custom-set-faces
    '(auto-dim-other-buffers-face ((t (:background "#d8dee9")))))
  (custom-set-faces
    '(auto-dim-other-buffers-hide ((t (:background "d8dee9")))))
  )

; avy
; TODO: Let's maybe change the highlight color from grey to ... red?
`(avy-background-face ((t (:foreground ,"#ff0000" :background  ,"#ff00ff"))))
'(avy-background-face ((t (:background "#750000" :foreground "#BD9800"))))
'(avy-lead-face ((t (:background "#750000" :foreground "#BD9800" :weight bold))))

; ledger-mode
(custom-set-faces! `(ledger-font-payee-uncleared-face :foreground ,(doom-color 'green)))
(custom-set-faces! `(ledger-font-comment-face :foreground ,(doom-color 'cyan)))

; auto-fill comments on all modes
(setq-default auto-fill-function 'do-auto-fill)
(setq comment-auto-fill-only-comments t)
(setq-default fill-column 100)

(add-hook 'ruby-mode-hook
  (function (lambda ()
          (setq evil-shift-width ruby-indent-level))))

;; Random mode Preferences  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(vertico-posframe-mode 1) ; In general, we seem to like these modes
(global-subword-mode 1)   ; Iterate through CamelCase words

; Winner mode, which we really only use for winner-undo (C-w C-u)
(when (fboundp 'winner-mode) (winner-mode t))

; Turns on urls in code comments
(global-goto-address-mode)

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

; This fixes a problem in the emacs --daemon mode, where evil-collection-debug crashes the init, due
; to this function not being defined. At some point we'll upgrade to an emacs where this is
; compiled, and this hack should be removed
(defun treesit-available-p () nil)

; I'm trying this out...
(savehist-mode 1)

; Aider
; Alternatively: "--model" "deepseek" "--api-key" (concat "deepseek=" (get-secret 'deepseek-api-key))
(use-package aider
  :config
  (setq aider-args (list "--no-auto-commits" "--model" "claude-3-5-sonnet-latest" "--api-key"
                     (concat "anthropic=" (get-secret 'anthropic-api-key))))
 (require 'aider-doom))

; Not sure I want this yet...
(global-set-key (kbd "C-c a") 'aider-transient-menu)
(global-auto-revert-mode 1) ; this reloads the aider changes. Possibly shouldn't be in this section...

; completion-preview-mode
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)
;; and in \\[shell] and friends
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))

(with-eval-after-load 'completion-preview
  ;; Show the preview already after two symbol characters
  (setq completion-preview-minimum-symbol-length 2))

; TODO
;  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
;  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
;  ;; Convenient alternative to C-i after typing one of the above
;  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

(custom-set-faces!
  `(completion-preview :foreground ,"#93a1a1", :background "#eee8d5")) ; Solarized base1, base2

; flyspell:
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

; epa
(require 'epa)
(epa-file-enable)

; Doom Dashboard:
(assoc-delete-all "Reload last session" +doom-dashboard-menu-sections) ;TODO is this working...
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
             '("Start LLM chat"
               :icon (nerd-icons-faicon "nf-fa-rocketchat" :face 'doom-dashboard-menu-title)
               ; TODO
               ;:when (modulep! mu4e)
               :action gptel))
(add-to-list '+doom-dashboard-menu-sections
             '("New Blank Buffer"
               :icon (nerd-icons-faicon "nf-fa-file" :face 'doom-dashboard-menu-title)
               :action +default/new-buffer))

; Disable the auto-pairing of parethesis and quotes and such...
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

; Solaire screws with auto-dim, and just kinda sucks for me. Disable it
(after! solaire-mode (solaire-global-mode -1))

;; Key Bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! 
  :n "C-+" #'text-scale-increase
  :n "C-=" nil

  ;; Next/Last Error
  :ni "C->" 'next-error
  :ni "C-<" 'previous-error

  ; Disable better-jumper for tab on insert
  :i [tab] #'indent-for-tab-command

  ;; Disable new tab shortcut:
  :n "C-t" nil

  ;; Comment
  :n "C-;" 'comment-region

  ;; Window Splits:
  :n "C-\\" #'evil-window-vsplit
  :n "C--" #'evil-window-split

  ; ace-window
  :n "C-f" #'ace-window

  ; Cursor Jumps
  :ni "C-o" #'better-jumper-jump-backward
  :ni "C-i" #'better-jumper-jump-forward

  ;; Otherwise, In normal mode, ctrl-s will 'search' for an open buffer
  :n "C-s" #'+vertico/switch-workspace-buffer
  :n "C-S-s" #'consult-buffer

  :nv "M-n" #'+workspace/switch-right
  :nv "M-p" #'+workspace/switch-left

  ;; Avy
  :nv "C-." #'evil-avy-goto-char-timer
  :nv "C-l" #'evil-avy-goto-line

  ;; Emoji's
  :i "C-z" #'emoji-insert

  ;; popup
  :n "C-e" #'+popup/toggle
  :n "C-`" nil

  ;; Globals:
  :g "C-S-c" #'org-capture

  ;; Winner:
  :nv "C-n" #'winner-redo ; For whatever reason, C-p cannot be declared this way

  ; Seemingly, :g doesn't adjust this map?
  :map global-map
    ; Window management
    "M-]" #'evil-window-next
    "M-[" #'evil-window-prev
    "M-}" #'evil-window-exchange
    "M-{" #'(lambda() (interactive) (evil-window-exchange -1))
    "C-<backspace>" #'evil-window-delete

    ;; Window Resize Up/down:
    "C-S-j" #'shrink-window
    "C-S-k" #'enlarge-window

    ;; Window Resize left/right. I guess this works..
    "C-S-h" #'shrink-window-horizontally
    "C-S-l" #'enlarge-window-horizontally
  )

(map! :map global-map "C-`" nil) ; We moved this to C-e

;; See note above for C-n
(evil-define-key 'normal 'global (kbd "C-p") 'winner-undo)

;; The SPC o ... customizations. Which, is kinda like my 'start' menu
(map! :leader 
      :n "o b" nil
      :desc "Aider" :n "o a" #'aider-transient-menu
      :desc "Org Capture" :n "o c" #'org-capture
      :desc "Toggle eshell popup" :n "o e" #'eshell-toggle
      :desc "Open eshell here" :n "o E" #'eshell-new
      :desc "Start an llm session" :n "o l" #'gptel
      :desc "haskell (ghci)" :n "o H" #'run-haskell
      :desc "javascript (node)" :n "o J" #'nodejs-repl
      :desc "Mu4e" :n "o m" #'mu4e
      :desc "New blank buffer" :n "o n" #'+default/new-buffer
      :n "o r" nil
      :desc "irb" :n "o R" #'inf-ruby
      :desc "Start Telegram Client" :n "o t" #'telega
      :n "o T" nil
      :desc "Web browser popup" :n "o w" #'eww
      :desc "Toggle vterm popup" :n "o v" #'vterm
      :desc "Open vterm here" :n "o V" #'vterm-new
      :desc "Google Translate this Buffer" :n "o x" #'google-translate-buffer
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

; NOTE: I think we want this, to allow our winner-mode to take over... I think
(map! :map evil-state-map
      :i "C-p" nil
      :i "C-n" nil
  )

; This affects ruby-inf, run-python, node-js repl
(map! :after comint
      :map comint-mode-map
      :i "C-p" #'comint-previous-input
      :i "C-n" #'comint-next-input
      :n "C-p" nil
      :n "C-n" nil
  )
;; load config.el.d ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq configfiles (file-expand-wildcards "~/.doom.d/config.el.d/*.el"))

; Stuff that we don't use on osx... 
(cond ((string-equal system-type "darwin")
       (setq configfiles (cl-delete-if (lambda (k) (string-match-p "\\(emacs-everywhere\\|telega\\)\.el$" k))
                         configfiles))
      )) 
(cond ((string-equal system-type "gnu/linux")
       (setq configfiles (cl-delete-if (lambda (k) (string-match-p "\\(slack\\|osx\\)\.el$" k))
                         configfiles))
      )) 

(mapc 'load configfiles)
