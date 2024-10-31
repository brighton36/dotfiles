;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; This is used in a few places

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; This seems to be needed, since switching to guix
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e") 
(add-to-list 'custom-theme-load-path "~/.doom.d/themes")

(setq user-full-name "Chris DeRose"
      user-mail-address "cderose@derosetechnologies.com"

      doom-font (font-spec :family "Cousine NF" :size 14)            ; the primary font to use
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 14) ; a non-monospace font (where applicable)
      doom-big-font (font-spec :family "Cousine NF" :size 14)        ; use this for presentations or streaming
      ;; NOTE: These fonts are also available:
      ;; - `doom-unicode-font' -- for unicode glyphs
      ;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
      doom-theme 'doom-solarized-light-cderose
      
      auth-sources '((:source "~/.authinfo.gpg")) ; This is used in a few places...

      load-prefer-newer t         ; Tells us to recompile the source files, when the cache is older..
      delete-by-moving-to-trash t ; Delete files to trash
      x-stretch-cursor t          ; Stretch cursor to the glyph width

      undo-limit 80000000         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t       ; By default while in insert all changes are one big blob. Be more granular

      which-key-idle-delay 0.5    ; faster which-key menu

      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow) ; Just a prettier indication that we're wrapping:

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

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
'(auto-dim-other-buffers-face ((t (:background "#EEE8D5"))))
(custom-set-faces '(auto-dim-other-buffers-face ((t (:background "#EEE8D5")))))
(custom-set-faces '(auto-dim-other-buffers-hide-face ((t (:extend t :background "#EEE8D5")))))

;; Random Preferences  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(require 'epa)
(epa-file-enable)

;; Custom Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Window Rotation:
(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

;; Custom keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :g "C-c t" telega-prefix-map
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

;; NOTE: For whatever reason, when we tried to incorporate these map!'s into the
;; above map, telega got unstable, and some of the mappings just kinda stopped 
;; working. Breaking these into multiple map! calls fixed all that.

;; Mode specific mappings
(map! :after undo-fu 
      :map undo-fu-mode-map 
      "C-_" #'text-scale-decrease)

(map! :after org 
      :map org-mode-map 
      "M-{" nil
      "M-}" nil)

(map! :after evil-org
      :map evil-org-mode-map 
      :ni "C-S-j" nil
      :ni "C-S-k" nil
      :ni "C-S-h" nil
      :ni "C-S-l" nil
      )

(map! :after mu4e 
      :map mu4e-headers-mode-map 
      "C-+" nil
      :n "r" nil
      :n "?" nil
      :n "!" nil
      :n "+" nil
      :n "-" nil
      :n "=" nil
      :n "&" nil
      :n "*" nil
      :n "y" nil
      :n "m" 'mu4e-headers-mark-for-read
      :n "M" 'mu4e-headers-mark-for-unread
      :n "c" 'mu4e-org-store-and-capture

      :map mu4e-view-mode-map
      "C-+" nil
      :n "r" nil
      :n "?" nil
      :n "+" nil
      :n "-" nil
      :n "=" nil
      :n "&" nil
      :n "*" nil
      :n "y" nil
      :n "m" 'mu4e-headers-mark-for-read
      :n "M" 'mu4e-headers-mark-for-unread
      :n "!" 'mu4e-view-raw-message
      :n "c" 'mu4e-org-store-and-capture)

(map! :after telega 
      :map telega-chat-mode-map
      "C-S-r" 'telega-msg-reply
      "C-S-e" 'telega-msg-edit
      :n "C-g" 'telega-chatbuf-cancel-aux
      :n "q" 'telega

      :map telega-msg-button-map 
      "e" 'telega-msg-edit)

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
(load! "config.el.d/org.el")
(load! "config.el.d/mu4e.el")

;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq avy-all-windows 'all-frames)

;; TODO: Let's maybe change the highlight color from grey to ... red?
`(avy-background-face ((t (:foreground ,"#ff0000" :background  ,"#ff00ff"))))
'(avy-background-face ((t (:background "#750000" :foreground "#BD9800"))))
'(avy-lead-face ((t (:background "#750000" :foreground "#BD9800" :weight bold))))

;; everywhere ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: Actually, these frame params kinda borked...
; These frame params were all necessary to get the xmonad doCenterFloat to work:
(setq emacs-everywhere-frame-name-format "Emacs Everywhere"
      emacs-everywhere-frame-parameters '(
                                          (name . "Emacs Everywhere")
                                          (width . 120)
                                          (height . 40)
                                          (minibuffer . t)
                                          (menu-bar-lines . t))
      emacs-everywhere-paste-command (list "/usr/bin/xdotool" 
                                           "key" "--clearmodifiers" "Shift+Insert"
                                           "sleep" "0.25" 
                                           "keyup" "Meta_L" "Meta_R" "Alt_L" "Alt_R" "Super_L" "Super_R")
)

(remove-hook 'emacs-everywhere-init-hooks 'emacs-everywhere-set-frame-position)


;; Telega ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq telega-server-libs-prefix "/usr"
      telega-use-images 1
      telega-filter-button-width 10)

(telega-notifications-mode 1)
(telega-appindicator-mode 1)

;; This sets our messages to wrap by default
(add-hook 'telega-chat-mode-hook 'visual-line-mode)

(set-face-attribute 'telega-msg-inline-reply nil :foreground "#556b72")

;; Company ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort) ;; make aborting less annoying.
)

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(set-company-backend!
  '(text-mode markdown-mode gfm-mode)
  '(:seperate company-ispell company-files company-yasnippet))

;; Projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mostly just ignores
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

;; explain-pause-top ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq explain-pause-top-auto-refresh-interval 1)

(defun my/get-frame-by-name (fname)
  "If there is a frame named FNAME, return it, else nil."
  (require 'dash)                       ; For `-some'
  (-some (lambda (frame)
           (when (equal fname (frame-parameter frame 'name))
             frame))
         (frame-list)))

(defun open-explain-pause-top-in-new-frame ()
  "Open `explain-pause-top` in a new frame."
  (interactive)
  (setq topframe (my/get-frame-by-name "Emacs top"))
  (if topframe
    (select-frame-set-input-focus topframe)
    (let ((new-frame (make-frame '((name . "Emacs top")) )))
      (select-frame new-frame)
      (call-interactively #'explain-pause-top))
    )
  )

;; Dumb Jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

; TODO: This ... stopped working when we upgraded to emacs 29. And, I'm not sure if we still want it
;(global-set-key
;	(kbd "C-j")
;	(defhydra dumb-jump-hydra (:color blue :columns 3)
;		"Dumb Jump"
;		("j" dumb-jump-go "Go")
;		("o" dumb-jump-go-other-window "Other window")
;		("e" dumb-jump-go-prefer-external "Go external")
;		("x" dumb-jump-go-prefer-external-other-window "Go external other window")
;		("i" dumb-jump-go-prompt "Prompt")
;		("l" dumb-jump-quick-look "Quick look")
;		("b" dumb-jump-back "Back"))
;)

