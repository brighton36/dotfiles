;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Chris DeRose"
      user-mail-address "cderose@derosetechnologies.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq doom-font (font-spec :family "Cousine NF" :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 14)
      doom-big-font (font-spec :family "Cousine NF" :size 14))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/org/")

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

;; Themes and Colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.doom.d/themes")
(setq doom-theme 'doom-solarized-light-cderose)

;; Background color of the line numbers column, and highlight current line color:
(custom-set-faces!
  `(line-number :background ,(doom-color 'bg-alt))
  `(line-number-current-line :background ,(doom-color 'grey)))

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

; 'Better' backspace deletes, that respect indentation. From:
; https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop
(defvar my-offset 4 "My indentation offset. ")
(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) my-offset))
          (p (point)))
      (when (= movement 0) (setq movement my-offset))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))


;; Preferences  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Enable Cmake Syntax highlighting
(require 'cmake-mode)

(setq shell-file-name "/bin/fish" vterm-max-scrollback 5000)

;; This should create more reasonable window close/resize behaviors
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

;; Edit Server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is used by firefox. For more on the edit server:
; https://www.emacswiki.org/emacs/Edit_with_Emacs

(when (and (require 'edit-server nil t) (daemonp))
      (edit-server-start))
; (setq edit-server-verbose t)

;; Dumb Jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(global-set-key
	(kbd "C-j")
	(defhydra dumb-jump-hydra (:color blue :columns 3)
		"Dumb Jump"
		("j" dumb-jump-go "Go")
		("o" dumb-jump-go-other-window "Other window")
		("e" dumb-jump-go-prefer-external "Go external")
		("x" dumb-jump-go-prefer-external-other-window "Go external other window")
		("i" dumb-jump-go-prompt "Prompt")
		("l" dumb-jump-quick-look "Quick look")
		("b" dumb-jump-back "Back"))
)

;; Custom keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Turn on the 80 character ruler on all files
(add-hook 'after-change-major-mode-hook 'fci-mode)

;; Alt-[ and Alt-] Window Cycle:
(global-set-key (kbd "M-]") '(lambda() (interactive) (other-window 1)))
(global-set-key (kbd "M-[") '(lambda() (interactive) (other-window -1)))

;; Alt-Shift-[ an Alt-Shift-] Move Window Cycle
(global-set-key (kbd "M-}") '(lambda() (interactive) (rotate-windows -1)))
(global-set-key (kbd "M-{") '(lambda() (interactive) (rotate-windows 1)))

;; Window Splits:
(define-key evil-normal-state-map (kbd "C--") '(lambda() (interactive) (split-window)(other-window 1)))
(global-set-key (kbd "C-\\") '(lambda() (interactive) (split-window-right)(other-window 1)))

;; Ubind C-= and C-+ in various places:
(global-unset-key (kbd "C-=")) ;; face-remap
(define-key evil-normal-state-map (kbd "C-=") nil) ;; doom-normal mode
(define-key evil-normal-state-map (kbd "C-+") nil) ;; doom-normal mode

;; Set Increase/Decrease Font size:
(global-set-key (kbd "C-+") 'text-scale-increase)
; This gets weird. I guess we could alternatively just disable undo-fu:
(after! undo-fu
  (map! :map undo-fu-mode-map "C-_" #'text-scale-decrease))

;; Close the window
(global-unset-key (kbd "C-<backspace>")) ;; simple.el backward-kill-word
(global-set-key (kbd "C-<backspace>") 'delete-window)

;; Alt-p and Alt-n for  Previous/Next Tab
(map! :nv "M-n" #'+workspace/switch-right :nv "M-p" #'+workspace/switch-left)

;; evil bindings:
;; https://github.com/noctuid/evil-guide#keybindings-and-states

;; In normal mode, ctrl-s will 'search' for an open buffer
(define-key evil-normal-state-map (kbd "C-s") #'+vertico/switch-workspace-buffer)

;; Window Resize Up/down:
(global-set-key (kbd "C-<") 'shrink-window)
(global-set-key (kbd "C->") 'enlarge-window)

;; Window Resize left/right. I guess this works..
(global-set-key (kbd "C-S-o") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-e") 'enlarge-window-horizontally)

;; Visual Mode's > and < shifts, don't really work the way you'd think. This adjusts
;; the shift, based on the language:
;; TODO: I'm not sure this is working... Answer: It does, but kills TRAMP
;; (add-to-list 'ignored-local-variable-values "tab-width")

;; Apps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Outline mode should use markdown style headers:
(setq outline-regexp "[#\f]+")

; mu4e Settings:
(set-email-account!
  "gmail"
  '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")
    (mu4e-trash-folder      . "/[Gmail]/Bin")
    (smtpmail-smtp-user     . "cderose@derosetechnologies.com"))
  t)

; TODO I think we'll want/need to rebind mule-headers-split-view-grow/shrink

; NOTE: The issue here, is that .emacs.d/modules/email/mu4e/config.el is loading after this file
;       so, we can just hook it here, like so:
(with-eval-after-load 'mu4e
  (setq 
    mu4e-get-mail-command "/usr/bin/mbsync -V cderose@derosetechnologies.com"
    ; NOTE: I tried setting up the sync in ~/.config/systemd/user/mbsync.service
    ; Per: https://wiki.archlinux.org/title/isync#With_a_timer
    ; But I think this works better:
    mu4e-update-interval 60
    mu4e-headers-auto-update t
    mu4e-compose-format-flowed t
    mu4e-index-cleanup nil
    mu4e-view-auto-mark-as-read nil
    mu4e-index-lazy-check t
    mu4e-view-show-images t
    mu4e-use-fancy-chars t
    mu4e-attachment-dir "~/Downloads"
    mu4e-headers-date-format "%y-%m-%d")
  (define-key mu4e-headers-mode-map (kbd "C-+") nil)
  ; Seems like these both need to be defined, in order to work:
  (define-key mu4e-view-mode-map (kbd "C-+") nil)
  (evil-define-key 'normal mu4e-view-mode-map (kbd "C-+") nil)
  )

;; TODO
;; (setq mu4e-compose-signature
;;    "Foo X. Bar\nhttp://www.example.com\n")

(setq message-kill-buffer-on-exit t)

; Telega settings
(setq telega-server-libs-prefix "/usr")
(setq telega-use-images 1)
(telega-notifications-mode 1)
(telega-appindicator-mode 1)
(define-key global-map (kbd "C-c t") telega-prefix-map)

;; Company (Code completion)

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

;; Projectile
; Mostly just ignores
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

;; ispell
(setq ispell-dictionary "en-custom")

;; Trying these settings out, from : https://tecosaur.github.io/emacs-config/config.html#fetching
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      )

(global-subword-mode 1)                           ; Iterate through CamelCase words

; New buffers default to org:
(setq-default major-mode 'org-mode)

; faster which-key menu:
(setq which-key-idle-delay 0.5) 

