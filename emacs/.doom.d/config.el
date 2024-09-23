;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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

;; This is used in a few places
(setq auth-sources '((:source "~/.authinfo.gpg")))

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

;; Tells us to recompile the source files, when the cache is older..
(setq load-prefer-newer t)

;; Themes and Colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.doom.d/themes")
(setq doom-theme 'doom-solarized-light-cderose)

;; Background color of the line numbers column, and highlight current line color:
(custom-set-faces!
  `(line-number :background ,(doom-color 'bg-alt))
  `(line-number-current-line :background ,(doom-color 'grey)))

; Mostly this seems to affect org-mode. But, in general, I don't want links in bold
(custom-set-faces! `(link :weight normal))

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

'(auto-dim-other-buffers-face ((t (:background "#EEE8D5"))))

; (setq display-line-numbers-type t)

;; Enable Cmake Syntax highlighting
;;(require 'cmake-mode)

; In general, we seem to like these modes
(vertico-posframe-mode 1)

; TODO: be... more judicious here:
;(setq spacious-padding-widths
;      '( :internal-border-width 6
;         :header-line-width 2
;         :mode-line-width 3
;         :tab-width 2
;         :right-divider-width 15
;         :scroll-bar-width 4
;         :fringe-width 8))

; This will fix mu4e's weirdness with spacious, when/if the frame loses focus:
(with-eval-after-load 'mu4e
  (custom-set-faces `(mode-line ((t (:overline nil :box nil)))))
  (custom-set-faces `(mode-line-inactive ((t (:background nil :box nil)))))
  (custom-set-faces `(mode-line-active ((t (:box nil)))))
  )

(custom-set-faces '(auto-dim-other-buffers-face ((t (:background "#EEE8D5")))))
(custom-set-faces '(auto-dim-other-buffers-hide-face ((t (:extend t :background "#EEE8D5")))))

; TODO: Do we want this?
;(spacious-padding-mode 1)

(setq shell-file-name "/bin/fish" vterm-max-scrollback 10000)

;; This should create more reasonable window close/resize behaviors
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

;; Trying these settings out, from : https://tecosaur.github.io/emacs-config/config.html#fetching
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      )

(global-subword-mode 1)                           ; Iterate through CamelCase words

; New buffers default to org:
; I disabled this because of issues with emacs everywhere. Maybe I want this ultimately...
; (setq-default major-mode 'org-mode)

; faster which-key menu:
(setq which-key-idle-delay 0.5) 

;; Ruler mode  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 80-column indicator, and ruler:
; TODO : I think I decided against the ruler...
;(add-hook 'find-file-hook (lambda () (ruler-mode 1)))

; We don't want the fill column on org-mode, but on every other mode - enable it:
(add-hook 'find-file-hook (lambda () (if (not (eq 'org-mode (buffer-local-value 'major-mode (current-buffer)))) (display-fill-column-indicator-mode t))))
(setq comment-column -1) ; This seems to be the way to disable that '#' character in the ruler...

;; Visual Line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Just a prettier indication that we're wrapping:
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Edit Server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I disabled this when we switched to emacs everywhere...
; This is used by firefox. For more on the edit server:
; https://www.emacswiki.org/emacs/Edit_with_Emacs

; (when (and (require 'edit-server nil t) (daemonp))
;      (edit-server-start))
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

;; Alt-[ and Alt-] Window Cycle:
(global-set-key (kbd "M-]") #'(lambda() (interactive) (other-window 1)))
(global-set-key (kbd "M-[") #'(lambda() (interactive) (other-window -1)))

;; Alt-Shift-[ an Alt-Shift-] Move Window Cycle
(global-set-key (kbd "M-}") #'(lambda() (interactive) (rotate-windows -1)))
(global-set-key (kbd "M-{") #'(lambda() (interactive) (rotate-windows 1)))

;; Window Splits:
(define-key evil-normal-state-map (kbd "C--") #'(lambda() (interactive) (split-window)(other-window 1)))
(global-set-key (kbd "C-\\") #'(lambda() (interactive) (split-window-right)(other-window 1)))

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
(global-set-key (kbd "C-S-j") 'shrink-window)
(global-set-key (kbd "C-S-k") 'enlarge-window)

;; Window Resize left/right. I guess this works..
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)

;; Hebrew Mode
;; TODO: seems to work in insert mode , oddly. But, I think we may just want this for eshell...
;; (global-set-key (kbd "C-i") #'(lambda() (interactive) (buffer-face-mode-face 'default nil ':family "Linux Libertine") (buffer-face-mode)))

;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-agenda-files (list "inbox.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-long-done 'time)
(setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d!)")))
(setq org-refile-targets '(("archive.org" :maxlevel . 1)))

; TODO: this is crashing the lisp: ;(set-fill-column 0)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1) (display-line-numbers-mode 0)))

(defun dw/read-file-as-string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(after! org
  ; This was getting in the way of rotate:
  (define-key org-mode-map (kbd "M-{") nil)
  (define-key org-mode-map (kbd "M-}") nil)

  '(mapc
    (lambda (face)
      (set-face-attribute
       face nil
       :inherit
       (my-adjoin-to-list-or-symbol
        'fixed-pitch
        (face-attribute face :inherit))))
    (list 'org-code 'org-block 'org-table))

  (setq org-capture-templates
    `(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
      ("@" "Inbox [mu4e]" entry (file "inbox.org")
        ,(concat "* TODO Process \"%a\" %?\n"
                 "/Entered on/ %U"))
      ("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/personal.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("ts" "Clocked Entry Subtask" entry (clock)
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/org/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/org/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
      ))
  )

(let* ((variable-tuple
       (cond ((x-list-fonts "Ubuntu") '(:font "Ubuntu"))))
       (headline           `(:weight normal)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Ubuntu Light" :height 170))))
 '(fixed-pitch ((t ( :family "Cousine NF" :height 130)))))

(defun my-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

;; Save org buffers after re-filing:
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t ( :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-n") 'evil-avy-goto-char-timer))
(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-g") 'evil-avy-goto-line))

(setq avy-all-windows 'all-frames)

;; TODO: Let's maybe change the highlight color from grey to ... red?
`(avy-background-face ((t (:foreground ,"#ff0000" :background  ,"#ff00ff"))))
'(avy-background-face ((t (:background "#750000" :foreground "#BD9800"))))
'(avy-lead-face ((t (:background "#750000" :foreground "#BD9800" :weight bold))))

;; highlight-indent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq highlight-indent-guides-method 'bitmap)
(setq highlight-indent-guides-auto-character-face-perc 25)

;; epa ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'epa)
(epa-file-enable)

;; everywhere ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These frame params were all necessary to get the xmonad doCenterFloat to work:
(setq emacs-everywhere-frame-name-format "Emacs Everywhere")
(setq emacs-everywhere-frame-parameters 
  '((name . "Emacs Everywhere")
    (width . 120)
    (height . 40)
    (minibuffer . t)
    (menu-bar-lines . t)) )
(remove-hook 'emacs-everywhere-init-hooks 'emacs-everywhere-set-frame-position)

; There were a couple problems that were caused by xdotool not clearing modifiers, 
; after a paste. This method was copied out of the emacs-everywhere.el, and 
; slightly modified, to fix the stuck-key issue, that was happening after paste.
; Note that we nixed osx support in this implementation:
(eval-after-load "emacs-everywhere"
  (defcustom emacs-everywhere-paste-command
    (list "/home/cderose/.doom.d/bin/emacs-everywhere-paste.sh" "Shift+Insert")
    "Command to trigger a system paste from the clipboard.
    This is given as a list in the form (CMD ARGS...).

    To not run any command, set to nil."
      :type '(set (repeat string) (const nil))
      :group 'emacs-everywhere))

;; Outline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq outline-regexp "[#\f]+")

;; mu4e ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-email-account!
  "gmail"
  '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")
    (mu4e-trash-folder      . "/[Gmail]/Bin")
    (smtpmail-smtp-user     . "cderose@derosetechnologies.com"))
  t)

; SMTP Settings:
(setq message-send-mail-function 'smtpmail-send-it
  smtpmail-stream-type 'starttls
  smtpmail-default-smtp-server "smtp.gmail.com"
  smtpmail-smtp-server "smtp.gmail.com"
  smtpmail-smtp-service 587)

; Org-msg settings:
; NOTE: I don't think I'm actually using this at the moment. But, I'd like to...
(setq mail-user-agent 'gnus-user-agent)
(require 'org-msg)
(setq org-msg-startup "hidestars indent inlineimages"
  org-msg-greeting-fmt "\nHello %s,\n\n"
  org-msg-recipient-names '(("chris@chrisderose.com" . "Chris DeRose"))
  org-msg-greeting-name-limit 3
  org-msg-default-alternatives '((new   . (text html))
  (reply-to-html . (text html))
  (reply-to-text . (text)))
  org-msg-convert-citation t
  org-msg-signature "

  Regards,

  #+begin_signature
  --
  *Chris Derose
  /chris@chrisderose.com/
  #+end_signature")
 (org-msg-mode)

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
    fill-flowed-encode-column 80
    mu4e-index-cleanup nil
    mu4e-view-auto-mark-as-read nil
    mu4e-index-lazy-check t
    mu4e-view-show-images t
    mu4e-use-fancy-chars t
    mu4e-attachment-dir "~/Downloads"
    mu4e-compose-signature "Chris Derose\nchris@chrisderose.com\n"
    mu4e-headers-date-format "%y-%m-%d")
  ; Keybindings:
  ; NOTE: Implicit:
  ; Headers Mode:
  (define-key mu4e-headers-mode-map (kbd "C-+") nil)
  (evil-define-key 'normal mu4e-headers-mode-map "r" nil)
  (evil-define-key 'normal mu4e-headers-mode-map "?" nil)
  (evil-define-key 'normal mu4e-headers-mode-map "!" nil)
  (evil-define-key 'normal mu4e-headers-mode-map "+" nil)
  (evil-define-key 'normal mu4e-headers-mode-map "-" nil)
  (evil-define-key 'normal mu4e-headers-mode-map "=" nil)
  (evil-define-key 'normal mu4e-headers-mode-map "&" nil)
  (evil-define-key 'normal mu4e-headers-mode-map "*" nil)
  (evil-define-key 'normal mu4e-headers-mode-map "y" nil)
  (evil-define-key 'normal mu4e-headers-mode-map "m" 'mu4e-headers-mark-for-read)
  (evil-define-key 'normal mu4e-headers-mode-map "M" 'mu4e-headers-mark-for-unread)
  (evil-define-key 'normal mu4e-headers-mode-map "c" 'mu4e-org-store-and-capture)

  ; View mode:
  (define-key mu4e-view-mode-map (kbd "C-+") nil) ; For some reason this is necessary
  (evil-define-key 'normal mu4e-view-mode-map (kbd "C-+") nil)
  (evil-define-key 'normal mu4e-view-mode-map "r" nil)
  (evil-define-key 'normal mu4e-view-mode-map "?" nil)
  (evil-define-key 'normal mu4e-view-mode-map "+" nil)
  (evil-define-key 'normal mu4e-view-mode-map "-" nil)
  (evil-define-key 'normal mu4e-view-mode-map "=" nil)
  (evil-define-key 'normal mu4e-view-mode-map "&" nil)
  (evil-define-key 'normal mu4e-view-mode-map "*" nil)
  (evil-define-key 'normal mu4e-view-mode-map "y" nil)
  (evil-define-key 'normal mu4e-view-mode-map "m" #'mu4e-view-mark-for-read)
  (evil-define-key 'normal mu4e-view-mode-map "M" #'mu4e-view-mark-for-unread)
  (evil-define-key 'normal mu4e-view-mode-map "!" #'mu4e-view-raw-message)
  (evil-define-key 'normal mu4e-view-mode-map "c" 'mu4e-org-store-and-capture)
  )

(setq message-kill-buffer-on-exit t)

;; Telega ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq telega-server-libs-prefix "/usr")
(setq telega-use-images 1)
(setq telega-filter-button-width 10)

(telega-notifications-mode 1)
(telega-appindicator-mode 1)
(define-key global-map (kbd "C-c t") telega-prefix-map)

(with-eval-after-load 'telega
	(define-key telega-chat-mode-map (kbd "C-S-r") 'telega-msg-reply)
	(define-key telega-chat-mode-map (kbd "C-S-e") 'telega-msg-edit)
  (evil-define-key 'normal telega-chat-mode-map "q" 'telega) )

;; This sets our messages to wrap by default
(add-hook 'telega-chat-mode-hook 'visual-line-mode)

;; Company ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mostly just ignores
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

;; ispell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ispell-dictionary "en-custom")

;; vterm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "M-]") nil) 
  (define-key vterm-mode-map (kbd "C-<backspace>") nil) 
  )

;; eshell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'eshell-mode-hook
  (lambda ()
    (define-key evil-insert-state-local-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
    (define-key evil-insert-state-local-map (kbd "C-n") 'eshell-next-matching-input-from-input)
    ))

(define-key evil-insert-state-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
(define-key evil-insert-state-map (kbd "C-n") 'eshell-next-matching-input-from-input)

;; explain-pause-top ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/get-frame-by-name (fname)
  "If there is a frame named FNAME, return it, else nil."
  (require 'dash)                       ; For `-some'
  (-some (lambda (frame)
           (when (equal fname (frame-parameter frame 'name))
             frame))
         (frame-list)))

(setq explain-pause-top-auto-refresh-interval 1)

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

(evil-define-key 'normal 'global (kbd "C-t") 'open-explain-pause-top-in-new-frame)

;; emojify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq emojify-display-style 'unicode)
(setq emojify-emoji-styles '(unicode))

;; web-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
