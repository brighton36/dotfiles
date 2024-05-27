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

; 80-column indicator, and ruler:
(add-hook 'find-file-hook (lambda () (ruler-mode 1)))
(add-hook 'find-file-hook (lambda () (display-fill-column-indicator-mode t)))

; TODO: Can we nix this, now that we have eaf?
; The 'open in browser' should always load in the default profile:
;(setq browse-url-generic-program
;	(shell-command-to-string "/usr/bin/firefox -P default-release")
;	browse-url-browser-function 'browse-url-generic)

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
(setq org-agenda-start-with-log-mode t)
(setq org-long-done 'time)
(setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d!)")))
(setq org-refile-targets '(("archive.org" :maxlevel . 1)))

(defun dw/read-file-as-string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(after! org
  (setq org-capture-templates
    `(("t" "Tasks / Projects")
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

;; Save org buffers after re-filing:
(advice-add 'org-refile :after 'org-save-all-org-buffers)

; This was getting in the way of rotate:
(with-eval-after-load 'org (define-key org-mode-map (kbd "M-{") nil))
(with-eval-after-load 'org (define-key org-mode-map (kbd "M-}") nil))

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
; NOTE: Be sure to follow the install steps: https://github.com/emacs-eaf/emacs-application-framework#install
(use-package! eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :commands (eaf-open
             eaf-open-browser
             eaf-open-jupyter
             +eaf-open-mail-as-html)
  :init
  (defvar +eaf-enabled-apps
    '(org browser mindmap jupyter org-previewer markdown-previewer file-sender video-player))

  (defun +eaf-app-p (app-symbol)
    (memq app-symbol +eaf-enabled-apps))

  (when (+eaf-app-p 'browser)
    ;; Make EAF Browser my default browser
    (setq browse-url-browser-function #'eaf-open-browser)
    (defalias 'browse-web #'eaf-open-browser)

    (map! :localleader
          :map (mu4e-headers-mode-map mu4e-view-mode-map)
          :desc "Open mail as HTML" "h" #'+eaf-open-mail-as-html
          :desc "Open URL (EAF)" "o" #'eaf-open-browser))

  (when (+eaf-app-p 'pdf-viewer)
    (after! org
      ;; Use EAF PDF Viewer in Org
      (defun +eaf--org-open-file-fn (file &optional link)
        "An wrapper function on `eaf-open'."
        (eaf-open file))

      ;; use `emacs-application-framework' to open PDF file: link
      (add-to-list 'org-file-apps '("\\.pdf\\'" . +eaf--org-open-file-fn)))

    (after! latex
      ;; Link EAF with the LaTeX compiler in emacs. When a .tex file is open,
      ;; the Command>Compile and view (C-c C-a) option will compile the .tex
      ;; file into a .pdf file and display it using EAF. Double clicking on the
      ;; PDF side jumps to editing the clicked section.
      (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
      (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
      (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

  :config
  ;; Generic
  (setq eaf-start-python-process-when-require t
        eaf-kill-process-after-last-buffer-closed t
        eaf-fullscreen-p nil)

  ;; Debug
  (setq eaf-enable-debug nil)

  ;; Web engine
  (setq 
        ;; eaf-webengine-font-family (symbol-name (font-get doom-font :family))
        ;; eaf-webengine-fixed-font-family (symbol-name (font-get doom-font :family))
        ;; eaf-webengine-serif-font-family (symbol-name (font-get doom-serif-font :family))
        ;; eaf-webengine-font-size 16
        ;; eaf-webengine-fixed-font-size 16
        eaf-webengine-enable-scrollbar t
        eaf-webengine-scroll-step 200
        eaf-webengine-default-zoom 1.25
        eaf-webengine-show-hover-link t
        eaf-webengine-download-path "~/Downloads"
        eaf-webengine-enable-plugin t
        eaf-webengine-enable-javascript t
        eaf-webengine-enable-javascript-access-clipboard t)

  (when (display-graphic-p)
    (require 'eaf-all-the-icons)
    (mapc (lambda (v) (eaf-all-the-icons-icon (car v)))
          eaf-all-the-icons-alist))

  ;; Browser settings
  (when (+eaf-app-p 'browser)
    (setq eaf-browser-continue-where-left-off t
          eaf-browser-dark-mode nil ;; "follow"
          eaf-browser-enable-adblocker t
          eaf-browser-enable-autofill nil
          eaf-browser-remember-history t
          eaf-browser-ignore-history-list '("google.com/search" "file://")
          eaf-browser-text-selection-color "auto"
          eaf-browser-blank-page-url "https://www.duckduckgo.com"
          eaf-browser-chrome-history-file "~/.config/google-chrome/Default/History"
          eaf-browser-default-search-engine "google"
          eaf-browser-continue-where-left-off t
          eaf-browser-aria2-auto-file-renaming t
          eaf-browser-keybinding '(
            ("C-u" . "scroll_down") 
            ("C-d" . "scroll_up")
            ("C-p" . "history_backward")
            ("C-n" . "history_forward")
            ("C-+" . "zoom_in")
            ("C-_" . "zoom_out")
            ("q" . "insert_or_close_buffer") 
            ("y" . "yank_text")
            ("v" . "select_text") 
            ("/" . "search_text_forward")
            ("SPC" . nil) 
            ) )

    (require 'eaf-browser)

    (defun +eaf-open-mail-as-html ()
      "Open the html mail in EAF Browser."
      (interactive)
      (let ((msg (mu4e-message-at-point t))
            ;; Bind browse-url-browser-function locally, so it works
            ;; even if EAF Browser is not set as a default browser.
            (browse-url-browser-function #'eaf-open-browser))
        (if msg
            (mu4e-action-view-in-browser msg)
          (message "No message at point.")))))

  ;; File manager settings
  (when (+eaf-app-p 'file-manager)
    (setq eaf-file-manager-show-preview nil
          eaf-find-alternate-file-in-dired t
          eaf-file-manager-show-hidden-file t
          eaf-file-manager-show-icon t)
    (require 'eaf-file-manager))

  ;; File Browser
  (when (+eaf-app-p 'file-browser)
    (require 'eaf-file-browser))

  ;; PDF Viewer settings
  (when (+eaf-app-p 'pdf-viewer)
    (setq eaf-pdf-dark-mode "follow"
          eaf-pdf-show-progress-on-page nil
          eaf-pdf-dark-exclude-image t
          eaf-pdf-notify-file-changed t)
    (require 'eaf-pdf-viewer))

  ;; Org
  (when (+eaf-app-p 'rss-reader)
    (setq eaf-rss-reader-split-horizontally nil
          eaf-rss-reader-web-page-other-window t)
    (require 'eaf-org))

  ;; Org
  (when (+eaf-app-p 'org)
    (require 'eaf-org))

  ;; Mail
  ;; BUG The `eaf-open-mail-as-html' is not working,
  ;;     I use `+eaf-open-mail-as-html' instead
  (when (+eaf-app-p 'mail)
    (require 'eaf-mail))

  ;; Org Previewer
  (when (+eaf-app-p 'org-previewer)
    (setq eaf-org-dark-mode "follow")
    (require 'eaf-org-previewer))

  ;; Markdown Previewer
  (when (+eaf-app-p 'markdown-previewer)
    (setq eaf-markdown-dark-mode "follow")
    (require 'eaf-markdown-previewer))

  ;; Jupyter
  (when (+eaf-app-p 'jupyter)
    (setq eaf-jupyter-dark-mode "follow"
          eaf-jupyter-font-family (symbol-name (font-get doom-font :family))
          eaf-jupyter-font-size 13)
    (require 'eaf-jupyter))

  ;; Mindmap
  (when (+eaf-app-p 'mindmap)
    (setq eaf-mindmap-dark-mode "follow"
          eaf-mindmap-save-path "~/Dropbox/Mindmap")
    (require 'eaf-mindmap))

  ;; File Sender
  (when (+eaf-app-p 'file-sender)
    (require 'eaf-file-sender))

  ;; Music Player
  (when (+eaf-app-p 'music-player)
    (require 'eaf-music-player))

  ;; Video Player
  (when (+eaf-app-p 'video-player)
    (setq eaf-video-player-keybinding
          '(("p" . "toggle_play")
            ("q" . "close_buffer")
            ("h" . "play_backward")
            ("l" . "play_forward")
            ("j" . "decrease_volume")
            ("k" . "increase_volume")
            ("f" . "toggle_fullscreen")
            ("R" . "restart")))
    (require 'eaf-video-player))

  ;; Image Viewer
  (when (+eaf-app-p 'image-viewer)
    (require 'eaf-image-viewer))

  ;; Git
  (when (+eaf-app-p 'git)
    (require 'eaf-git))

  ;; Fix EVIL keybindings
  (after! evil
    (require 'eaf-evil)
    (define-key key-translation-map (kbd "SPC")
      (lambda (prompt)
        (if (derived-mode-p 'eaf-mode)
            (pcase eaf--buffer-app-name
              ("browser" (if (eaf-call-sync "execute_function" eaf--buffer-id "is_focus")
                             (kbd "SPC")
                           (kbd eaf-evil-leader-key)))
              ("pdf-viewer" (kbd eaf-evil-leader-key))
              ("image-viewer" (kbd eaf-evil-leader-key))
              ("music-player" (kbd eaf-evil-leader-key))
              ("video-player" (kbd eaf-evil-leader-key))
              ("file-sender" (kbd eaf-evil-leader-key))
              ("mindmap" (kbd eaf-evil-leader-key))
              (_  (kbd "SPC")))
          (kbd "SPC"))))))

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
  )

(setq message-kill-buffer-on-exit t)


;; Telega ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq telega-server-libs-prefix "/usr")
(setq telega-use-images 1)
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

;; explain-pause-top ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: this isn't working, seemingly
(setq explain-pause-top-auto-refresh-interval 1)

(defun open-explain-pause-top-in-new-frame ()
  "Open `explain-pause-top` in a new frame."
  (interactive)
  (let ((new-frame (make-frame '((name . "Emacs top")) )))
    (select-frame new-frame)
    (call-interactively #'explain-pause-top)))

(evil-define-key 'normal 'global (kbd "C-t") 'open-explain-pause-top-in-new-frame)
