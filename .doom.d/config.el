;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Chris DeRose"
      user-mail-address "chris@chrisderose.com")

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
(setq doom-theme 'doom-solarized-light)

;; Background color of the line numbers column, and highlight current line color:
(custom-set-faces!
  `(line-number :background ,(doom-color 'bg-alt))
  `(line-number-current-line :background ,(doom-color 'grey)))

;; Unfocused window colors:
(custom-set-faces
 ;; solarize base2
 '(auto-dim-other-buffers-face ((t (:background "#eee8d5"))))
 )

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

;; Custom keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alt-[ and Alt-] Window Cycle:
(global-set-key (kbd "M-]") '(lambda() (interactive) (other-window 1)))
(global-set-key (kbd "M-[") '(lambda() (interactive) (other-window -1)))

;; Ctrl-p and Ctrl-n for  Previous/Next Tab
(map! :nv "C-n" #'+workspace/switch-right :nv "C-p" #'+workspace/switch-left)

;; evil bindings:
;; https://github.com/noctuid/evil-guide#keybindings-and-states

;; In normal mode, ctrl-s will 'search' for an open buffer
(define-key evil-normal-state-map (kbd "C-s") #'+vertico/switch-workspace-buffer)


;; TODO: See if we still need/want this
;; This 'locks' (pins) the window, so it can't be closed:
;; NOTE, maybe try this: (set-window-parameter ook-window 'no-delete-other-windows t)
;; Per: https://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00975.html
;; then M-x sticky-buffer-mode
;; TO
;; (global-set-key (kbd "C-l") 'locked-buffer-mode)
;; (define-minor-mode sticky-buffer-mode
;;   "Make the current window always display this buffer."
;;   nil " sticky" nil
;;   (set-window-dedicated-p (selected-window) sticky-buffer-mode))

