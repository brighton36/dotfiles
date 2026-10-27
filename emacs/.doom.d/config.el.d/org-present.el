;;; org-present.el -*- lexical-binding: t; -*-

;; Presentations from an org buffer: one top-level heading per slide.

;;   M-x org-present      start (narrows to the current slide)
;;   n / p                next / previous slide (evil normal state)
;;   <right> / <left>     the package's own bindings -- shadowed by evil's
;;                        evil-forward-char in normal state; they still work in
;;                        evil's emacs state
;;   C-c <   / C-c >      first / last slide
;;   C-c C-1              toggle "one big page" (all slides at once)
;;   C-c C-= / C-c C--    bigger / smaller text
;;   C-c C-r / C-c C-w    buffer read-only / read-write
;;   C-c C-q              quit, back to plain org

;; The package is deliberately minimal: it does nothing cosmetic by itself.
;; The hooks below are the setup its README recommends -- bigger text, inline
;; images, hidden cursor, read-only while presenting -- and the quit hook undoes
;; it.  Quitting via C-c C-q is what fires that hook (org-present-quit), so exit
;; the presentation that way rather than by toggling org-present-mode off.

(autoload 'org-present "org-present" nil t)

(with-eval-after-load 'org-present
  (defun org-present-setup ()
    "Presentation look for `org-present'."
    (org-present-big)
    (org-display-inline-images))

  (defun org-present-teardown ()
    "Undo `org-present-setup'."
    (org-present-small)
    (org-remove-inline-images))

  (add-hook 'org-present-mode-hook #'org-present-setup)
  (add-hook 'org-present-mode-quit-hook #'org-present-teardown))

;; n / p for paging.  The package binds <right>/<left>, but evil's normal-state
;; map outranks a minor mode's keymap, so those lose to evil-forward-char there
;; (they still reach org-present in evil's emacs state).  `map!' with :n gives
;; general's :states binding, which general implements with `evil-define-key*':
;; an auxiliary keymap stored inside org-present-mode-keymap under
;; [normal-state], which evil places ahead of the state map itself.  Evil's "n"
;; here is evil-ex-search-next and "p" is evil-paste-after, so both are
;; overridden during a presentation only -- the aux keymap lives in the minor
;; mode's keymap and is active only while the mode is on.
(map! :after org-present
      :map org-present-mode-keymap
      :n "n" #'org-present-next
      :n "p" #'org-present-prev)

;; That auxiliary keymap is created when org-present loads, which -- because of
;; the autoload above -- is the moment you first run M-x org-present, i.e. after
;; evil has already cached the keymap chain for the current buffer.  Without
;; renormalizing, the bindings silently do not take effect on that first
;; presentation and "n" keeps going to evil-ex-search-next.  Rebuilding the
;; chain when the mode starts is what closes that window.
(add-hook 'org-present-mode-hook #'evil-normalize-keymaps)
