;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq org-agenda-files (list "inbox.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-long-done 'time)
(setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d!)")))
(setq org-refile-targets '(("archive.org" :maxlevel . 1)))

; TODO: this is crashing the lisp: ;(set-fill-column 0)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1) (display-line-numbers-mode 0)))

; This was getting in the way of the global map shortcuts
(after! org
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
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/org/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
      ))
  )

(let* ((variable-tuple '(:font "Ubuntu"))
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
