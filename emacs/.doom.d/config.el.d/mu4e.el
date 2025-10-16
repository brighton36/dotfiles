; mu4e.el --- Description -*- lexical-binding: t; -*-

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e") 
(require 'org-msg)

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

(setq 
  ; SMTP Settings:
  message-send-mail-function 'smtpmail-send-it
  smtpmail-stream-type 'starttls
  smtpmail-default-smtp-server (get-secret 'mu4e-smtp-server)
  smtpmail-smtp-server (get-secret 'mu4e-smtp-server)
  smtpmail-smtp-service (get-secret 'mu4e-smtp-port)
  smtpmail-smtp-user (get-secret 'mu4e-smtp-username)

  ; Org-msg settings:
  ; NOTE: I don't think I'm actually using this at the moment. But, I'd like to...
  mail-user-agent 'gnus-user-agent
  message-kill-buffer-on-exit t
  org-msg-startup "hidestars indent inlineimages"
  org-msg-greeting-fmt "\nHello %s,\n\n"
  org-msg-recipient-names '(((get-secret 'mu4e-from-address) . (get-secret 'mu4e-from-fullname)))
  org-msg-greeting-name-limit 3
  org-msg-default-alternatives '((new   . (text html))
  (reply-to-html . (text html))
  (reply-to-text . (text)))
  org-msg-convert-citation t
  org-msg-signature (concat "\n\nRegards,\n\n#+begin_signature\n--\n*"
                      (get-secret 'mu4e-from-fullname) "\n/"
                      (get-secret 'mu4e-from-address) "/\n#+end_signature"))
(org-msg-mode)

; NOTE: The issue here, is that .emacs.d/modules/email/mu4e/config.el is loading after this file
;       so, we can just hook it here, like so:
(after! mu4e
  (setq 
    ; NOTE: I tried setting up the sync in ~/.config/systemd/user/mbsync.service
    ; Per: https://wiki.archlinux.org/title/isync#With_a_timer
    ; But I think this works better:
    mu4e-update-interval 60

    mu4e-change-filenames-when-moving t   ; needed for mbsync
    mu4e-get-mail-command (get-secret 'mu4e-get-mail-command)
    mu4e-attachment-dir  (get-secret 'mu4e-attachment-dir)
    mu4e-sent-folder     (get-secret 'mu4e-sent-folder)
    mu4e-drafts-folder   (get-secret 'mu4e-drafts-folder)
    mu4e-trash-folder    (get-secret 'mu4e-trash-folder)
    mu4e-refile-folder   (get-secret 'mu4e-refile-folder)

    mu4e-headers-auto-update t
    mu4e-compose-format-flowed t
    fill-flowed-encode-column 80
    mu4e-index-cleanup nil
    mu4e-view-auto-mark-as-read nil
    mu4e-index-lazy-check t
    mu4e-use-fancy-chars t
    mu4e-compose-signature (concat (get-secret 'mu4e-from-fullname) "\n" (get-secret 'mu4e-from-address) "\n")
    mu4e-headers-date-format "%y-%m-%d")

  (add-to-list 'mu4e-bookmarks
    '(:name "Inbox Unread"
      :key  ?i
      :query "maildir:/inbox AND flag:unread"))
  )
