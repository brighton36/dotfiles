
;(use-package emacs-slack
;  :bind (("C-c S K" . slack-stop)
;         ("C-c S c" . slack-select-rooms)
;         ("C-c S u" . slack-select-unread-rooms)
;         ("C-c S U" . slack-user-select)
;         ("C-c S s" . slack-search-from-messages)
;         ("C-c S J" . slack-jump-to-browser)
;         ("C-c S j" . slack-jump-to-app)
;         ("C-c S e" . slack-insert-emoji)
;         ("C-c S E" . slack-message-edit)
;         ("C-c S r" . slack-message-add-reaction)
;         ("C-c S t" . slack-thread-show-or-create)
;         ("C-c S g" . slack-message-redisplay)
;         ("C-c S G" . slack-conversations-list-update-quick)
;         ("C-c S q" . slack-quote-and-reply)
;         ("C-c S Q" . slack-quote-and-reply-with-link)
;         (:map slack-mode-map
;               (("@" . slack-message-embed-mention)
;                ("#" . slack-message-embed-channel)))
;         (:map slack-thread-message-buffer-mode-map
;               (("C-c '" . slack-message-write-another-buffer)
;                ("@" . slack-message-embed-mention)
;                ("#" . slack-message-embed-channel)))
;         (:map slack-message-buffer-mode-map
;               (("C-c '" . slack-message-write-another-buffer)))
;         (:map slack-message-compose-buffer-mode-map
;               (("C-c '" . slack-message-send-from-buffer)))
;         )
;  :custom
;  (slack-extra-subscribed-channels (mapcar 'intern (list "some-channel")))
;  :config
;  (slack-register-team
;     :name "clojurians"
;     :token "xoxc-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
;     :cookie "xoxd-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj; d-s=888888888888; lc=888888888888"
;     :full-and-display-names t
;     :default t
;     :subscribed-channels nil ;; using slack-extra-subscribed-channels because I can change it dynamically
;     ))

;(use-package alert
;  :commands (alert)
;  :init
;  (setq alert-default-style 'notifier))
(require 'slack)
(slack-register-team
     :name (get-secret 'slack-team-name)
     :token (get-secret 'slack-token)
     :cookie (get-secret 'slack-cookie)
     :full-and-display-names t
     :default t
     :subscribed-channels nil ;; using slack-extra-subscribed-channels because I can change it dynamically
     )

;(map!
;  :g
;  "C-c S K" slack-stop
;  "C-c S c" slack-select-rooms
;  "C-c S u" slack-select-unread-rooms
;  "C-c S U" slack-user-select
;  "C-c S s" slack-search-from-messages
;  "C-c S J" slack-jump-to-browser
;  "C-c S j" slack-jump-to-app
;  "C-c S e" slack-insert-emoji
;  "C-c S E" slack-message-edit
;  "C-c S r" slack-message-add-reaction
;  "C-c S t" slack-thread-show-or-create
;  "C-c S g" slack-message-redisplay
;  "C-c S G" slack-conversations-list-update-quick
;  "C-c S q" slack-quote-and-reply
;  "C-c S Q" slack-quote-and-reply-with-link
;  :map slack-mode-map
;       (("@" . slack-message-embed-mention)
;        ("#" . slack-message-embed-channel)))
;  :map slack-thread-message-buffer-mode-map
;       (("C-c '" . slack-message-write-another-buffer)
;        ("@" . slack-message-embed-mention)
;        ("#" . slack-message-embed-channel)))
;  :map slack-message-buffer-mode-map
;       (("C-c '" . slack-message-write-another-buffer)))
;  :map slack-message-compose-buffer-mode-map
;               (("C-c '" . slack-message-send-from-buffer)))
;)
