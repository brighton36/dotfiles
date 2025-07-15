
(require 'slack)

(slack-register-team
     :name (get-secret 'slack-team-name)
     :token (get-secret 'slack-token)
     :cookie (get-secret 'slack-cookie)
     :full-and-display-names t
     :default t
     :subscribed-channels nil)

(set 'slack-extra-subscribed-channels (mapcar 'intern (get-secret 'slack-subscribed-channels)))

(slack-start)

(map!
  (:prefix-map ("C-c s" . "slack")
    :desc "Stop" "K" #'slack-stop
    :desc "Select Room" "c" #'slack-select-rooms
    :desc "Select Unread Room" "u" #'slack-select-unread-rooms
    :desc "Select User" "U" #'slack-user-select
    :desc "Search messages" "s" #'slack-search-from-messages
    :desc "Jump to Browser" "J" #'slack-jump-to-browser
    :desc "Jump to App" "j" #'slack-jump-to-app
    :desc "Insert Emoji" "e" #'slack-insert-emoji
    :desc "Edit Message" "E" #'slack-message-edit
    :desc "Select Im" "i" #'slack-im-select
    :desc "Add Message Reaction" "r" #'slack-message-add-reaction
    :desc "Thread Show or Create" "t" #'slack-thread-show-or-create
    :desc "Message Redisplay" "g" #'slack-message-redisplay
    :desc "Conversations Quick Update" "G" #'slack-conversations-list-update-quick
    :desc "Quote and Reply" "q" #'slack-quote-and-reply
    :desc "Quote and Reply with link" "Q" #'slack-quote-and-reply-with-link
   )

  :map slack-mode-map
    :n "q" 'evil-delete-buffer
    :i "@" 'slack-message-embed-mention
    :i "#" 'slack-message-embed-channel
  :map slack-thread-message-buffer-mode-map
    :n "q" 'evil-delete-buffer
    :i "C-c '" 'slack-message-write-another-buffer
    :i "@" 'slack-message-embed-mention
    :i "#" 'slack-message-embed-channel
  :map slack-message-buffer-mode-map
    :n "q" 'evil-delete-buffer
    :i "C-c '" 'slack-message-write-another-buffer
  :map slack-message-compose-buffer-mode-map
    :n "q" 'evil-delete-buffer
    :i "C-c '" 'slack-message-send-from-buffer
)

; Seems like the newer versions of emacs causes warnings in slack mode, unless we disable that behavior in this way
(setq eieio-backward-compatibility null)

