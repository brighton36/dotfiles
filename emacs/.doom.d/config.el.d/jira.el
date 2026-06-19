;;; jira.el -*- lexical-binding: t; -*-

(setq
  jira-debug nil
  jira-base-url (get-secret 'jira-base-url)
  jira-token-is-personal-access-token nil
  jira-api-version 3
  jira-username (get-secret 'jira-username)
  jira-token (get-secret 'jira-token)
  jira-issues-max-results 60
  jira-issues-table-fields '(:key :issue-type-name :status-name :assignee-name :summary)
  jira-issues-sort-key '("Status" . t)
  )

;;; Configure jire.el for Evil mode keybindings
(map!
  :map jira-issues-mode-map
  :desc "jira Actions" :n "i?" 'jira-issues-actions-menu
  :desc "jira Issues" :n "il" 'jira-issues-menu
  :desc "jira Jump to Tempo" :n "it" '(lambda () (interactive) (jira-issues--jump-to-tempo))
  :desc "jira Find Issue" :n "if" '(lambda () (interactive) (jira-detail-find-issue-by-key))
  :desc "jira Copy Key to Clipboard" :n "ic" '(lambda () (interactive) (jira-actions-copy-issues-id-to-clipboard (jira-utils-marked-item)))
  :desc "jira Change Issue" :n "iC" 'jira-actions-change-issue-menu
  :desc "jira Show Issue" :n "ii" '(lambda () (interactive) (jira-detail-show-issue (jira-utils-marked-item)))
  :desc "jira Open Issue in browser" :n "io" '(lambda () (interactive) (jira-actions-open-issue (jira-utils-marked-item)))
  :desc "jira Add Worklog" :n "iw" 'jira-actions-add-worklog-menu)

(map!
  :map jira-detail-mode-map
  :n "?" 'jira-detail--actions-menu
  :n "+" '(lambda () (interactive ) (jira-detail--add-comment jira-detail--current-key))
  :n "-" '(lambda () "Remove comment at point" (interactive) (jira-detail--remove-comment-at-point))
  :n "C" '(lambda () "Change issue status" (interactive) (jira-detail--change-issue-status))
  :n "O" '(lambda () "Open issue in browser" (interactive)  (jira-actions-open-issue jira-detail--current-key))
  :n "U" '(lambda () "Update issue field" (interactive) (jira-detail--update-field))
  :n "w" 'jira-detail--watchers-menu
  :n "f" '(lambda () "Find issue by key" (interactive) (jira-detail-find-issue-by-key))
  :n "c" '(lambda () "Copy selected issue id to clipboard" (interactive) (jira-actions-copy-issues-id-to-clipboard jira-detail--current-key))
  :n "g" '(lambda () "Refresh issue detail" (interactive) (jira-detail-show-issue jira-detail--current-key))
  :n "P" '(lambda () "Show parent issue" (interactive) (jira-detail--show-parent-issue))
  :n "S" '(lambda () "Add subtask" (interactive) (jira-detail--create-subtask)))
