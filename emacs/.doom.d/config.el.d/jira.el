;;; jira.el -*- lexical-binding: t; -*-

(setq
  jira-debug nil
  jira-base-url (get-secret 'jira-base-url)
  jira-token-is-personal-access-token nil
  jira-api-version 3
  jira-username (get-secret 'jira-username)
  jira-token (get-secret 'jira-token))

;;; Configure jire.el for Evil mode keybindings
(map!
  :map jira-issues-mode-map
  :n "g?" 'jira-issues-actions-menu
  :n "gl" 'jira-issues-menu
  :n "gt" '(lambda () (interactive) (jira-issues--jump-to-tempo))
  :n "gf" '(lambda () (interactive) (jira-detail-find-issue-by-key))
  :n "gc" '(lambda () (interactive) (jira-actions-copy-issues-id-to-clipboard (jira-utils-marked-item)))
  :n "gC" 'jira-actions-change-issue-menu
  :n "gi" '(lambda () (interactive) (jira-detail-show-issue (jira-utils-marked-item)))
  :n "go" '(lambda () (interactive) (jira-actions-open-issue (jira-utils-marked-item)))
  :n "gw" 'jira-actions-add-worklog-menu)

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
