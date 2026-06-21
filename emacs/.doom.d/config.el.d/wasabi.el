; wasabi.el --- Description -*- lexical-binding: t; -*-

(setq
  wasabi-wuzapi-command `(,(format "%s/bin/wuzapi/wuzapi" (getenv "HOME"))
                           "-mode=stdio"
                           ,(format "-datadir=%s" (expand-file-name "wasabi" user-emacs-directory)))
  wasabi-user-token (get-secret 'wasabi-user-token))
