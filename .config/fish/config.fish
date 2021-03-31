# NOTE: fish_key_reader generates these binding codes:

fish_vi_key_bindings --no-erase

bind -M insert \cB backward-word
bind -M insert \cF forward-word

bind -M insert \cA beginning-of-line
bind -M insert \cE end-of-line
bind -M insert \cW backward-kill-word

# Note: The .inputrc might be required to make this work:
if [ -n "$TMUX" ]
  # urxt with tmux:
  bind -M insert \e\[1\;5D backward-word
  bind -M insert \e\[1\;5C forward-word
  bind \e\[1\;5D backward-word
  bind \e\[1\;5C forward-word
else
  # This seems to be the left key, without ctrl. I think the tty needs to send \eOd
  #bind -M insert \e\[D backward-word
  #bind -M insert \e\[C forward-word
  #bind \e\[D backward-word
  #bind \e\[C forward-word

  # Ubuntu:
  bind -M insert \eOd backward-word
  bind -M insert \eOc forward-word
  bind \eOd backward-word
  bind \eOc forward-word
end

# Our config alias to manage our dotfiles in git:
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
