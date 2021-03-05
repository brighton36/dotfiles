# NOTE: fish_key_reader generates these binding codes:

fish_vi_key_bindings --no-erase

bind -M insert \cB backward-word
bind -M insert \cF forward-word

bind -M insert \cA beginning-of-line
bind -M insert \cE end-of-line
bind -M insert \cW backward-kill-word

# Alt-left and Alt-right:
# TODO: This is screwing up our left/right
#bind -M insert \e\[C 'nextd-or-forward-word'
#bind -M insert \e\[D 'prevd-or-backward-word'

# Note: The .inputrc might be required to make this work:
if [ $TERM = 'linux' ]
  # This is for the tty consoles:
  bind -M insert '[C' forward-word
  bind -M insert '[D' backward-word
else
  # This is for urxt, and really any other term:
  bind -M insert \eOd backward-word
  bind -M insert \eOc forward-word
end

# Our config alias to manage our dotfiles in git:
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
