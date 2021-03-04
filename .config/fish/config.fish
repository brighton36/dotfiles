# NOTE: fish_key_reader generates these binding codes:

fish_vi_key_bindings --no-erase
bind -M insert \cB backward-word
bind -M insert \cF forward-word

bind -M insert \cA beginning-of-line
bind -M insert \cE end-of-line
bind -M insert \cW backward-kill-word

# Note: The .inputrc might be required to make this work:
# This is for urxt:
bind -M insert \eOd 'backward-word'
bind -M insert \eOc 'forward-word'
# This is for the tty consoles:
bind -M insert '[C' forward-word
bind -M insert '[D' backward-word

# Dotfile management:
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
