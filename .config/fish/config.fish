fish_vi_key_bindings --no-erase
bind -M insert \cB backward-word
bind -M insert \cF forward-word
bind -M insert \cA beginning-of-line
bind -M insert \cE end-of-line
bind -M insert \cW backward-kill-word
bind -M insert \e\[1\;5C forward-word
bind -M insert \e\[1\;5D backward-word

# Dotfile management:
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
