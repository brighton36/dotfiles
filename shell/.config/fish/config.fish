## Bindings: ##################################################################
if [ -n "$INSIDE_EMACS" ]
  # Vterm
  set -g fish_key_bindings "fish_default_key_bindings"
else
  set -g fish_key_bindings "fish_vi_key_bindings"
end

# Basic Emacs keys
bind -M insert \cB backward-char
bind -M insert \cF forward-char

bind -M insert \cA beginning-of-line
bind -M insert \cE end-of-line
bind -M insert \cW backward-kill-word
bind -M insert \cK kill-line

# Ctrl-n and ctrl-p
bind -M insert \cp up-or-search
bind -M insert \cn down-or-search

## Terminal Emulator hacks: ##################################################
# NOTE: fish_key_reader generates these binding codes:
# NOTE: The .inputrc might be required to make this work:
if [ -n "$TMUX" ]
  # urxt with tmux:
  bind -M insert \e\[1\;5D backward-char
  bind -M insert \e\[1\;5C forward-char
  bind \e\[1\;5D backward-word
  bind \e\[1\;5C forward-word
else
  bind -M insert \eOd backward-char
  bind -M insert \eOc forward-char
  bind \eOd backward-word
  bind \eOc forward-word
end

# Emacs vterm
function vterm_printf;
  if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
    # tell tmux to pass the escape sequences through
    printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
  else if string match -q -- "screen*" "$TERM"
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$argv"
  else
    printf "\e]%s\e\\" "$argv"
  end
end

## Theme: #####################################################################
set fish_greeting

set -g theme_color_scheme solarized-light

## Environment: ###############################################################
export LEDGER_FILE=$HOME/ledger/chris-derose.journal

## OS specific variables #####################################################
if test (uname) = Darwin
  # homebrew
  /opt/homebrew/bin/brew shellenv | source

  # Move asdf to the origin:
  fish_add_path -m $HOME/.asdf/shims
else
  # Guix:
  if type -q 'fenv'
    fenv export GUIX_PROFILE="$HOME/.guix-profile"
    fenv source "$GUIX_PROFILE/etc/profile"
  end
end
