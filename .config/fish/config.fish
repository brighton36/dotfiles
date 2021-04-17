if [ "$TERM" = "xterm-kitty" ]
  # This fixes weird shape output on kitty terminal. It doesn't seem to cause
  # problems outside kitty. But, I'm only enabling it for kitty nonetheless...
  # NOTE: kitty outputs this error. But, the OSC code 50 seems valid: 
  #   [PARSE ERROR] Unknown OSC code: 50
  function fish_vi_cursor --on-variable fish_bind_mode
    if set -q __last_fish_bind_mode
      and test $__last_fish_bind_mode = $fish_bind_mode
      return
    end
    set -g __last_fish_bind_mode $fish_bind_mode
    switch $fish_bind_mode
      case insert
        printf '\e]0;CursorShape=1\x7'
      case default
        printf '\e]50;CursorShape=0\x7'
      case "*"
        printf '\e]50;CursorShape=0\x7'
    end
  end

  set -x fish_cursor_default block
  set -x fish_cursor_visual block
  set -x fish_cursor_insert line
  set -x fish_cursor_replace_one underscore

  alias icat="/usr/bin/kitty +kitten icat --align=left"
  alias clipboard="/usr/bin/kitty +kitten clipboard"
end

fish_vi_key_bindings --no-erase

bind -M insert \cB backward-word
bind -M insert \cF forward-word

bind -M insert \cA beginning-of-line
bind -M insert \cE end-of-line
bind -M insert \cW backward-kill-word

# NOTE: fish_key_reader generates these binding codes:
# NOTE: The .inputrc might be required to make this work:
if [ -n "$TMUX" ]
  # urxt with tmux:
  bind -M insert \e\[1\;5D backward-word
  bind -M insert \e\[1\;5C forward-word
  bind \e\[1\;5D backward-word
  bind \e\[1\;5C forward-word
else
  bind -M insert \eOd backward-word
  bind -M insert \eOc forward-word
  bind \eOd backward-word
  bind \eOc forward-word
end

set -g theme_color_scheme solarized-light

# Our config alias to manage our dotfiles in git:
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
