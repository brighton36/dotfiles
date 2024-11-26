#!/usr/bin/env bash

handle() {
  match_workspace="^workspace>>(.*)"
  if [[ $1 =~ $match_workspace ]]; then
    # NOTE at the time of writing, this feature doesn't work: hyprctl keyword misc:background_color 65535
    hyprctl hyprpaper wallpaper ",~/.config/hypr/workspace-${BASH_REMATCH[1]}.png"
  fi
}

socat -U - UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock | while read -r line; do handle "$line"; done
