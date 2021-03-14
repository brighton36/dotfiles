#!/usr/bin/bash

setxkbmap `cat "$HOME/.Xkbmap"`

# This mostly just bind's l_control to super
xmodmap "$HOME/.Xmodmap"

# This will synchronize the urxt (and other X programs) clipboards 
autocutsel -fork -selection CLIPBOARD
autocutsel -fork -selection PRIMARY

/usr/bin/xscreensaver -no-splash &
/usr/bin/picom -b

export GDK_SCALE=2
/usr/bin/nm-applet &
/usr/bin/pnmixer &
/usr/bin/blueman-manager &
/usr/bin/cbatticon -l 20 -c 10 &
/usr/bin/psensor &

/usr/bin/telegram-desktop &
