#!/usr/bin/bash

setxkbmap `cat "$HOME/.Xkbmap"`

# This mostly just bind's l_control to super
xmodmap "$HOME/.Xmodmap"

# This should enable the liberation font in xscreensaver:
xset +fp /usr/share/fonts/ubuntu/
xset fp rehash

/usr/bin/xscreensaver -no-splash &
# NOTE: The no-fading-openclose *should* prevent the desktop from displaying
# on resume.
/usr/bin/picom -b --no-fading-openclose

/usr/bin/feh --bg-max --randomize "$HOME/Pictures/Wallpapers/"

/usr/bin/nm-applet &
/usr/bin/volumeicon &
/usr/bin/blueman-applet &
/usr/bin/telegram-desktop &

