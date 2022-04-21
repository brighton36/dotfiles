#!/usr/bin/bash

setxkbmap `cat "$HOME/.Xkbmap"`

# This mostly just bind's l_control to super
xmodmap "$HOME/.Xmodmap"

# This should enable the liberation font in xscreensaver:
xset +fp /usr/share/fonts/ubuntu/
xset fp rehash

/usr/bin/xscreensaver -no-splash &

# I just disabled this after it seemed impossible to get the window borders
# opaque:
# NOTE: The no-fading-openclose *should* prevent the desktop from displaying
# on resume.
# /usr/bin/picom -b --no-fading-openclose

# I stopped liking the background image...
# /usr/bin/feh --bg-max --randomize "$HOME/Pictures/Wallpapers/"
# Let's just set it to solarized base1
/usr/bin/xsetroot -solid '#93a1a1'

/usr/bin/dunst &
/usr/bin/nm-applet &
/usr/bin/volumeicon &
/usr/bin/blueman-applet &
/bin/bash -c "sleep 4; /usr/bin/telegram-desktop -startintray &" &

/usr/bin/emacs --daemon &
