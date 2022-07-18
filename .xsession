#!/usr/bin/bash

# NOTE: .xprofile seems to be the best place to set environment variables

# This enables tap for escape on the capslock key (which is control_l, 
# apparently, due to the above modmaps
xcape -e 'Control_L=Escape'

# This should enable the liberation font in xscreensaver:
xset +fp /usr/share/fonts/ubuntu/
xset fp rehash

/usr/bin/xscreensaver -no-splash &

/usr/bin/picom -b

/usr/bin/feh --bg-max --randomize "$HOME/Pictures/Wallpapers/"

# Event Notifications:
/usr/bin/dunst &

# Tray Icons/Apps
/usr/bin/nm-applet &
/usr/bin/blueman-applet &
/bin/bash -c "sleep 4; /usr/bin/telegram-desktop -startintray &" &
/usr/bin/autokey-gtk &

/usr/bin/emacs --daemon &
