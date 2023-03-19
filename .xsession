#!/usr/bin/bash

# NOTE: .xprofile seems to be the best place to set environment variables

# gdm sets the cursors weird. This sets it back:
xsetroot -cursor_name left_ptr

# This should enable the liberation font in xscreensaver:
xset +fp /usr/share/fonts/ubuntu/
xset fp rehash

/usr/bin/xscreensaver -no-splash &

/usr/bin/picom -b

/usr/bin/feh --bg-max --randomize "$HOME/Pictures/Wallpapers/"

# Event Notifications:
/usr/bin/dunst &

/usr/bin/keynav

# Tray Icons/Apps
/usr/bin/nm-applet &
/usr/bin/blueman-applet &
# We switched to telega
# /bin/bash -c "sleep 4; /usr/bin/telegram-desktop -startintray &" &

# This xhost + is maybe less secure? But was needed for autokey-gtk
/usr/bin/xhost +
/usr/bin/autokey-gtk &

/usr/bin/emacs --daemon &
