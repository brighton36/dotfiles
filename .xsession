#!/usr/bin/bash

setxkbmap `cat "$HOME/.Xkbmap"`

# This mostly just bind's l_control to super
xmodmap "$HOME/.Xmodmap"

if [$hostname == "link"]; then
  export GDK_SCALE=2
fi

# This should enable the liberation font in xscreensaver:
xset +fp /usr/share/fonts/ubuntu/
xset fp rehash

/usr/bin/xscreensaver -no-splash &
/usr/bin/picom -b

/usr/bin/nm-applet &
/usr/bin/pnmixer &
/usr/bin/blueman-applet &
/usr/bin/cbatticon -l 20 -c 10 &
/usr/bin/psensor &

