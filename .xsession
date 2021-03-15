#!/usr/bin/bash

setxkbmap `cat "$HOME/.Xkbmap"`

# This mostly just bind's l_control to super
xmodmap "$HOME/.Xmodmap"

if [$hostname == "link"]; then
  # 3200x1800 11.56"x6.51"
  xrandr --dpi 276
  export GDK_SCALE=2
else
  # 1. xdpyinfo | grep -B 2 resolution
  # 2. Convert mm, to inches
  # 3. Divide the resolution by the dimension
  # 1920 / 20" = 96
  # 1080 / 11.22" = 96.25
  xrandr --dpi 96
fi

# This will synchronize the urxt (and other X programs) clipboards 
autocutsel -fork -selection CLIPBOARD
autocutsel -fork -selection PRIMARY

/usr/bin/xscreensaver -no-splash &
/usr/bin/picom -b

/usr/bin/nm-applet &
/usr/bin/pnmixer &
/usr/bin/blueman-applet &
/usr/bin/cbatticon -l 20 -c 10 &
/usr/bin/psensor &

