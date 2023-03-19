#!/bin/bash

DIR_OUTPUT="$HOME/Pictures/Screenshots"
FILENAME="Screenshot "$(/usr/bin/date "+%Y-%m-%d %I%M%S")".png"

# This was the first version, but, picom made it weird:
# bash -c "sleep 0.25 && scrot -q 90 -s -e 'mv \$f ~/Pictures/Screenshots/'"

tempfile=$(/usr/bin/mktemp)

# Snap the shot, and round the corners:
/usr/bin/maim -s -o | /usr/bin/convert png:- \
  \( +clone  -alpha extract \
    -draw 'fill black polygon 0,0 0,15 15,0 fill white circle 15,15 15,0' \
    \( +clone -flip \) -compose Multiply -composite \
    \( +clone -flop \) -compose Multiply -composite \
  \) -alpha off -compose CopyOpacity -composite \
  "$tempfile"

# Now Drop shadow:
/usr/bin/convert "$tempfile" \( -clone 0 -background black -shadow 80x3+5+5 \) \
  +swap -background none -layers merge +repage "$DIR_OUTPUT/$FILENAME"

# And present:
/usr/bin/feh "$DIR_OUTPUT/$FILENAME"
