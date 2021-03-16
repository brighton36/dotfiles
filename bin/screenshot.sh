#!/bin/bash

DIR_OUTPUT="$HOME/Pictures/Screenshots"
FILENAME="Screenshot "$(/usr/bin/date "+%Y-%m-%d %I:%M:%S")".png"

# This was the first version, but, picom made it weird:
# bash -c "sleep 0.25 && scrot -q 90 -s -e 'mv \$f ~/Pictures/Screenshots/'"

/usr/bin/maim -s -o | /usr/bin/convert - \( +clone -background black -shadow 80x3+5+5 \) +swap -background none -layers merge +repage "$DIR_OUTPUT/$FILENAME"
/usr/bin/viewnior "$DIR_OUTPUT/$FILENAME"
