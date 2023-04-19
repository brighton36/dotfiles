#!/bin/bash

/usr/bin/pgrep trayer
if [ $? -ne 0 ]; then
  /usr/bin/trayer --edge top\
    --align right\
    --SetDockType true\
    --SetPartialStrut true\
    --expand true\
    --height 52\
    --distance 36\
    --distancefrom top\
    --tint "0xffffff"\
    --widthtype request\
    --monitor 'primary'\
    --SetPartialStrut false
    --margin 0 &
else
  killall trayer
fi
