#!/usr/bin/bash

/usr/bin/xdotool key --clearmodifiers $@
/usr/bin/sleep 0.25
/usr/bin/xdotool keyup Meta_L Meta_R Alt_L Alt_R Super_L Super_R
