#!/bin/bash

/usr/bin/ghc -i$HOME/.config/xmobar -threaded -dynamic ~/.config/xmobar/xmobar.hs

# At bootup, we seem to launch too fast. So, if we see that there's no running
# xmobar, we sleep
if ! /usr/bin/pgrep 'xmobar$'; then /usr/bin/sleep 2; fi

/usr/bin/xmobar
