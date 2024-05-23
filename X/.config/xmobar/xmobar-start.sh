#!/bin/bash

/usr/bin/ghc -i$HOME/.config/xmobar -package xmobar -threaded -dynamic ~/.config/xmobar/xmobar.hs

/usr/bin/xmobar
