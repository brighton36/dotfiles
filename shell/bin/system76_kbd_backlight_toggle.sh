#!/bin/bash

CAT="/usr/bin/cat"
KEYBOARD_LED="/sys/class/leds/system76_acpi::kbd_backlight/brightness"

current_brightness=$(($(cat $KEYBOARD_LED)))
new_brightness=0

if [[ $current_brightness -eq 0 ]]; then
  new_brightness=2
elif [[ $current_brightness -eq 2 ]]; then
  new_brightness=5
fi

echo $new_brightness > $KEYBOARD_LED

