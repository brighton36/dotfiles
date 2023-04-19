#!/bin/bash

UPOWER="/usr/bin/upower"
GREP="/usr/bin/grep"
TR="/usr/bin/tr"
SYSTEMCTL="/usr/bin/systemctl"

BATTERY_PATH="/org/freedesktop/UPower/devices/battery_BAT0"
at_level=$1

if ! [[ "$at_level" =~ ^[0-9]+$  ]]; then
  echo "Usage: $0 BATTERY_LEVEL_AS_PERCENT"
  exit
fi

while [ $($UPOWER -i $BATTERY_PATH | $GREP percentage | $TR -dc 0123456789) -gt $at_level ]; do
  sleep 1;
done

$SYSTEMCTL hibernate
