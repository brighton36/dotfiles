#!/bin/bash

XDOTOOL=/usr/bin/xdotool

intercept_class=$1
intercept_from=$2
intercept_to=$3

if [ -z "$intercept_class" -o -z "$intercept_from" -o -z "$intercept_to" ]; then
  echo "Usage: $0 INTERCEPT_WM_CLASS FROM_KEYCODE TO_KEYCODE"
  exit
fi

active_window_id=$($XDOTOOL getwindowfocus)
active_window_class=$($XDOTOOL getwindowclassname $active_window_id)

if [ "${active_window_class}" = "$intercept_class" ]; then
  $XDOTOOL key --window "$active_window_id" "$intercept_to"
else
  $XDOTOOL key --window "$active_window_id" "$intercept_from"
fi
