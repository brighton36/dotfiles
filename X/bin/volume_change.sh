#!/bin/bash
# changeVolume

# Arbitrary but unique message tag
msgTag="myvolume"

# Change the volume using alsa(might differ if you use pulseaudio)
# amixer -c 0 set Master "$@" > /dev/null

case $1 in
  up)
    /usr/bin/pactl -- set-sink-volume 0 "+10%"
    ;;
  down)
    /usr/bin/pactl -- set-sink-volume 0 "-10%"
    ;;
  mute)
    /usr/bin/pactl -- set-sink-mute 0 toggle
    ;;
  show)
    ;;

  *)
    echo "ERROR: Unknown parameter \"$1\""
    exit
esac

echo "1"
# Query amixer for the current volume and whether or not the speaker is muted
# volume="$(amixer -c 0 get Master | tail -1 | awk '{print $4}' | sed 's/[^0-9]*//g')"
# mute="$(amixer -c 0 get Master | tail -1 | awk '{print $6}' | sed 's/[^a-z]*//g')"
volume="$(/usr/bin/pactl -- get-sink-volume 0 | /bin/grep -oP '[^ ]+\%' | /bin/head --lines=1)"
mute="$(/bin/pactl -- get-sink-mute 0 | /bin/grep -oP '[^ ]+$')"

echo "2"
function beep {
  # Play the volume changed sound
  # canberra-gtk-play -i audio-volume-change -d "changeVolume"
  /usr/bin/mpv /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga
}

if [[ $1 == "show" ]]; then
  # We're mostly just using this in xmobar
  echo "${volume}"
elif [[ $volume == 0 || "$mute" == "yes" ]]; then
    # Show the sound muted notification
    dunstify -a "changeVolume" -u low -i audio-volume-muted -h string:x-dunst-stack-tag:$msgTag "Volume muted"
    beep
else
    # Show the volume notification
    dunstify -a "changeVolume" -u low -i audio-volume-high -h string:x-dunst-stack-tag:$msgTag \
    -h int:value:"$volume" "Volume: ${volume}"
    beep
fi
echo "3"
