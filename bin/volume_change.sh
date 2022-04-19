#!/bin/bash
# changeVolume

# Arbitrary but unique message tag
msgTag="myvolume"

# Change the volume using alsa(might differ if you use pulseaudio)
# amixer -c 0 set Master "$@" > /dev/null
/usr/bin/pactl -- set-sink-volume 0 "$@"

# Query amixer for the current volume and whether or not the speaker is muted
# volume="$(amixer -c 0 get Master | tail -1 | awk '{print $4}' | sed 's/[^0-9]*//g')"
# mute="$(amixer -c 0 get Master | tail -1 | awk '{print $6}' | sed 's/[^a-z]*//g')"
volume="$(/usr/bin/pactl -- get-sink-volume 0 | /bin/grep -oP '[\d]+\%' | /bin/head --lines=1)"
mute="$(/bin/pactl -- get-sink-mute 0 | /bin/grep -oP '[^ ]+$')"
if [[ $volume == 0 || "$mute" == "off" ]]; then
    # Show the sound muted notification
    dunstify -a "changeVolume" -u low -i audio-volume-muted -h string:x-dunst-stack-tag:$msgTag "Volume muted"
else
    # Show the volume notification
    dunstify -a "changeVolume" -u low -i audio-volume-high -h string:x-dunst-stack-tag:$msgTag \
    -h int:value:"$volume" "Volume: ${volume}"
fi

# Play the volume changed sound
canberra-gtk-play -i audio-volume-change -d "changeVolume"
