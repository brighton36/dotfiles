#!/usr/bin/env bash

HYPRCTL="/usr/bin/hyprctl"

function isNumber(){
  match_number="^[0-9]+$"
  if [[ $1 =~ $match_number ]]; then
    exit 0
  fi

  exit 1
}

function getActiveWorkspaceNum(){
  match_workspace="^workspace ID ([^ ]+)"
  if [[ $($HYPRCTL activeworkspace) =~ $match_workspace ]]; then
    echo ${BASH_REMATCH[1]}
  else
    echo "Fatal Error: Unable to determine active workspace."
    return 1
  fi
}

echo $@ >> /tmp/hypr-ws-change.log
if $(isNumber $1); then
  toWorkspace=$1
else
  toWorkspace=$(getActiveWorkspaceNum)
  case $1 in
    "next")
      if ((toWorkspace == 9)); then
        toWorkspace=1
      else
        toWorkspace=$((toWorkspace+1))
      fi ;;
    "prev")
      if ((toWorkspace == 1)); then
        toWorkspace=9
      else
        toWorkspace=$((toWorkspace-1))
      fi ;;
    *)
      echo "Fatal Error: Unable to parse argument \"$1\"."
      exit 1
  esac
fi

# We moved this into the hyprmonitor.sh
#$HYPRCTL hyprpaper wallpaper ",~/.config/hypr/workspace-${toWorkspace}.png" > /dev/null
$HYPRCTL dispatch workspace $toWorkspace
