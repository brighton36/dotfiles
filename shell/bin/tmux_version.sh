#!/bin/bash

ECHO="echo"
SED="sed"
BC="/usr/bin/bc"
TMUX="/usr/bin/tmux"

if [ ! $# -eq 2 ]; then
  echo "Invalid Parameters. Two arguments are required."
  exit 2
fi

if [[ $1 == 'lt' ]]; then
  operator="<"
elif [[ $1 == 'lte' ]]; then
  operator="<="
elif [[ $1 == 'gt' ]]; then
  operator=">"
elif [[ $1 == 'gte' ]]; then
  operator=">="
elif [[ $1 == 'eq' ]]; then
  operator="=="
else
  echo "Parameter one invalid. Must be one of: lt, lte, gt, gte, eq."
  exit 2
fi

if [[ $2 =~ ^[0-9\.]+$ ]]; then
  version_right=$2
else
  echo "Parameter two invalid. Must be numeric."
  exit 2
fi

version_left=$($TMUX -V | $SED 's/[^0-9\.]*//g')

if [ $($ECHO "$version_left $operator $version_right" | $BC) = 1 ]; then
  exit 0
fi

exit 1
