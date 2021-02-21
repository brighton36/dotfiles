#!/bin/bash
#
#

strPortsToScan="21 80 22 53 443 515 631 3389 5900 9100"

ECHO="/bin/echo"
NMAP="/usr/bin/nmap"
GREP="/bin/grep"
AWK="/usr/bin/awk"
NMBLOOKUP="/usr/bin/nmblookup"
TAIL="/usr/bin/tail"
COLUMN="/usr/bin/column"

if [ $# -eq 0 ]; then
  $ECHO "Usage: $0 iprange/subnet"

  exit;
fi

# This readies the -p paramter to nmap later on down
strNmapP=""
for strPort in $strPortsToScan
do
  if [ -n "$strNmapP" ]; then
    strNmapP="$strNmapP,$strPort"
  else
    strNmapP="$strPort"
  fi
done

$ECHO "Scanning Target Network : $1 ..."
$ECHO 

# Now we'll go ahead a perform a scan of the network:
$NMAP -sP -T Insane "$1" | $GREP 'Host' | $AWK '{print $2}' | {
  while read strIpAddr; do

    $ECHO -n "($strIpAddr:"

    # This grabs the netbios name, notice it'll take whatever is available from nbt records 00, 20, or 03.
    strNbtName=$($NMBLOOKUP -A "$strIpAddr" | $GREP '<ACTIVE>' | $GREP -v '<GROUP>' | $GREP -E  '00|20|03' | $AWK '{print $1}' | $TAIL --lines=1)

    # TODO: A Dns-check would be nice...

    $ECHO " $strNbtName)"

    # Now scan for open ports of interest:
    $NMAP -P0 -T Aggressive -sT "$strIpAddr" "-p$strNmapP" | $GREP "open" | {
      while read strNmapOut; do
        if [ $($ECHO "$strNmapOut" | $AWK '{print $2}' | $GREP 'open') ] ; then
          $ECHO -n "  "
          $ECHO -n "$strNmapOut" | $AWK '{print $1 "," $3}'
        fi
      done
      $ECHO "           ,  "
    } | $COLUMN -t -c 2 -s ','

  done 
}

