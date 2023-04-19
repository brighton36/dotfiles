#!/bin/bash

ECHO="/bin/echo"
FIND="/usr/bin/find"
MD5SUM="/usr/bin/md5sum"
AWK="/usr/bin/awk"
SED="/bin/sed"


if [ ! -d "$1" ] || [ ! -d "$2" ]; then
  $ECHO "$0 src_dir dest_dir"
  exit
fi

# Right here's we'll remove any trailing '/' 's from the parameters:
strPathSrc=${1%%/}
strPathDest=${2%/}

$FIND "$strPathSrc" -type f -exec $MD5SUM \{\} \; | (
	while read strLine; do
		strLineMd5=$($ECHO $strLine | $AWK '{print $1}')
		strLineFile=$($ECHO $strLine | $SED -e 's/^[^/]*//')

		$ECHO "$strLineMd5  ${strPathDest}${strLineFile#$strPathSrc}" | $MD5SUM -c 
	done	
)
