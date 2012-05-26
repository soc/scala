#!/bin/sh
#

home=${SCALAC_HOME:-$(dirname $BASH_SOURCE)/../build/quick}
scalac="$home/bin/scalac"

for f in $(cd files/pos && ls -1 *.scala); do
  base=${f%%.scala}
  flags=$( [[ -f files/pos/$base.flags ]] && cat files/pos/$base.flags )
  cmd="$scalac $flags -d /tmp files/pos/$f"
  echo "$cmd"
  $cmd
done
