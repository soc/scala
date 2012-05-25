#!/bin/sh
#

scalac="$SCALA_HOME/bin/scalac"

for f in $(cd files/pos && ls -1 *.scala); do
  base=${f%%.scala}
  flags=$( [[ -f files/pos/$base.flags ]] && cat files/pos/$base.flags )
  cmd="$scalac $flags -d /tmp files/pos/$f"
  echo "$cmd"
  $cmd
done
