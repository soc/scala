#!/usr/bin/env bash
#

okEq () {
  accept=$(cat <<EOM
EmptyTree
emptyValDef
null
undetBaseTypeSeq
[A-Z]\w+Class
[A-Z]\w+Type
No[A-Z]\w+
nme\.
tpnme\.
EOM
  )

  ( set -- $accept && IFS='|' && echo "$*" )
}

exclude="($(okEq))"
regexp="\s+(?:eq|ne)\s+(?!$exclude)"
ackopts="--noenv --nogroup --no-filename -A0 -B0 --scala"

ack $ackopts "$regexp" src/{compiler,reflect}  | perl -pe 's/^\s*//' | sort | uniq -c | gsort -gr
