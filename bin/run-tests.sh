#!/bin/sh
#

set -e
root=$(pwd)
out=$(mktemp -dt pscala)
cd $out
pwd

for kind in "$@"; do
  for f in $( ls -1 $root/test/files/$kind/*.scala | sort-random ) ; do
    dir=$root/test/files/$kind
    base=$(basename $f | sed 's/\.scala$//')
    check=$dir/$base.check
    flags=$( [[ -f $dir/$base.flags ]] && cat $dir/$base.flags || echo "" )
    mkdir $base && ( \
      cd $base && ( pscalac $flags $f &> compile.txt || echo "[FAIL] $kind/$base (compile)" ) && \
        if [[ -f $check ]]; then
          ( pscala Test jvm > run.txt && ( diff -q run.txt $check && echo "[ OK ] $kind/$base (checked)" || echo "[FAIL] $kind/$base" ) )
        else
          echo "[ OK ] $kind/$base" # " (no checkfile)"
        fi
    )
  done
done
