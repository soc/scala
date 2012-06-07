#!/bin/sh
#

# set -e
root=$(pwd)
out=$(mktemp -dt pscala)
cd $out
pwd

for kind in "$@"; do
  dir=$root/test/files/$kind
  files=$(ls -1 $dir/*.scala | sort-random)

  for f in $files; do
    base=$(basename $f | sed 's/\.scala$//')
    check=$dir/$base.check
    # flags=$( [[ -f $dir/$base.flags ]] && cat $dir/$base.flags || echo "" )
    mkdir $base && ( \
      cd $base && \
        pscalac $flags $f 2>compile.txt 1>stdout.txt
        compiled=$?
        sed -i "" "s#${dir}/##" compile.txt
        # echo "compiled = $compiled"
        if [[ $kind == "neg" ]]; then
          if [[ $compiled == 0 ]]; then
            echo "[FAIL] $kind/$base (compiles)"
          elif [[ ! -f $check ]]; then
            echo "[FAIL] $kind/$base (no checkfile!)"
          else
            # echo "diff $(pwd)/compile.txt $check"
            if diff -q compile.txt $check >/dev/null; then
              echo "[ OK ] $kind/$base (diff ok)"
            else
              echo "[FAIL] $kind/$base (diff failed)"
              diff compile.txt $check
            fi
          fi
        elif [[ ! $compiled ]]; then
          echo "[FAIL] $kind/$base (compile)"
        elif [[ ! -f $check ]]; then
          if [[ $kind == "pos" ]]; then
            note=""
          else
            note="(no checkfile)"
          fi
          echo "[ OK ] $kind/$base $note"
        else
          pscala Test jvm > run.txt
          ran=$?
          if [[ ! $ran ]]; then
            echo "[FAIL] $kind/$base (run)"  
          elif diff -q run.txt $check; then
            echo "[ OK ] $kind/$base (diffed)"
          else
            echo "[FAIL] $kind/$base (diff)"
          fi
        fi
    )
  done
done
