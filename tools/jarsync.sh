#!/bin/sh
#
# Update the jars in build/pack/lib to match the files
# found in build/quick/classes.

echoerr () {
  echo >&2 "$@"
}

run () {
  echoerr "% $@"
  "$@"
}

compiler_touch=$(cat <<EOM
src/compiler/scala/tools/nsc/Global.scala
src/compiler/scala/tools/nsc/interpreter/ReplGlobal.scala
src/compiler/scala/tools/nsc/transform/Erasure.scala
EOM
)

reflect_touch=$(cat <<EOM
src/reflect/scala/reflect/internal/transform/Erasure.scala
src/reflect/scala/reflect/internal/SymbolTable.scala
EOM
)

# src/compiler/scala/tools/nsc/transform/Erasure.scala

base=$(cd $(dirname $0)/.. && pwd)

AUXCP="$base/build/asm/classes:$base/lib/msil.jar:$base/lib/forkjoin.jar:$base/lib/jline.jar:$base/lib/fjbg.jar"
export ANT_OPTS="-Xmx3g -Xms3g -XX:+TieredCompilation -XX:ReservedCodeCacheSize=256m -XX:MaxPermSize=384m -XX:+UseNUMA -XX:+UseParallelGC"
ZIP_OPTS="--display-counts --filesync"
ZINC_OPTS="-nailed -java-only -scala-home /scala/inst/3 -classpath $(cpof $base/build/locker/classes):$AUXCP"

linkage_errors () {
  $base/build/quick/bin/scala -nc -e '5' 2>&1 | \
    egrep 'java\.lang\.(AbstractMethodError|NoSuchMethodError|NoClassDefFoundError)' | \
    perl -pe 's#^.*?java\.lang\.\w+.*[./](\S+)\s*$#$1#;'
}

sync () {
  local dir=build/quick/classes/$1
  local jar=$(pwd)/build/pack/lib/scala-$1.jar

  [[ -f $jar ]] && ( cd $dir && /usr/bin/zip $ZIP_OPTS -r $jar * ) # | grep -v "^Archive is current$" )
}
sync_all () {
  for w in compiler library reflect partest; do
    sync $w
  done
}

cd $base
[[ $# -gt 0 ]] && echoerr "Detected change in $@, rebuilding... "
# ant -q quick.done >/dev/null 2>&1
# if [[ $? ]]; then
#   echo "ok."
# else
#   echo "failed. Error code was $?."
#   exit $?
# fi

for arg in "$@"; do
  where=$(echo "$arg" | perl -pe 's/.*?(compiler|library|reflect|partest).*/$1/')
  run zinc $ZINC_OPTS \
    -d $base/build/quick/classes/$where \
    "$arg"

  # [[ $? ]] || {
  #   echoerr "Aborting."
  #   exit 1
  # }
done

# smoke test
fails=$(linkage_errors)
[[ -z $fails ]] || {
  echoerr "Linkage errors: $fails"
  # regexp="($(mkString '|' $fails))"
  # files=$(ack --files-with-matches "$regexp" $base/src)
  # run touch $touchers
  # run ant quick.done
  run zinc $ZINC_OPTS -d $base/build/quick/classes/reflect $reflect_touch
  run zinc $ZINC_OPTS -d $base/build/quick/classes/compiler $compiler_touch
  sync_all
  exit 0
  # for w in library reflect compiler partest; do
  #   echoerr "Rebuilding $w ..."
  #   zinc $ZINC_OPTS -d $base/build/quick/classes/$w $(find $base/src/$w -name '*.scala' -o -name '*.java')
  # done
}

[[ -f build/pack/lib/scala-asm.jar ]] || {
  ( cd build/asm/classes && jar cf scala-asm.jar scala && mv scala-asm.jar ../../../build/pack/lib )
}

sync_all
