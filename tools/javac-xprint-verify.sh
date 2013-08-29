#!/usr/bin/env bash
#

dir=$(mktemp -d -t xprint)

for f in $(find . -name '*.class' | fgrep -v '$')
do
  path=${f:2}
  binaryName=$(echo ${path%.class} | tr '/' '.' | grep -v \.package$)
  echo "javac -Xprint $binaryName"
  javac -Xprint "$binaryName" | grep -v 'scala.reflect.ScalaSignature' &> "$dir/$binaryName.txt"
  if [[ $(grep "An exception has occurred in the compiler" "$dir/$binaryName.txt") ]]; then
    cat "$dir/$binaryName.txt"
  fi
done

# ; path="${$f:2}" ; echo $path ; done
