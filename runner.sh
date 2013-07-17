#!/usr/bin/env bash
#

if [[ $# -eq 0 ]]; then
  args="Test.main(Array())"
else
  args="Test.main(Array(\"$1\"))" && shift
fi

pscala -J-XX:MaxPermSize=1g "$@" -nc -i test/classpathTest.scala -e $args
