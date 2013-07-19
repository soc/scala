#!/usr/bin/env bash
#

run () { echo >&2 "$@" && "$@"; }

scalaArgs="-J-XX:MaxPermSize=1g -nc"
[[ $1 == "-d" ]] && scalaArgs="$scalaArgs -Dcp.debug" && shift
[[ $1 == "-p" ]] && scalaArgs="$scalaArgs -Dcp.parallel" && shift

run ./build/pack/bin/scala $scalaArgs test/classpathTest.scala "$@"
