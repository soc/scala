#!/usr/bin/env bash
#
##############################################################################
# Copyright 2002-2011, LAMP/EPFL
#
# This is free software; see the distribution for copying conditions.
# There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.
##############################################################################

default_java_opts="-Xmx1g -Xms1g"

# break out -D and -J options and add them to JAVA_OPTS as well
# so they reach the underlying JVM in time to do some good.  The
# -D options will be available as system properties.
declare -a java_args scala_args residual_args
unset verbose debug quiet bootcp toolcp colors saved_stty

die() {
  echo "Aborting: $@"
  exit 1
}
echoArgs () {
  echo ""
  for arg; do
    echo "$arg"
  done
  echo ""
}
execCommand () {
  [[ -n $debug ]] && echoArgs "$@"
  "$@"
}
dlog () {
  [[ -n $debug ]] && echo "$@"
}

bootcp=true
# save terminal settings - clear on error so we don't later try to restore them
saved_stty=$(stty -g 2>/dev/null) && dlog "saved stty: $saved_stty" || unset saved_stty

addJava () {
  dlog "[addJava] arg = '$1'"
  java_args=( "${java_args[@]}" "$1" )
}
addScala () {
  dlog "[addScala] arg = '$1'"
  scala_args=( "${scala_args[@]}" "$1" )
}
addResidual () {
  dlog "[residual] arg = '$1'"
  residual_args=( "${residual_args[@]}" "$1" )
}

findScalaHome () {
  # see SI-2092 and SI-5792
  local source="${BASH_SOURCE[0]}"
  while [ -h "$source" ] ; do
    local linked="$(readlink "$source")"
    local dir="$( cd -P $(dirname "$source") && cd -P $(dirname "$linked") && pwd )"
    source="$dir/$(basename "$linked")"
  done
  ( cd -P "$(dirname "$source")/.." && pwd )
}

# Not sure what the right default is here: trying nonzero.
scala_exit_status=127

# restore stty settings (echo in particular)
restoreSttySettings() {
  dlog "restoring stty: $saved_stty"
  stty $saved_stty
  unset saved_stty
}

# mkString : foo bar baz  ==> foo:bar:baz
mkString ()
{
  local sep="$1"
  local str=""
  shift

  for arg; do
    [[ -n "$str" ]] && str="$str$sep"
    str="$str$arg"
  done

  echo "$str"
}

onExit() {
  [[ -n $saved_stty ]] && restoreSttySettings
  exit $scala_exit_status
}

# to reenable echo if we are interrupted before completing.
trap onExit INT

# Finding the root folder for this Scala distribution
scala_home="$(findScalaHome)"

# Constructing the java classpath
toolClasspath () {
  if [[ -n "$toolcp" ]]; then
    echo "$toolcp"
  else
    mkString : $scala_home/lib/*
  fi
}

# If using the boot classpath, also pass an empty classpath
# to java to suppress "." from materializing.
classpathArgs () {
  if [[ -n $bootcp ]]; then
    echo "-Xbootclasspath/a:$(toolClasspath) -classpath \"\""
  else
    echo "-classpath $(toolClasspath)"
  fi
}

# e.g. path -java-home /path/to/java_home
require_arg () {
  local type="$1"
  local opt="$2"
  local arg="$3"

  if [[ -z "$arg" ]] || [[ "${arg:0:1}" == "-" ]]; then
    die "$opt requires <$type> argument"
  fi
}

while [[ $# -gt 0 ]]; do
  case "$1" in
           --) shift; for arg; do addResidual "$arg"; done; set -- ;;
     -h|-help) usage; exit 1 ;;
  -v|-verbose) verbose=true && shift ;;
    -d|-debug) debug=true && shift ;;
    -q|-quiet) quiet=true && shift ;;

        -repl) scala_main_class=scala.Repl && shift ;;
     -compile) scala_main_class=scala.tools.nsc.Main && shift ;;
         -run) scala_main_class=scala.tools.nsc.MainGenericRunner && shift ;;
      -bootcp) bootcp=true && shift ;;
   -no-bootcp) unset bootcp && shift ;;
      -colors) colors=true && shift ;;
   -no-colors) unset colors && shift ;;
      -jrebel) jrebel=true && shift ;;
   -no-jrebel) unset jrebel && shift ;;

      -toolcp) require_arg classpath "$1" "$2" && toolcp="$2" && shift 2 ;;
   -java-home) require_arg path "$1" "$2" && java_cmd="$2/bin/java" && shift 2 ;;

          -D*) addJava "$1" && addScala "$1" && shift ;;
          -J*) addJava "${1:2}" && addScala "$1" && shift ;;
            *) addResidual "$1" && shift ;;
  esac
done

[[ -z $java_cmd ]] && prefix=${java_home:+$java_home/bin/} && java_cmd="${prefix}java"

# note that variables which may intentionally be empty must not
# be quoted: otherwise an empty string will appear as a command line
# argument, and java will think that is the program to run.
execCommand \
  "$java_cmd" \
  ${JAVA_OPTS:-$default_java_opts} \
  "${java_args[@]}" \
  $(classpathArgs) \
  -Dscala.home="$scala_home" \
  -Dscala.usejavacp=true \
  "${scala_main_class:-scala.Repl}" \
  "${scala_args[@]}" \
  "${residual_args[@]}"

# record the exit status lest it be overwritten:
# then reenable echo and propagate the code.
scala_exit_status=$?
onExit
