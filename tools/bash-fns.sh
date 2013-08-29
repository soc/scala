# bash-fns.sh, by paulp
#
# Start your bash script like this.
#
#    #!/usr/bin/env bash
#    #
#    . $(dirname $0)/bash-fns.sh  # pulls in this file
#
# _init will be run automatically.
# TODO: flesh out the quoting, more collections-like methods.
#

_init () {
  # exit on error
  set -e
  # changing directory will alter relative paths, so note script dir up front
  script_dir=$(cd $(dirname $0) ; pwd)
}

chr () {
  printf \\$(($1/64*100+$1%64/8*10+$1%8))
}

# random alpha string
random-string () {
  local i=${1:-16}
  local result=""

  while [[ $i -gt 0 ]]; do
    num=$( expr 97 + $(expr $RANDOM % 26) )
    printf -v result "$result$(chr $num)"
    i=$(($i-1))
  done

  echo "$result"
}

bind-anonfun () {
  name=$(random-string 10)-anonfun
  local argstoken='$@'

  # echo "$name() { local args=$argstoken ; $@; }"
  eval "$name() { local args=$argstoken ; $@; }"

  echo $name
}

bind-function () {
  fxn=$1
  shift
  eval "$fxn() { $@; }"
}

# prints the file extension of the given arguments,
# or nothing if there isn't one.
file-extension () {
  for path in "$@"; do
    local base=$(basename "$path")
    # make sure it has a dot, else the expression would return the basename
    [[ $base == *.* ]] && ( echo ${base##*.} )
  done
}

# filters stdin for paths having any of the given args as an extension.
filter-by-extension () {
  extensions="$@"
  while read line; do
    local fileExt=$(file-extension "$line")
    for ext in $extensions; do
      [[ "$ext" == "$fileExt" ]] && echo "$line"
    done
  done
}
# filter-by-extension () {
#   extensions="$@"
#
#   run-filter () {
#

# finds the absolute path for the given argument
abspath () {
  for path in "$@"; do
    if [[ -d "$path" ]]; then
      ( cd "$path" && pwd )
    elif [[ -f "$path" ]]; then
      dir=$(abspath $(dirname "$path"))
      echo "$dir/$(basename "$path")"
    fi
  done
}

expand-dirs () {
  for entry in "$@"; do
    for path in $(ls -1 $entry) ; do
      if [[ -f "$path" ]]; then
        ( abspath "$path" )
      elif [[ -d "$path" ]]; then
        ( cd "$path" && abspath $(ls -1) )
      fi
    done
  done
}

# creates a classpath from the given arguments
mkClasspath () {
  # todo - figure out $sep on windows is not :
  local sep=":"

  for path in $(expand-dirs "$@"); do
    if [[ -d "$path" ]]; then
      echo "$path"
    else
      fileExt=$(file-extension "$path")
      for ext in jar zip; do
        [[ "$ext" == "$fileExt" ]] && echo "$path"
      done
    fi
  done | mkString "$sep"
}

# iterates over stdin, running the given command on each line.
map-stdin () {
  local cmd="$@"
  if [[ $# -eq 0 ]]; then
    cmd="echo"
  fi

  while read line
  do
    $cmd "$line"
  done
}

# not much different from map-stdin in bash land.
foreach-stdin () {
  map-stdin "$@"
}

# iterates over stdin, echoing only those lines for which the filter is true.
filter-stdin () {
  local cond="$@"

  while read line; do
    if [[ $# -eq 0 ]] || $cond "$line"; then
      echo "$line"
    fi
  done
}

foreach-ls () {
  local cmd="$@"

  for entry in $(ls -1) ; do
    $cmd $entry
  done
}

# first argument is directory to run command in (or file, parent will be used)
# remainder of arguments are command to run.
run-in-dir () {
  set -- "$@"
  local where=$1
  shift 1

  # Uses subshell to avoid changing script directory
  if [[ -d "$where" ]]; then
    ( cd "$where" && $* )
  elif [[ -f "$where" ]]; then
    local dir=$(dirname "$where")
    ( cd "$dir" && $* )
  fi
}

# runs the command from each immediate subdirectory of the current directory.
run-in-subdirs () {
  local cmd="$@"

  for dir in $(ls -1) ; do
    [[ -d "$dir" ]] && run-in-dir "$dir" $cmd
  done
}

# don't call init if this is being run from a shell rather
# than included in another script.
if [[ "$0" != "-bash" ]]; then
  _init
fi
