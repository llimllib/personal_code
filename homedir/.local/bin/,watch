#!/usr/bin/env bash
#
# SPDX short identifier: MIT
# Copyright 2022 Paul Smith <paulsmith@gmail.com>
# 
# reloader.sh - automatically trigger a reload of a build/server process when a
# file change is detected.
#
# Theory of operation
# -------------------
#
# The main design decision of reloader.sh is to not be a long-running process
# in and of itself between reloads of the build and/or server process. Unlike
# other utilities that provide reloading which sit in a loop, waiting for file
# changes and restarting processes, reloader.sh waits for the first filesystem
# change event only. It then arranges for the the process or processes to be
# reloaded, and immediately re-execs itself. In this way, no long-running state
# needs to be managed, which can be finicky, or be subtlely modified in ways
# that can be hard to debug. A clean new process runs each time.
#
# Dependencies
# ------------
#
# reloader.sh does have one 3rd-party depedency other than bash 4.x, which is
# [fswatch][fswatch]. fswatch is a filesytem event notification utility that
# abstracts the differences between various operating system event mechanisms.
#
# [fswatch]: https://emcrisostomo.github.io/fswatch/
#
# Design goals
# ------------
#
#   * Simple operation
#
#   * Correctness - the tool should do the right thing with regard to shutting
#     down cleanly (i.e., stop all descendent processes that are created by the
#     main commands), and handle signals correctly (i.e., use Ctrl-C/SIGINT to
#     stop the reloader and propagate that signal correctly to the other
#     processes and controlling shell; similar correct SIGTERM behavior).

set -euo pipefail

progname=reloader.sh

# check for fswatch dependency
if ! command -v fswatch >/dev/null 2>&1; then
	echo "fswatch is required for $progname"
	exit 1
fi

# save copy of original cli args array for the later exec invocation 
origargs=("$@")

usage() {
	echo "usage: $progname [-d dir] [-e exclude] firstcmd [nextcmd]" 1>&2
	exit 1
}

if [ -t 1 ] && [ -n "$(tput colors)" ]; then
	blue="\x1b[1;34m"
	yellow="\x1b[1;33m"
	green="\x1b[1;32m"
	reset="\x1b[0m"
else
	blue=""
	yellow=""
	green=""
	reset=""
fi

# configurable options, mainly to pass options to fswatch
directories=()
excludes=()

# parse cli flags
while getopts "d:e:h" arg; do
	case $arg in
		d) directories+=("$OPTARG")
			;;
		e) excludes+=("$OPTARG")
			;;
		*) usage
			;;
	esac
done
shift $((OPTIND-1))

# usage
[ $# -lt 1 ] && usage

# finish cli arg processing
buildcmd=
runcmd="$1"
shift
if [ $# -ge 1 ]; then
	# the first arg is actually a build step
	buildcmd="$runcmd"
	runcmd="$1"
	shift
fi
if [ $# -ge 1 ]; then
	echo -e "${yellow}WARNING: extra unprocessed arguments passed in: $*${reset}" 1>&2
fi

# set defaults
[ ${#directories[@]} -eq 0 ] && directories+=(".")

# the pid of the main long-running process in the background, which will have
# its own process group (by turning on job control (set -m) just before
# invoking the command). the pid will be used to kill the process group when
# either triggered to restart or on overall script shutdown.
bgpid=

killprocgroup() {
	kill -- -"$bgpid" 2>/dev/null || true # signal the process group
	wait
	bgpid=
}

# execute the build and/or run server commands
[ -n "$buildcmd" ] && $buildcmd
set -m # turn on job control to get a new process group ID
trap '[ -n "$bgpid" ] && killprocgroup "$bgpid"' EXIT
trap 'echo -e "${blue}░▒ Caught interrupt, shutting down${reset}"; trap - INT; kill -s INT -"$bgpid"; wait' INT
$runcmd &
bgpid=$!
set +m # turn off job control

# main reloading logic - on any detected file change, kill the server process
# and any children of its process group, and re-exec this script with the same
# arguments
events=("Updated" "Created" "Removed")
exclude_opt=
if [ ${#excludes[@]} -gt 0 ]; then
	exclude_opt=$(printf -- "--exclude %s " "${excludes[@]}")
fi
# shellcheck disable=SC2046,SC2086
fswatch -1 -r -l 0.5 $(printf -- "--event=%s " "${events[@]}") $exclude_opt $(printf -- "%s " "${directories[@]}") & # >/dev/null &
trap 'trap - TERM; kill "$fwpid"; wait "$fwpid"' TERM
fwpid=$!
wait "$fwpid"
echo -e "${green}░▒ File change detected, reloading${reset}" >&2
killprocgroup "$bgpid"
exec "$0" "${origargs[@]}"
