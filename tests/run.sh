#!/bin/bash
# Run clxcb-truetype's tests, each in a fresh SBCL, against a scratch
# Xephyr server that this script starts and stops.
#
#   tests/run.sh                  all tests
#   tests/run.sh bench.lisp ...   the named ones
#
# XEPHYR_DISPLAY picks the display (default :11).  Any Xephyr already on
# that display is stopped first, so do not point it at one you use.
# The server has two screens, depths 24 and 16, for screens-and-threads.lisp.
# Needs Xephyr, SBCL and Quicklisp, with clxcb where ASDF can find it.

cd "$(dirname "$0")" || exit 1

display=${XEPHYR_DISPLAY:-:11}
socket=/tmp/.X11-unix/X${display#:}

[ $# -gt 0 ] && tests=("$@") || tests=(drawables.lisp font-cache.lisp glyphs-vs-masks.lisp screens-and-threads.lisp bench.lisp)

stop_server() {
  ps -eo pid,args | awk -v d="$display" '$2 ~ /Xephyr$/ && $3 == d {print $1}' | xargs -r kill
  for _ in $(seq 50); do [ -e "$socket" ] || break; sleep 0.1; done
}

stop_server
Xephyr "$display" -screen 1024x768 -screen 800x600x16 -br >/dev/null 2>&1 &
for _ in $(seq 50); do [ -e "$socket" ] && break; sleep 0.1; done
sleep 0.5

status=0
for t in "${tests[@]}"; do
  echo "== $t"
  out=$(DISPLAY=$display timeout 300 sbcl --noinform --non-interactive --load "$t" --eval '(uiop:quit 0)' 2>&1)
  echo "$out" | grep -v '^;\|STYLE-WARNING\|^$'
  echo "$out" | grep -q '^FAIL\|^Unhandled' && status=1
done

stop_server
exit $status
