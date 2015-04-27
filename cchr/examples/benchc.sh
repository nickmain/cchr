#!/bin/bash

export TIMEFORMAT="%3U %3R"

V="$(echo "$2" | sed -e 'y/,/ /')"

(time C/$1 $V >/dev/null 2>&1) 2>&1 | while read C R; do
  CC=$(echo "a=1000*$C; scale=0; a/1" | bc -q)
  RR=$(echo "a=1000*$R; scale=0; a/1" | bc -q)
  echo "$CC $RR"
done

