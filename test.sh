#!/bin/bash

set -e 

s=1000

echo "Benchmark running with log to $1"

rm -f $1 &> /dev/null

for exe in LINE MWC PCG_HACKAGE PCG_PURE PCG_PURE_NEXT SPLITMIX SPLITMIX_NEW TFRANDOM; do
    main=./Main-$exe
    for t in `seq 1 4` ; do
        cmd="$main -t $t -s $s +RTS -qa -N$t -K2m -A2m"
        echo $cmd
        $cmd 1>> $1
    done
done
