#!/bin/bash

for r in STDGEN LINE MWC PCG_HACKAGE PCG_PURE; do
   ghc -O2 -threaded -rtsopts Main.hs -o Main-$r -outputdir .build-$r -D$r
done
