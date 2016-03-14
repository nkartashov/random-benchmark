#!/bin/bash

for r in STDGEN LINE MWC PCG_HACKAGE PCG_PURE PCG_PURE_NEXT SPLITMIX SPLITMIX_NEW TFRANDOM; do
  stack ghc --package pcg-random --package MonadRandom --package splitmix -- -O2 -threaded -rtsopts Main.hs -o Main-$r -outputdir .build-$r -D$r
done
