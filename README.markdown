## Silly concurrent random number benchmark.

This is derived directly from my transactional red-black tree micro-benchmark, so
it might look a bit odd.  After cloning this repo I do the following to run the
benchmark:

~~~~
$ git clone -b pure https://github.com/cchalmers/pcg-random.git
$ cabal init sandbox
$ cabal install optparse-applicative MonadRandom mwc-random pcg-random/
$ cabal exec bash
$ ./build.sh
$ ./test.sh output.log
~~~~

Edit `test.sh` to match your machine's number of threads.

