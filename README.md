# CIS 194

This is my set of solutions for the homeworks proposed on Upenn's [CIS 194][ref]
Spring 2013 class. This code follows the guidelines set by Johan Tibell's
[haskell-style-guide][stl].

[ref]: http://www.seas.upenn.edu/~cis194/spring13/
[stl]: https://github.com/tibbe/haskell-style-guide

## Installation and usage

The project is cabalized and contains a set of tests written with HSpec. The
following commands will download the dependencies, build and run the test specs
(assuming GHC and Cabal are installed):

    cabal install --only-dependencies --enable-tests
    cabal build
    cabal test

`cabal test`'s output is not colorized though. To see HSpec's output with colors
try running the binary instead by calling `./dist/build/spec/spec`.

## License

This code is under the public domain. See [LICENSE](LICENSE) for more details.

