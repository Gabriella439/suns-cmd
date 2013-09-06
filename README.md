# suns-cmd v4.0.0

`suns-cmd` is a command line client to the Suns protein search engine.

## Quick Start

Install the [Haskell Platform](http://www.haskell.org/platform/).

    cabal update
    cabal install

To use `suns-cmd`, just create a directory to store the results:

    mkdir results

... and feed in the motif to search to the program's standard input:

    ~/.cabal/bin/suns-cmd -d results < myMotif.pdb

## Testing

If you want to run tests before installation, then use the following
installation sequence instead:

    cabal update                    # Updates Hackage metadata
    cabal configure --enable-tests  # Enable tests
    cabal build                     # Compile the executable and tests
    cabal test                      # Run the test suite
    cabal copy                      # Copy the executable to ~/.cabal/bin

This test requires a network connection because it sends the searches to the
official search engine hosted at `suns.degradolab.org`.  You can find the
test input structures in the `test` directory.

## License (BSD 3-clause)

Copyright (c) 2012, 2013 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Gabriel Gonzalez nor the names of other contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
