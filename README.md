Concraft-pl
===========

This package provides a morphosyntactic tagger for the Polish language.
The tool combines the following components into a pipeline:
* A morphosyntactic segmentation and analysis tool [Maca][maca],
* A morphosyntactic disambiguation library [Concraft][concraft],
* A simple frequency-driven lemmatiser (TODO).

Installation
============

First you need to install the [Maca][maca] tool.  The other dependencies will be
installed automatically when you install the `concraft-pl` package.

Maca
----

A detailed [installation guide][maca-install] can be found on the
[Maca][maca] homepage.

Concraft-pl
-----------

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build Concraft-pl.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

To install Concraft-pl from the official [Hackage repository][hackage-repo]
just run:

    cabal install concraft-pl

If you want to update Concraft-pl to a newer version you should
update the package list first:

    cabal update 
    cabal install concraft-pl

To install the latest development version from github just run

    cabal install

from the `concraft-pl` toplevel directory.

Data format
===========

The current version of Concraft-pl works on a simple `plain` text format supported by
the [Corpus2][corpus2] tools.  You will have to install these tools when you install
[Maca][maca] anyway, so you can use them to convert the output results generated
by Concraft-pl to one of the other formats supported by [Corpus2][corpus2].

Training
========

If you have the training material with disambiguation annotations (stored in the
`plain` text format) you can train the Concraft-pl model yourself.

    concraft-pl train config/nkjp-tagset.cfg train.plain -e eval.plain -o model.bin

The first program argument is a specification of the [NKJP][nkjp] morphosyntactic
tagset.  It can be found in the `config` toplevel directory.
Run `concraft-pl train --help` to learn more about the program arguments and
possible training options.

Consider using [runtime system options][ghc-rts].  You can speed up processing
by making use of multiple cores by using the `-N` option.  The `-s` option will
produce the runtime statistics, such as the time spent in the garbage collector.
If the program is spending too much time collecting garbage, you can try to
increase the allocation area size with the `-A` option.  For example, to train
the model using four threads and 1GB allocation area size, run:

    concraft-pl train config/nkjp-tagset.cfg train.plain -e eval.plain -o model.bin +RTS -N4 -A1G -s

Disambiguation
==============

Once you have the model you can use the following command to segment,
analyse and tag the text file: 

    concraft-pl tag model.bin < input.txt > output.plain

Run `concraft tag --help` to learn more about the possible tagging options.

*Note: you should use the same version of [Maca][maca] for both training
and tagging.*

[concraft]: https://github.com/kawu/concraft "Concraft"
[hackage-repo]: http://hackage.haskell.org/package/concraft-pl "Concraft-pl Hackage repository"
[maca]: http://nlp.pwr.wroc.pl/redmine/projects/libpltagger/wiki "Maca"
[maca-install]: http://nlp.pwr.wroc.pl/redmine/projects/libpltagger/wiki#Download-and-install-MACA "Maca installation guide"
[corpus2]: http://nlp.pwr.wroc.pl/redmine/projects/corpus2/wiki "Corpus2"
[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[ghc-rts]: http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html "GHC runtime system options"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"
[nkjp]: http://nkjp.pl/index.php?page=0&lang=1 "NKJP"
