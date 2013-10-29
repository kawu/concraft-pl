Concraft-pl
===========

This package provides a morphosyntactic tagger for the Polish language.
The tool combines the following components into a pipeline:
* A morphosyntactic segmentation and analysis tool [Maca][maca],
* A morphosyntactic disambiguation library [Concraft][concraft],
* A simple, frequency-driven lemmatiser (TODO).  Until the lemmatiser component
  is implemented, the tagger may output multiple interpretations (all related
  to the same morphosyntactic tag, but with different lemmas) in some cases.

See the [homepage][homepage] if you wish to download a pre-trained
model for the Polish language.


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build Concraft-pl.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].  

Unless you plan to use a custom preprocessing pipeline or run [Maca][maca] on a
different machine (see section [Tagging analysed data](#tagging-analysed-data)),
you will also need the [Maca][maca] tool. 
A detailed [installation guide][maca-install] can be found on the [Maca][maca]
homepage.

To install Concraft-pl from the official [Hackage repository][hackage-repo]
just run:

    cabal install concraft-pl

If you want to upgrade Concraft-pl to a newer version you should
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
[Maca][maca] anyway, so you can use them to convert the output generated
by Concraft-pl to one of other formats supported by [Corpus2][corpus2].


Training
========

If you have the training material with disambiguation annotations (stored in the
`plain` text format) you can train the Concraft-pl model yourself.

    concraft-pl train config/nkjp-tagset.cfg train.plain -e eval.plain -o model.gz

The first program argument is a specification of the [NKJP][nkjp] morphosyntactic
tagset.  It can be found in the `config` toplevel directory.

Consider using [runtime system options][ghc-rts].  You can speed up processing
by making use of multiple cores by using the `-N` option.  The `-s` option will
produce the runtime statistics, such as the time spent in the garbage collector.
If the program is spending too much time collecting garbage, you can try to
increase the allocation area size with the `-A` option.  If you have a big
dataset and it doesn't fit in the computer memory, use the `--disk` flag.
For example, to train the model using four threads and 256M allocation area
size, run:

    concraft-pl train config/nkjp-tagset.cfg train.plain -e eval.plain -o model.gz +RTS -N4 -A256M -s

Keep in mind, that Concraft-pl will reanalyse the input data before training.
You can suppress this behaviour by using the `--noana` option.

Run `concraft-pl train --help` to learn more about the program arguments and
possible training options.


Tagging
=======

Once you have a Concraft-pl model you can use the following command tag `input.txt` file:

    concraft-pl tag model.gz < input.txt > output.plain

The input file is first divided into paragraphs (the tool interprets empty lines
as paragraph ending markers).  After that, [Maca][maca] is used to segment and analyse
each paragraph.  Finally, [Concraft][concraft] module is used to disambiguate each
sentence in the [Maca][maca] output.

Run `concraft-pl tag --help` to learn more about possible tagging options.


Server
======

Concraft-pl provides also a client/server mode.  It is handy when, for example,
you need to tag a large collection of small files.  Loading Concraft-pl model
from a disk takes considerable amount of time which makes the tagging method
described above very slow in such a setting.

To start the Concraft-pl server, run:

    concraft-pl server model.gz

You can supply a custom port number using a `--port` option.  For example,
to run the server on the `10101` port, use the following command:

    concraft-pl server model.gz --port 10101

To use the server in a multi-threaded environment, you need to specify the
`-N` [RTS][ghc-rts] option.  A set of options which usually yields good
server performance is presented in the following example:

    concraft-pl server model.gz +RTS -N -A4M -qg1 -I0

Run `concraft-pl server --help` to learn more about possible server-mode options.

The client mode works just like the tagging mode.  The only difference is that,
instead of supplying your client with a model, you need to specify the port number
(in case you used a custom one when starting the server; otherwise, the default
port number will be used).

    concraft-pl client --port 10101 < input.txt > output.plain

Run `concraft-pl client --help` to learn more about possible client-mode options.


Tagging analysed data
=====================

In some situations you might want to feed Concraft-pl with a previously
analysed data.  Perhaps your Maca instance is installed on a different
machine, or maybe you want to use Concraft-pl with a custom
preprocessing pipeline.

If you want to use a preprocessing pipeline significantly different from
the standard one (Maca), you should first train your own Concraft model.
To train the model on analysed data use the `--noana` training flag.

Use the same `--noana` flag when you want to tag analysed data.
Input format should be the same as the output format.
This option is currently not supported in the client/server mode.

*Remember to use the same preprocessing pipeline (segmentation + analysis) for both
training and disambiguation.  Inconsistencies between training material and input
data may severely harm the quality of disambiguation.*


[homepage]: http://zil.ipipan.waw.pl/Concraft "Homepage"
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
