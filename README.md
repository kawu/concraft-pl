Concraft-pl 2.0
===============

This package provides a morphosyntactic tagger for the Polish language. The tool
is coupled with [Morfeusz][morfeusz], a morphosyntactic analyzer for Polish,
which represents both morphosyntactic and segmentation ambiguities in the form
of a directed acyclic graph (DAG).

This is the new, 2.0 version of Concraft-pl. The previous version, now obsolete,
can be found at https://github.com/kawu/concraft-pl/tree/maca.

As for now, the tagger doesn't provide any lemmatisation capabilities. As a
result, it may output multiple interpretations (all related to the same
morphosyntactic tag, but with different lemmas) for some known words, while for
the out-of-vocabulary words it just outputs orthographic forms as lemmas.

<!--
See the [homepage][homepage] if you wish to download a pre-trained
model for the Polish language.
-->


Installation
============

First you will need to download and install the [Haskell Tool Stack][stack].
Then use the following script:

    git clone https://github.com/kawu/concraft-pl.git
    cd concraft-pl
    stack install

Known installation issues
-------------------------

  * **Ubuntu**: if installation fails with the message that the `tinfo` library is missing, install the `libtinfo-dev` package (`sudo apt install libtinfo-dev`) and then run `stack install` again in the cloned repository.
    

Data format
==============

Concraft-pl works with tab-separated values (`.tsv`) files, with the individual
paragraphs separated by blank lines. Each non-blank line corresponds to an edge
in the paragraph DAG and contains the following 10 columns:

  * ID of the start node
  * ID of the end node
  * word form
  * base form (lemma)
  * morphosyntactic tag
  * commonness (common word, named entity)
  * qualifiers
  * probability of the edge
  * interpretation-related meta information
  * end-of-sentence (eos) marker
  * segment-related meta information

For the moment, the tool ignores (i.e. rewrites) the values of commonness,
qualifiers, and meta-information (both interpretation- and segment-related),
but we plan to exploit them in the future.

An example of a file following the above specification can be found in
`example/test.dag`.


Training
==========

The `train` command can be used to train the model based on a given `.dag` file.
The following example relies on the files available in the `example` directory.

    concraft-pl train train.dag -c config.dhall --tagsetpath=tagset.cfg -e test.dag -o model.gz
    
where:

  * `train.dag` is the training file, based on which the model parameters are estimated
  * `test.dag` is the evaluation file (optional; allows to track tagging quality during training)
  * `config.dhall` is the general configuration (e.g., disambiguation tiers)
  * `tagset.cfg` is the tagset configuration
  * `model.gz` is the output model (optional)

Run `concraft-pl train --help` to learn more about the program arguments and
possible training options.

Pre-trained models
------------------

A model pre-trained on the [National Corpus of Polish][nkjp] can be downloaded
from [here][ncp-pre-model]. The corresponding training material (including
configuration) is also [available for download][ncp-pre-train]. This model is
compatible with the current version of [Morfeusz SGJP][morfeusz] (i.e., the
version from September 1st 2018 or newer), which should be also used for
morphosyntactic analysis preceding tagging.

Runtime options
---------------

Consider using [runtime system options][ghc-rts].  You can speed up processing
by making use of multiple cores by using the `-N` option.  The `-s` option will
produce the runtime statistics, such as the time spent in the garbage collector.
If the program is spending too much time collecting garbage, you can try to
increase the allocation area size with the `-A` option.
<!--If you have a big dataset and it doesn't fit in the computer memory, use the
`-\-disk` flag.-->
For example, to train the model using four threads and 256M allocation area
size, run:


    concraft-pl train train.dag -c config.dhall --tagsetpath=tagset.cfg -e test.dag -o model.gz +RTS -N4 -A256M -s

<!--
Finally, you may consider pruning the resultant model in order to reduce its size.
Features with values close to 0 (in log-domain) have little effect on the modeled
probability and, therefore, it should be safe to discard them.

    concraft-pl prune -t 0.05 input-model.gz pruned-model.gz
-->

Probabilities
-------------

During the process of training, you may encounter a warning like this one:

```
===== Train sentence segmentation model =====
Discarded 49/18484 elements from the training dataset
```

This means that some of the graphs (paragraphs, sentences) in the training
dataset are either ill-formed (e.g. have cycles) or have incorrectly assigned
probabilities.  You can use the following command to identify such graphs:

    concraft-pl check -j tagset.cfg train.dag

The probabilities assigned to the individual interpretations in the DAG should
follow certain rules.  Let `in(v)` be the sum of the probabilities assigned to
the arcs incoming to `v` and `out(v)` be the sum of the probabilities assigned
to the arcs outgoing from `v`.  Let also assume that:

  * `in(s) = 1` for the source node `s` (with no incoming arcs)
  * `out(t) = 1` for the target node `t` (with no outgoing arcs)

Then, the following constraint must be satisfied for any node `v` in the DAG:

  * `in(v) = out(v)`

For instance, the following DAG (which contains four different paths, each with
probability 0.25) is structured properly:
```
0	1	co	co:s	subst			0.25			
0	1	co	co:c	comp			0.25			
0	2	coś	coś:s	subst			0.25			
0	2	coś	coś:q	part			0.25			
1	2	ś	być	aglt			0.5			
2	3	jadł	jeść	praet			1.0			
```


Tagging
=======

Once you have a Concraft-pl model you can use the following command to tag:

    concraft-pl tag model.gz -i input.dag -o output.dag

<!--
With the `-\-marginals` option enabled, Concraft-pl will output marginal probabilities
corresponding to individual tags (determined on the basis of the disambiguation model)
instead of `disamb` markers.
-->

Run `concraft-pl tag --help` to learn more about the possible tagging options.

Blacklist
---------

You can provide a list of blacklisted tags using the `-b` (`--blackfile`)
option.  Blacklisted tags are guaranteed not to be selected by the guesser.
The blacklisted tags provided on input (i.e., resulting from morphosyntactic
analysis) can still be selected by the disambiguation module, though.

The list of blacklisted tags should be provided in a separate file, one tag per
line.

Marginals and performance considerations
----------------------------------------

By default, Concraft-pl outputs the marginal probabilities of the individual
interpetations on top of the standard `disamb` markers.  Calculating marginals,
however, is more computationally intensive than determining those markers.

If you wish to speed up tagging and you don't care about the
disambiguation-related probabilities, you can use the `-p guess` option.  With
this option, Concraft-pl outputs the marginal probabilities originating from
the guessing model istead.


Server
======

Concraft-pl provides also a client/server mode.  It is handy when, for example,
you need to tag a large collection of small files.  Loading Concraft-pl model
from a disk takes considerable amount of time.

To start the Concraft-pl server on port `3000`, run:

    concraft-pl server --port=3000 -i model.gz

To use the server in a multi-threaded environment, you need to specify the `-N`
[RTS][ghc-rts] option. A set of options which yields good server performance is
presented in the following example:

    concraft-pl server --port=3000 -i model.gz +RTS -N -A64M
<!--
    # NOTE: adding the options `-qg1 -I0` may be good, but it only showed
    # improvements when using smaller allocation area size.
    concraft-pl server -\-port=3000 -i model.gz +RTS -N -A4M -qg1 -I0
-->

The `-Asize` option specifies the allocation area size of the garbage collector.
You can increase its value (e.g. `-A256M`), which may still improve the
performance, but at the cost of a higher memory consumption.

Run `concraft-pl server --help` to learn more about possible server-mode options.

Haskell Client
--------------

The client mode works just like the tagging mode. The difference is that,
instead of supplying the client with a model, you need to specify the server:

    concraft-pl client -s "http://localhost" --port=3000 -i input.dag -o output.dag
    
<!--
**NOTE**: the client has been designed so as to be run on short data files.
Ideally, the `input.dag` file should contain only one paragraph.
-->

**NOTE**: you can use `stdin` and `stdout` instead of the `-i` and `-o`
options, respectively.

Run `concraft-pl client --help` to learn more about possible client-mode options.

Python Client
-------------

A [Python client code][python-client] code is also provided. It allows to
communicate with the Concraft-pl's server directly from Python.  Clients in
other programming languages can be written in a similar manner.


<!--
Tagging analysed data
=====================

In some situations you might want to feed Concraft-pl with a previously
analysed data.  Perhaps your Maca instance is installed on a different
machine, or maybe you want to use Concraft-pl with a custom
preprocessing pipeline.

If you want to use a preprocessing pipeline significantly different from
the standard one (Maca), you should first train your own Concraft model.
To train the model on analysed data use the `-\-noana` training flag.

Use the same `-\-noana` flag when you want to tag analysed data.
Input format should be the same as the output format.
This option is currently not supported in the client/server mode.

*Remember to use the same preprocessing pipeline (segmentation + analysis) for both
training and disambiguation.  Inconsistencies between training material and input
data may severely harm the quality of disambiguation.*
-->


[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
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
[morfeusz]: http://sgjp.pl/morfeusz/index.html "Morfeusz"
[ncp-pre-model]: https://user.phil.hhu.de/~waszczuk/concraft/model-04-09-2018.gz "NCP model"
[ncp-pre-train]: https://user.phil.hhu.de/~waszczuk/concraft/train.zip "NCP training data"
[python-client]: https://github.com/kawu/concraft-pl/tree/master/bindings/python "Python client"
