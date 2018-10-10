Python
======

This directory contains a Python client code, which allows to communicate with
the Concraft-pl's server.  The code relies on the [Morfeusz][morfeusz] Python
library, which should be installed beforehand.

**Warning**: the Python API is not stable yet, it can still undergo significant
changes in future versions.


Example
=======

To run the Concraft-pl server, you can use the command explained on the [main
github page][main]:

    concraft-pl server -i model.gz +RTS -N -A64M

Alternatively, you can launch the server directly from Python:

```python
from morfeusz2 import Morfeusz
from concraft_pl2 import Concraft, Server

server = Server(model_path="/path/to/concraft-pl/model.gz")
```

Next, create the Morfeusz and Concraft instances:

```python
morfeusz = Morfeusz(expand_tags=True)
concraft = Concraft()
```

The `expand_tags=True` option is required, Concraft-pl will not be able to
perform disambiguation otherwise.

Finally, you can use Morfeusz to perform morphosyntactic analysis, and Concraft
to disambiguated the resulting DAG:

```python
dag = morfeusz.analyse(u'W Szczebrzeszynie chrząszcz brzmi w trzcinie.')
dag_disamb = concraft.disamb(dag)
```

The Concraft class also provides a lower level method, `disamb_str`, and
conversion routines, `dag_to_str` and `str_to_dag`, which allow to work with
the main text format supported by Concraft:

```python
dag = morfeusz.analyse(u'W Szczebrzeszynie chrząszcz brzmi w trzcinie.')
dag_str = concraft.dag_to_str(dag)
dag_disamb_str = concraft.disamb_str(dag_str)
print(dag_disamb_str)
```

This should result in:
```
0	1	W	w	prep:acc:nwok			0.0000			
0	1	W	w	prep:loc:nwok			1.0000			disamb
1	2	Szczebrzeszynie	Szczebrzeszyn	subst:sg:loc:m3	nazwa geograficzna		1.0000			disamb
1	2	Szczebrzeszynie	Szczebrzeszyn	subst:sg:voc:m3	nazwa geograficzna		0.0000			
2	3	chrząszcz	chrząszcz	subst:sg:nom:m2	nazwa pospolita		1.0000			disamb
3	4	brzmi	brzmieć:v1	fin:sg:ter:imperf			1.0000			disamb
4	5	w	w	prep:acc:nwok			0.0000			
4	5	w	w	prep:loc:nwok			1.0000			disamb
5	6	trzcinie	trzcina	subst:sg:dat:f	nazwa pospolita		0.0000			
5	6	trzcinie	trzcina	subst:sg:loc:f	nazwa pospolita		1.0000			disamb
6	7	.	.	interp			1.0000		eos	disamb
```

If you run the Concraft-pl server directly from Python, don't forget to
terminate it once you are done with coding:

```python
server.terminate()
```

Or, even better, use `try ... catch`:
```python
try:
   server = Server(model_path="/path/to/concraft-pl/model.gz")
   ...
finally:
   server.terminate()
```

Acknowledgements
================

The code provided here was written by Witold Kieraś from the Institute of
Computer Sciense, Polish Academy of Sciences, and adapted by Jakub Waszczuk.


[morfeusz]: http://sgjp.pl/morfeusz/index.html "Morfeusz"
[main]: https://github.com/kawu/concraft-pl#server "Concraft server"
