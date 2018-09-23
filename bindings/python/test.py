#! /usr/bin/python
# *-* coding: utf-8 *-*

from morfeusz2 import Morfeusz
from concraft import Concraft

morf = Morfeusz(expand_tags=True)
conc = Concraft()

dag = morf.analyse(u'Jam się w ogóle nie bał.')
res = conc.disamb(dag)
print(res)

dag = morf.analyse(u'Tomek jam się w ogóle nie bał.')
dag_str = conc.stringify_dag(dag)
print(conc.disamb_str(dag_str))
