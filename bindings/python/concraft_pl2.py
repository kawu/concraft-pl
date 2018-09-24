#! /usr/bin/python
# *-* coding: utf-8 *-*
#
# Authors:
# * Witek Kieraś
# * Kuba Waszczuk

import requests
import json

class Concraft(object):
    def __init__(self, server_addr='http://localhost:3000/parse'):
        self.server_addr = server_addr

    def dag_to_str(self, morf_dag):
        """
        Convert a DAG in the Morfeusz-compliant format to a DAG in the
        Concraft-compliant format.
        """
        conc_dag = ''
        for item in morf_dag:
            num1, num2, (forma, lemat, tag, posp, kwal) = item
            line_string = '\t'.join((str(num1), str(num2), forma, lemat, tag, ','.join(posp), ','.join(kwal), '0.0', '', '' + '\n'))
            conc_dag += line_string
        return conc_dag

    def str_to_dag(self, dag_str):
        """
        Reverse of `dag_to_str`.
        """
        analyse_list = []
        for line in dag_str.split('\n'):
            if line != '':
                num1, num2, forma, lemat, tag, posp, kwal, prob, interp, eos, disamb = line.strip('\n').split('\t')
                eos = 'eos' if eos else None
                disamb = 'disamb' if disamb else None
                posp = posp.split(',') if posp else []
                kwal = kwal.split(',') if kwal else []
                analyse_list.append((int(num1), int(num2), (forma, lemat, tag, posp, kwal), prob, eos, disamb))
            else:
                analyse_list.append("")
        return analyse_list

    def disamb_str(self, dag):
        """
        Disambiguate a DAG represented as a string in the Concraft-compliant
        format (tab separated string with one arc represented per line).
        """
        analyse_list = []
        # TODO: only add '\n' if necessary!
        request_data = {'dag':dag + '\n'}
        r = requests.post(self.server_addr, data=json.dumps(request_data))
        return r.json()['dag']
         
    def disamb(self, dag):
        """
        Disambiguate a DAG represented in the Morfeusz-compliant format.
        """
        dag_str = self.dag_to_str(dag)
        dag_result = self.disamb_str(dag_str)
        return self.str_to_dag(dag_result)
