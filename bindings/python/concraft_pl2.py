#! /usr/bin/python
# *-* coding: utf-8 *-*
#
# Authors:
# * Witek Kieraś
# * Kuba Waszczuk


import requests
import json
import time
from subprocess import Popen, PIPE


class Concraft(object):
    def __init__(self, server_addr='http://localhost', port=3000):
        """
        Parameters
        ----------
        server_addr : url
            Address of the Concraft-pl server
        port : Server
            Port number used by the Concraft-pl server
        """
        self.server_addr = server_addr + ":{}/parse".format(port)

    def dag_to_str(self, morf_dag):
        """
        Convert a DAG in the Morfeusz-compliant format to a DAG in the
        Concraft-compliant format.
        """
        conc_dag = ''
        for item in morf_dag:
            num1, num2, (forma, lemat, tag, posp, kwal) = item
            line_string = '\t'.join((str(num1), str(num2), forma, lemat, tag, ','.join(posp), ','.join(kwal), '0.0', '', '', '' + '\n'))
            conc_dag += line_string
        return conc_dag

    def str_to_dag(self, dag_str):
        """
        Reverse of `dag_to_str`.
        """
        analyse_list = []
        for line in dag_str.split('\n'):
            if line != '':
                num1, num2, forma, lemat, tag, posp, kwal, prob, interp_meta, eos, seg_meta, disamb = line.strip('\n').split('\t')
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


class Server(object):
    def __init__(self, model_path, concraft_path="concraft-pl", port=3000,
            core_num=1, allocation_size=64):
        """
        Start a Concraft-pl server instance in the background.

        Parameters
        ----------
        model_path : path
            Path to a Concraft-pl model
        concraft_path : path
            Path to a Concraft-pl executable
        port : int
            Port number to be used to run a Concraft-pl server instance
        core_num : int
            Number of processor cores to use
        allocation_size : int
            Allocation area size (in MBs) of the garbage collector
        """
        self.port = port
        self.concraft_server = Popen([concraft_path, 'server',
            '--port={}'.format(port), '-i', model_path, '+RTS',
            '-N{}'.format(core_num), '-A{}M'.format(allocation_size),],
            stdin=PIPE, stdout=PIPE, stderr=PIPE)
        # print(u"Concraft model " + model_path + u" loading...")
        loaded = False
        while not loaded:
            try:
                request_data = {'dag':''}
                r = requests.post('http://localhost:{}/parse'.format(port),
                        data=json.dumps(request_data))
                loaded = True
                #print(u"loaded!")
            except requests.ConnectionError as e:
                #print(u"loading�~@�")
                time.sleep(1)

    def terminate(self):
        """Terminate the Concraft-pl server."""
        self.concraft_server.terminate()
