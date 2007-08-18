"""Decompiler class and program entry point."""

import disasm
import scumm
import bytecode
import cfg

import array
from optparse import OptionParser

from altgraph import Dot

class Decompiler:
    """The Core"""
    pass

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-d", "--disassemble", dest="disonly",
                      action="store_true", default=False)
    parser.add_option("-g", "--graphviz", dest="dotty",
                      action="store_true", default=False)
    (options, args) = parser.parse_args()
    f = open(args[0], 'r')
    sc = scumm.SCUMM345(array.array('B', f.read()),
                        3,
                        indy_flag = True,
                        halt_on_error = False)
    try:
        sc.parse_header()
        dis = disasm.Disasm(sc)
        decoded = dis.decode()
        if decoded:
            if not options.disonly:
                blocks = cfg.gen_basic_blocks(decoded)
                graph = cfg.gen_control_flow_graph(blocks, decoded)
                if options.dotty:
                    Dot.Dot(graph).display()
                cfg.set_immediate_dominators(graph)
                intervals = cfg.gen_intervals(graph)
                dseq = cfg.gen_derived_sequence(graph, intervals)
#                 for dg, di in dseq:
#                     print dg
#                     for nid in dg.node_list():
#                         print dg.node_data(nid)
#                     for eid in dg.edge_list():
#                         print dg.edge_by_id(eid)
#                     print di
#                     print '-'*72
                cfg.loop_struct(dseq)
                cfg.two_way_struct(graph)
                cfg.comp_conds_struct(graph)
                for nid in graph.node_list():
                    block = graph.node_data(nid)
                    print block
                    for instr in block.instrs:
                        print decoded[instr]
            else:
                decoded_tuples = decoded.items()
                decoded_tuples.sort(key=lambda t: t[0])
                for off, instr in decoded_tuples:
                    print "[%.4X] %s;" % (off, instr)
    except:
        print "ERRRRRRRRRRRRRRRRRRRRRRRRRRRRRROOOOOOOOOOOOOOOOOOOOOORRRRRRRRRRRR"
        print args[0]
        raise
    print "END"
