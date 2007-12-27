# DecomPy - Bytecode Decompiler for the ScummVM project
# Copyright (C) 2007 Andreas Scholta

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

"""Decompiler class and program entry point."""

import disasm
import scumm
#import bytecode
import cfg
from PseudoCode import PseudoCode


import array
from optparse import OptionParser

from altgraph import Dot

# class Decompiler:
#     """The Core"""

#     def __init__(self, args, frontends):
#         """Initialize Decompiler instance with file."""
#         self.disasm = None

#         self.opt_parser = OptionParser()
#         self.opt_parser.add_option("-d", "--disassemble",
#                                    action="store_true",
#                                    dest="disonly",
#                                    default=False)
#         for opt, frontend in frontends:
#             self.opt_paser.add(opt,
#                                action="store_const",
#                                const=frontend,
#                                dest="disasm")

#         (options, rest_args) = self.opt_parser.parse_args(args)
#         self.disasm = options.disasm.parse_options(rest_args)

#     def decompile(self):
#         """Decompile the script in self.file"""
#         decoded = disasm.decode()
#         if self.options.disonly:
#             decoded_tuples = decoded.items()
#             decoded_tuples.sort(key=lambda t: t[0])
#             for off, instr in decoded_tuples:
#                 print "[%.4X] %s;" % (off, instr)
#             print "END"


if __name__ == '__main__':
    # preliminary cmd line interface
    usage = "usage: %prog [options] <1|2|3|4|5> <file>"
    parser = OptionParser(usage=usage)
    parser.add_option("-d", "--disassemble", dest="disonly",
                      action="store_true", default=False,
                      help="disassemble only")
    parser.add_option("-g", "--graphviz", dest="dotty",
                      action="store_true", default=False,
                      help="launch dotty showing the control flow graph")
    parser.add_option("-i", "--indy", action="store_true",
                      dest="indy", default=False,
                      help="use Indy3-256 specific hacks")
    parser.add_option("-z", "--zak", action="store_true",
                      dest="zak", default=False,
                      help="use Zak256 specific hacks")
    parser.add_option("-u", "--unblocked", action="store_true",
                      dest="unblocked", default=False,
                      help="script has no header")
    parser.add_option("-x", "--halt-on-error", action="store_true",
                      dest="hoe", default=False,
                      help="halt on error")
    (options, args) = parser.parse_args()
    if len(args) != 2:
        parser.print_help()
        exit(2)

    if args[0] in ('1', '2', '3', '4', '5'):
        version = int(args[0])
    else:
        parser.print_help()
        exit(2)

    try:
        f = open(args[1], 'rb')
        data_arr = array.array('B', f.read())
        if version in (3,4,5):
            sc = scumm.SCUMM345(data_arr, version,
                                indy_flag = options.indy,
                                zak_flag = options.zak,
                                unblocked = options.unblocked,
                                halt_on_error = options.hoe)
        else:
            sc = scumm.SCUMM12(data_arr, version,
                                indy_flag = options.indy,
                                zak_flag = options.zak,
                                unblocked = options.unblocked,
                                halt_on_error = options.hoe)
        try:
            sc.parse_header()
            dis = disasm.Disasm(sc)
            decoded = dis.decode()
            if decoded:
                if not options.disonly:
                    blocks = cfg.gen_basic_blocks(decoded)
                    graph = cfg.gen_control_flow_graph(blocks, decoded)
                    cfg.set_immediate_dominators(graph)
                    intervals = cfg.gen_intervals(graph)
                    dseq = cfg.gen_derived_sequence(graph, intervals)
                    cfg.loop_struct(dseq)
                    cfg.comp_conds_struct(graph)
                    cfg.two_way_struct(graph)
                    if options.dotty:
                        Dot.Dot(graph).display()
#                         for nid in graph.node_list():
#                             block = graph.node_data(nid)
#                             print block
#                             for instr in block.instrs:
#                                 print decoded[instr]
                    pc = PseudoCode(graph, decoded)
                    pc.generate_code()
                else:
                    decoded_tuples = decoded.items()
                    decoded_tuples.sort(key=lambda t: t[0])
                    for off, instr in decoded_tuples:
                        print "[%.4X] %s;" % (off, instr)
        except:
            print "An error occured while decompiling %s" % args[1]
            raise
        print "END"
    except IOError:
        print "Error opening '%s'" % args[1]
        exit(-1)
