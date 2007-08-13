"""Decompiler class and program entry point."""

import disasm
import scumm
import bytecode

import array
from optparse import OptionParser

class Decompiler:
    """The Core"""
    pass

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-f", "--file", dest="filename")
    (options, args) = parser.parse_args()
    f = open(options.filename, 'r')
    sc = scumm.SCUMM345(array.array('B', f.read()),
                        3,
                        indy_flag = True,
                        halt_on_error = False)
    sc.parse_header()
    try:
        while 1:
            print sc.decode_next()
    except bytecode.IncompleteFetchError:
        pass
    print "END"
