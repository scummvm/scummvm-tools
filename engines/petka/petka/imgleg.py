# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import array, struct, io

from . import EngineError

class LEGLoader:
    def __init__(self):
        self.coords = []

    def load_info(self, f):
        return self.load_data(f)

    def load_data(self, f):
        hdr = f.read(4)
        if hdr != b"xyof":
            raise EngineError("Bad LEG/OFF magic \"{}\"".format(hdr))

        rest = f.read()
        if len(rest) % 8:
            raise EngineError("Bad LEG/OFF size {}".format(len(rest)))

        sf = struct.unpack("<{}l".format(len(rest) // 4), rest)
        self.coords = [[sf[i * 2], sf[i * 2 + 1]] for i in range(len(rest) // 8)]
