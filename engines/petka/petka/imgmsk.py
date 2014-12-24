# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import array, struct, io

from . import EngineError

class MSKLoader:
    def __init__(self):
        self.bound = [0, 0, 0, 0]
        self.rects = []
        
    def load_info(self, f):
        return self.load_data(f)
        
    def load_data(self, f):
        rest = f.read()
        f.seek(0)
        delta = len(rest) - 16
        rects = []
        while delta > 0:
            temp = f.read(4)
            delta -= 4
            rects_len = struct.unpack_from("<I", temp)[0]
            rec = []
            for r_ref in range(rects_len):
                temp = f.read(8)
                delta -= 8
                l,t,r,b = struct.unpack_from("<4h", temp)
                rec.append([l, t, r, b])
            rects.append(rec)
            delta -= 4
            
        if delta != 0:
           raise EngineError("Bad MSK file")
            
        temp = f.read(len(rects) * 4)
        frms = struct.unpack_from("<{}I".format(len(rects)), temp)

        if len(rects) != len(frms):
            raise EngineError("Bad MSK file structure")

        temp = f.read(16)
        self.bound = struct.unpack_from("<4i", temp)

        self.rects = list(zip(reversed(frms), rects))

