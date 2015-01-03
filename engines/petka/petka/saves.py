# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import array, struct, io
import binascii

from . import EngineError, BMPLoader


class SaveLoader:
    def __init__(self, enc = None):
        self.part = None
        self.chap = None
        self.stamp = None
        self.enc = enc
        self.objects = []
       
    def load_data(self, f, part, objnum):
        data = f.read(8)
        self.part, self.chap = struct.unpack("<2I", data)
        if self.part != part: return

        # read date stamp, asciiz
        stamp = f.read(30)
        stamp = stamp.split(b"\x00")[0]
        self.stamp = stamp.decode(self.enc)
        
        # read screenshot
        sl = 108 * 81 * 2
        rgb = f.read(sl)
        if len(rgb) != sl:
            raise EngineError("Bad SAVE length (no screenshot)")
        self.shot = BMPLoader()
        self.shot.load_raw(108, 81, rgb)
               
        hz2 = f.read(216)
        if hz2 != b"\x00" * 216:
            raise EngineError("Bad SAVE error in HZ2 field")
            
        data = f.read(4)
        hz3 = struct.unpack("<I", data)[0]

        if hz3 != objnum + 3:
            raise EngineError("Bad SAVE objects number")
        
        def readstr():
            data = f.read(4)
            strlen = struct.unpack("<I", data)[0]
            s = f.read(strlen)
            #print("STR", strlen, data, s)
            return s.decode(self.enc)
        
        for i in range(objnum):
            s1 = readstr()
            s2 = readstr()
            data = f.read(33)
            obj = {"name": s1, "alias": s2, "data": data}
            recs = struct.unpack("<B8i", data)
            obj["recs"] = recs
            obj["res"] = recs[2]
            self.objects.append(obj)
            #print(i, s1, s2)
            
        # invntr        
        data = f.read(4)
        invlen = struct.unpack("<I", data)[0]
        data = f.read(invlen * 2)
        self.invntr = struct.unpack("<{}H".format(invlen), data)

        # scene
        self.scene = readstr()
        
        # char positions
        data = f.read(16)
        charpos = struct.unpack("<4I", data)
        
        # arr dialog opcodes
        data = f.read(4)
        dlgoplen = struct.unpack("<I", data)[0]
        self.dlgops = []
        for _ in range(dlgoplen):
            data = f.read(4)
            ref, arg, code = struct.unpack_from("<HBB", data)
            self.dlgops.append([code, arg, ref])
        
        data = f.read(20)
        self.cursor_res, self.cursor, self.cursor_obj, c1res, c2res = \
            struct.unpack("<5I", data)

        # charters: x, y, res
        self.char1 = (charpos[0], charpos[1], c1res)
        self.char2 = (charpos[2], charpos[3], c2res)

        hz7 = f.read(32)
        if hz7 != b"\xff" * 32:
            raise EngineError("Bad SAVE error in HZ7 field")

        if f.read():
            raise EngineError("Bad SAVE length (extra data)")


