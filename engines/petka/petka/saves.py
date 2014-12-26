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
       
    def load_data(self, f, part, objnum):
        data = f.read(8)
        self.part, self.chap = struct.unpack("<2I", data)
        if self.part != part: return

        # read date stamp, asciiz
        stamp = b""
        while len(stamp) < 30:
            ch = f.read(1)
            if ch == b"\x00": break
            stamp += ch
        self.stamp = stamp.decode(self.enc)
        print("Saved at", len(self.stamp), repr(self.stamp))
        hz1 = f.read(max(0, 29 - len(stamp)))
        #print("HZ1", hz1)
        
        # read screenshot
        sl = 108 * 81 * 2
        rgb = f.read(sl)
        if len(rgb) != sl:
            raise EngineError("Bad SAVE length (no screenshot)")
        self.shot = BMPLoader()
        self.shot.load_raw(108, 81, rgb)
               
        print("RGB beg", binascii.hexlify(rgb[:8]))
        print("RGB end", binascii.hexlify(rgb[-8:]))

        hz2 = f.read(216)
        if hz2 != b"\x00" * 216:
            print("HZ2", hz2) # all zeroes?
            raise EngineError("Bad SAVE error in HZ2 field")
            
        #print("HZ2", hz2) # all zeroes?
        data = f.read(4)
        hz3 = struct.unpack("<I", data)[0]
        print("HZ3", hz3)
        
        def readstr():
            data = f.read(4)
            strlen = struct.unpack("<I", data)[0]
            s = f.read(strlen)
            #print("STR", strlen, data, s)
            return s.decode(self.enc)
        
        print("Req", objnum)
        for i in range(objnum):
            s1 = readstr()
            s2 = readstr()
            data = f.read(33)
            #print(i, s1, s2)
            
        
        # scene
        data = f.read(4)
        hz4len = struct.unpack("<I", data)[0]
        data = f.read(hz4len * 2)
        hz4 = struct.unpack("<{}H".format(hz4len), data)
        print("HZ4", hz4)

        self.scene = readstr()
        print("Scene:", self.scene)
        
        
        data = f.read()
        print(len(data))
