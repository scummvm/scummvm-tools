# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import array, struct

from . import EngineError

class BMPLoader:
    def __init__(self):
        self.raw = None
        self.width = 0
        self.height = 0
        
    def load_data(self, f):
        # TODO: normal BMP, rle BMP
        # check magic string "BM"
        temp = f.read(2)
        if temp != b"BM":
            raise EngineError("Bad magic string")
        off = 2
        
        temp = f.read(12)
        f_sz, res1, res2, data_offset = struct.unpack_from("<IHHI", temp)
        off += 12
        
        # read next 40 bytes, BITMAPINFOHEADER
        temp = f.read(40)
        pict = struct.unpack_from("<IiiHHIIiiII", temp)
        off += 40
        if pict[0] != 40:
            raise EngineError("Unsupported InfoHeader")
        pictw = pict[1]
        self.width = pictw
        picth = pict[2]
        self.height = picth
        
        # read data_offset - 40 - 6 bytes
        delta = data_offset - 40 - 6
        if delta < 0:
            raise EngineError("To small bitmap data offset")
        if delta != 8:
            raise EngineError("Unsupported Header at 0x36")
        temp = f.read(delta)
        hdr36 = struct.unpack_from("<II", temp)
        off += delta

        bsz = pictw * picth * 2
        picture_data = f.read(bsz)
        off += bsz
        if len(picture_data) != bsz:
            raise EngineError("Bitmap truncated, need {}, got {}".format(bsz, \
                len(picture_data)))

        # read 2 zero bytes
        temp = f.read(2)
        if temp != b"\x00\x00":
            raise EngineError("Magic zero bytes absent or mismatch")
        off += 2

        if len(f.read()) > 0:
            raise EngineError("BMP read error, some data unparsed")
                
        # convert 16 bit to 24
        b16arr = array.array("H") # unsigned short
        b16arr.frombytes(picture_data)
        rgb = array.array("B")
        for b16 in b16arr:
            rgb.append((b16 >> 5) & 0b11111000)
            rgb.append((b16 << 5) & 0b11100000 | (b16 >> 11) & 0b00011100)
            rgb.append((b16 << 0) &0b11111000) 
        # Y-mirror
        self.rgb = array.array("B")
        for i in range(picth):
            off = (picth - i - 1) * pictw * 3
            self.rgb += rgb[off:off + pictw * 3]
        