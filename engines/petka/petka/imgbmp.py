# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import array, struct

from . import EngineError

class BMPLoader:
    def __init__(self):
        self.raw = None
        self.width = 0
        self.height = 0
        
    def load_data(self, data):
        # TODO: normal BMP, rle BMP
        # check magic string "BM"
        if data[:2] != b"BM":
            raise EngineError("Bad magic string")
        off = 2
        
        f_sz, res1, res2, data_offset = struct.unpack_from("<IHHI", \
            data[off:off + 12])
        off += 12
        
        # read next 40 bytes, BITMAPINFOHEADER
        pict = struct.unpack_from("<IiiHHIIiiII", data[off:off + 40])
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
        hdr36 = struct.unpack_from("<II", data[off:off + delta])
        off += delta

        bsz = pictw * picth * 2
        picture_data = data[off:off + bsz]
        off += bsz
        if len(picture_data) != bsz:
            raise EngineError("Bitmap truncated, need {}, got {}".format(bsz, \
                len(picture_data)))

        # read 2 zero bytes
        if data[off:off + 2] != b"\x00\x00":
            raise EngineError("Magic zero bytes absent [{:02x},{:02x}]".\
                format(data[off], data[off + 1]))
        off += 2

        if len(data) - off > 0:
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
        
