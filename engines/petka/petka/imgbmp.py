# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import array, struct, io

from . import EngineError

try:
    from PIL import Image
except ImportError:
    Image = None

class BMPLoader:
    def __init__(self):
        self.rgb = None
        self.image = None
        self.width = 0
        self.height = 0
        
    def load_data_int16(self, f):
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
        picth = pict[2]
        
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
        #temp = f.read(2)
        #if temp != b"\x00\x00":
        #    raise EngineError("Magic zero bytes absent or mismatch")
        #off += 2

        temp = f.read(2)
        if len(temp) > 0:
            if temp != b"\x00\x00":
                raise EngineError("BMP read error, some data unparsed")
                
        return pictw, picth, picture_data
        
    def pixelswap16(self, pw, ph, pd):
        # convert 16 bit to 24
        b16arr = array.array("H") # unsigned short
        b16arr.frombytes(pd)
        b16arr.byteswap()
        rgb = array.array("B", [0] * pw * ph * 3)
        for j in range(ph):
            for i in range(pw):
                off = (ph - j - 1) * pw * 3 + i * 3
                b16 = b16arr[j * pw + i]
                rgb[off] = (b16 << 3) & 0b11111000
                rgb[off + 1] = (b16 >> 3) & 0b11111100
                rgb[off + 2] = (b16 >> 8) & 0b11111000
        return rgb

    def load_info(self, f):
        try:
            pw, ph, pd = self.load_data_int16(f)
            self.width = pw
            self.height = ph
        except:
            f.seek(0)
            self.image = Image.open(f)
        
    def load_data(self, f):
        try:
            pw, ph, pd = self.load_data_int16(f)
            if Image:
                pd = self.pixelswap16(pw, ph, pd).tobytes()
                self.image = Image.frombytes("RGB", (pw, ph), pd) 
            else:
                self.width = pw
                self.height = ph
                self.rgb = self.pixelswap16(pw, ph, pd)
        except:
            f.seek(0)
            self.image = Image.open(f)
            
