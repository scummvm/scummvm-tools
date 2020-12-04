# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import array, struct, io

from . import EngineError

try:
    from PIL import Image
except ImportError:
    Image = None

try:
    from PIL import FliImagePlugin
except ImportError:
    pass

FLC_HEADER = [
    ["fsize",        1, "I", False],
    ["ftype",        1, "H", False],
    ["frames_num",   1, "H", True],
    ["width",        1, "H", True],
    ["height",       1, "H", True],
    ["depth",        1, "H", False],
    ["flags",        1, "H", True],
    ["speed",        1, "I", True],
    ["reserved1",    1, "H", False],
    ["created",      1, "I", True],
    ["creator",      1, "I", True],
    ["updated",      1, "I", True],
    ["updater",      1, "I", True],
    ["aspect_dx",    1, "H", True],
    ["aspect_dy",    1, "H", True],
    ["ext_flags",    1, "H", False],
    ["keyframes",    1, "H", False],
    ["totalframes",  1, "H", False],
    ["req_memory",   1, "I", False],
    ["max_regions",  1, "H", False],
    ["transp_num",   1, "H", False],
    ["reserved2",   24, "s", False],
    ["oframe1",      1, "I", False],
    ["oframe2",      1, "i", False],
    ["reserved3",   40, "s", False],
]

class FLCLoader:
    def __init__(self):
        self.rgb = None
        self.image = None
        self.width = 0
        self.height = 0
        self.frame_num = 0
        self.delay = 0


    def load_info(self, f):
        self.image = Image.open(f)
        self.frame_num = 1
        try:
            while 1:
                self.image.seek(self.image.tell() + 1)
                self.frame_num += 1
        except EOFError:
            pass # end of sequence


    def parseflcchunks(self, f, offset, limit, level = 0, maxchunks = None,):
        def check_hdr(size, delta, name, offset):
            if delta < size:
                raise EngineError("Incorrect FLC %s chunk at 0x{:08x}".format(
                    (name, offset)))

        chunks = []
        while True:
            if limit is not None:
                if offset >= limit:
                    break
            if maxchunks is not None:
                if len(chunks) >= maxchunks:
                    break
            chunk = {"offset": offset}
            temp = f.read(6)
            sz, tp = struct.unpack_from("<IH", temp)
            offset += 6

            chunk["size"] = sz
            chunk["type"] = tp
            delta = sz - 6
            if delta < 0:
                raise EngineError("Incorrect FLC chunk at 0x{:08x}".format(
                    chunk["offset"]))

            raw_chunks = [
                0x4, # COLOR_256
                0x7, # DELTA_FLC
                0xf, # BYTE_RUN
                0xF100, # PREFIX_TYPE - mismaked, out destination ignore this
            ]
            #print("{}CHUNK 0x{:x}, size = {}".format("  "*level, tp, sz))
            # do not parse 3rd level 0x12 chunk
            if tp == 0x12 and level == 2:
                tp = 0x4

            # parse chunks
            if tp in raw_chunks:
                temp = f.read(delta)
                offset += delta
                chunk["data"] = temp
            elif tp == 0x12:
                # PSTAMP
                check_hdr(6, delta, "PSTAMP", offset)
                temp = f.read(6)
                delta -= 6
                height, width, xlate = struct.unpack_from("<3H", temp)
                offset += 6
                offset, subchunks = self.parseflcchunks(f, offset,
                    offset + delta, level + 1, 1)
                chunk["chunks"] = subchunks
                #print(subchunks)
            elif tp == 0xF1FA:
                # FRAME_TYPE
                check_hdr(10, delta, "FRAME_TYPE", offset)
                temp = f.read(10)
                delta -= 10
                sub_num, delay, reserved, width, height = \
                    struct.unpack_from("<5H", temp)
                offset += 10
                chunk["delay"] = delay
                chunk["width"] = width
                chunk["height"] = height
                offset, subchunks = self.parseflcchunks(f, offset,
                    offset + delta, level + 1, sub_num)
                chunk["chunks"] = subchunks
            else:
                raise Exception("Unknown FLC chunk type 0x{:04x} at 0x{:x08x}".\
                    format(tp, offset))

            chunks.append(chunk)

        return offset, chunks

    def load_data(self, f):
        # parse header
        offset = 0
        hdr_keys = []
        hdr_struct = "<"
        for hnam, hsz, htp, hed in FLC_HEADER:
            hdr_keys.append(hnam)
            if hsz == 1:
                hdr_struct += htp
            else:
                hdr_struct += "%d" % hsz + htp

        header = {}
        temp = f.read(128)
        hdr = struct.unpack_from(hdr_struct, temp)

        offset += 128

        if len(hdr) != len(hdr_keys):
            raise EngineError("Incorrect FLC header {} != {}".format(
                len(hdr), len(hdr_keys)))
        for hid in range(len(hdr)):
            header[hdr_keys[hid]] = hdr[hid]

        if header["ftype"] != 0xAF12:
            raise EngineError("Unsupported FLC type (0x{:04x})".format(
                header["ftype"]))

        # check if not EGI ext
        if header["creator"] == 0x45474900:
            if header["ext_flags"] != 0:
                raise EngineError("Unsupported FLC EGI extension")

        # NOTE: we recreate FLC to avoid Pilllow bug
        #  1. remove 0xf100 chunk  (PREFIX, implementation specific)
        #  2. remobe 0x12 subchunk (PSTAMP) from 1st frame

        # read chunks
        _, chunks = self.parseflcchunks(f, offset, header["fsize"])

        f.seek(0)
        buf = io.BytesIO()
        buf.write(f.read(128)) # clone header
        for chunk in chunks:
            if chunk["type"] == 0xF100:
                continue
            elif chunk["type"] == 0xF1FA:
                rebuild = False
                nchunks = []
                nsz = 16 # I6H - type, size, sub_num, delay,
                         # reserved, width, height
                for schunk in chunk["chunks"]:
                    if schunk["type"] == 0x12: # detect mailformed PSTAMP
                        rebuild = True
                    elif rebuild:
                        nchunks.append(schunk)
                        nsz += schunk["size"]
                if rebuild:
                    buf.write(struct.pack("<I6H", nsz, 0xF1FA, len(nchunks),
                        chunk["delay"], 0, chunk["width"], chunk["height"]))
                    for schunk in nchunks:
                        f.seek(schunk["offset"])
                        buf.write(f.read(schunk["size"]))
                    continue
            # copy chunk
            buf.write(f.read(chunk["size"]))

        buf.seek(0)
        self.image = Image.open(buf)
