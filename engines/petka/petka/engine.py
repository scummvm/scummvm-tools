# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import os
import struct
import io

from .fman import FileManager

OPCODES = {
    1:  ("USE",         0),
    2:  ("SETPOS",      2),
    3:  ("GOTO",        0),
    4:  ("LOOK",        0),
    5:  ("SAY",         0),
    6:  ("TAKE",        0),
    9:  ("WALK",        2),
    10: ("TALK",        0),
    11: ("END",         0),
    14: ("SET",         1),
    15: ("SHOW",        1),
    16: ("HIDE",        0),
    17: ("DIALOG",      1),
    18: ("ZBUFFER",     0),
    19: ("TOTALINIT",   1),
    20: ("ANIMATE",     1),
    21: ("STATUS",      1),
    22: ("ADDINV",      0),
    23: ("DELINV",      0),
    24: ("STOP",        1),
    25: ("CURSOR",      1),
    26: ("OBJECTUSE",   0),
    27: ("ACTIVE",      1),
    28: ("SAID",        0),
    29: ("SETSEQ",      0),
    30: ("ENDSEQ",      0),
    31: ("CHECK",       0),
    32: ("IF",          0),
    33: ("DESCRIPTION", 0),
    34: ("HALF",        0),
    36: ("WALKTO",      0),
    37: ("WALKVICH",    0),
    38: ("INITBG",      0),
    39: ("USERMSG",     0),
    40: ("SYSTEM",      0),
    41: ("SETZBUFFER",  0),
    42: ("CONTINUE",    0),
    43: ("MAP",         1),
    44: ("PASSIVE",     1),
    45: ("NOMAP",       1),
    46: ("SETINV",      1),
    47: ("BGSFX",       1),
    48: ("MUSIC",       1),
    49: ("IMAGE",       1),
    50: ("STAND",       1),
    51: ("ON",          1),
    52: ("OFF",         1),
    53: ("PLAY",        1),
    54: ("LEAVEBG",     0),
    55: ("SHAKE",       1),
    56: ("SP",          2),
    57: ("RANDOM",      1),
    58: ("JUMP",        0),
    59: ("JUMPVICH",    0),
    60: ("PART",        2),
    61: ("CHAPTER",     2),
    62: ("AVI",         1),
    63: ("TOMAP",       0),
}

class ScrObject:
    def __init__(self, idx, name):
        self.idx = idx
        self.name = name

class Engine:
    def __init__(self):
        self.fman = None
        self.parts = []
        self.start_part = None
        self.start_chap = None
        self.start_scene = None

        self.curr_part = None
        self.curr_chap = None
        
        self.curr_path = None
        self.curr_speech = None
        self.curr_diskid = None
        
        
    def parse_ini(self, f):
        # parse ini settings
        curr_sect = None
        ini = {}
        order_sect = []
        orders = {}
        for line in f.readlines():
            line = line.decode(self.enc).strip()
            if len(line) == 0: continue
            if line[:1] == ";": continue
            if line[:1] == "[" and line[-1:] == "]":
                if curr_sect is not None:
                    orders[curr_sect] = order
                curr_sect = line[1:-1].strip()
                order_sect.append(curr_sect)
                order = []
                ini[curr_sect] = {}
                continue
            kv = line.split("=", 1)
            if len(kv) != 2: continue
            ini[curr_sect][kv[0].strip()] = kv[1].strip()
            order.append(kv[0].strip())
        orders[curr_sect] = order
        ini["__ordersect__"] = order_sect
        ini["__order__"] = orders
        return ini
        
    def parse_res(self, f):
        res  = {}
        resord = []
        for line in f.readlines():
            line = line.decode(self.enc).strip()
            if len(line) == 0:
                continue
            pair = line.split("=", 1)
            if len(pair) < 2:
                continue
            value = pair[1].strip()
            if value[:1] == "=":
                value = value[1:].strip()
            res_id = int(pair[0].strip(), 10)
            res[res_id] = value
            resord.append(res_id)
        return res, resord
        
    def load_data(self, folder, enc):
        self.fman = FileManager(folder)
        self.enc = enc
        # load PARTS.INI
        pf = self.fman.find_path("parts.ini")
        if pf:
            f = open(pf, "rb")
            try:
                self.parts_ini = self.parse_ini(f)
            finally:
                f.close()
            for sect in self.parts_ini["__ordersect__"]:
                data = self.parts_ini[sect]
                if sect == "All":
                    if "Part" in data:
                        self.start_part = int(data["Part"])
                    if "Chapter" in data:
                        self.start_chap = int(data["Chapter"])
                elif sect[:5] == "Part ":
                    self.parts.append(sect)
        else:
            # load BGS.INI only (e.g. DEMO)
            self.parts_ini = None
            
        # std stores
        self.fman.load_store("patch.str")
        self.fman.load_store("main.str")

    def open_part(self, part, chap):
        self.fman.unload_stores(1)
        self.curr_part = part
        self.curr_chap = chap
        if self.parts_ini:
            pname = "Part {}".format(part)
            pcname = pname
            if chap:
                pcname += " Chapter {}".format(chap)
            ini = self.parts_ini[pname]
            self.curr_path = ini["CurrentPath"]
            self.curr_speech = ini["PathSpeech"]
            self.curr_diskid = ini["DiskID"]
            inic = self.parts_ini[pcname]
            if "Chapter" in inic:
                self.fman.load_store(inic["Chapter"], 1)
        else:
            ini = {}
            self.curr_path = ""
            self.curr_speech = ""
            self.curr_diskid = None
        
        # load BGS.INI
        self.bgs_ini = {}
        self.start_scene = None
        pf = self.fman.find_path(self.curr_path + "bgs.ini")
        if pf:
            f = open(pf, "rb")
            try:
                self.bgs_ini = self.parse_ini(f)
            finally:
                f.close()
            if "Settings" in self.bgs_ini:
                if "StartRoom" in self.bgs_ini["Settings"]:
                    self.start_scene = self.bgs_ini["Settings"]["StartRoom"]
        # load .STR
        strs = ["Flics", "Background", "Wav", "Music", "SFX"]
        for strf in strs:
            pf = self.fman.find_path(self.curr_path + "bgs.ini")
            if not pf: continue
            if strf in ini:
                self.fman.load_store(ini[strf], 1)
        # load script
        self.load_script()
        
    def load_script(self):
        self.objects = []
        self.scenes = []
        self.obj_idx = {}
        self.scn_idx = {}

        data = self.fman.read_file(self.curr_path + "script.dat")
        num_obj, num_scn = struct.unpack_from("<II", data[:8])
        off = 8
        def read_rec(off):
            obj_id, name_len = struct.unpack_from("<HI", data[off:off + 6])
            off += 6
            name = data[off:off + name_len].decode(self.enc)
            off += name_len
            num_act = struct.unpack_from("<I", data[off:off + 4])[0]
            off += 4
            acts = []
            for i in range(num_act):
                act_id, act_cond, act_arg, num_op = struct.unpack_from(\
                    "<HBHI", data[off:off + 9])
                off += 9
                ops = []
                for j in range(num_op):
                    op = struct.unpack_from("<5H", data[off:off + 10])
                    off += 10
                    ops.append(op)
                acts.append([act_id, act_cond, act_arg, ops])
            rec = ScrObject(obj_id, name)
            rec.acts = acts
            return off, rec
        
        for i in range(num_obj):
            off, obj = read_rec(off)
            self.objects.append(obj)
            self.obj_idx[obj.idx] = obj

        for i in range(num_scn):
            off, scn = read_rec(off)
            self.scenes.append(scn)
            self.scn_idx[scn.idx] = scn
            
        data = self.fman.read_file(self.curr_path + "backgrnd.bg")
        num_rec = struct.unpack_from("<I", data[:4])[0]
        off = 4
        for i in range(num_rec):
            scn_ref, num_ref = struct.unpack_from("<HI", data[off:off + 6])
            off += 6
            if scn_ref in self.scn_idx:
                scn = self.scn_idx[scn_ref]    
                scn.refs = []
            else:
                raise EngineError("DEBUG: Scene ID = 0x{:x} not found".\
                    format(scn_ref))

            for j in range(num_ref):
                ref = struct.unpack_from("<H5I", data[off:off + 22])
                off += 22
                if ref[0] in self.obj_idx:
                    obj = self.obj_idx[ref[0]]
                    scn.refs.append([obj] + list(ref[1:]))
                else:
                    raise EngineError("DEBUG: Object ID = 0x{:x} not found".\
                        format(obj[0]))
                        
        f = self.fman.read_file_stream(self.curr_path + "resource.qrc")
        self.res, self.resord = self.parse_res(f)
        f.close()
        
        self.names = {}
        self.namesord = []
        fp = self.curr_path + "names.ini"
        if self.fman.exists(fp):
            f = self.fman.read_file_stream(fp)
            ini = self.parse_ini(f)
            self.names = ini["all"]
            self.namesord = ini["__order__"]["all"]
            f.close()

        self.invntr = {}
        self.invntrord = []
        fp = self.curr_path + "invntr.txt"
        if self.fman.exists(fp):
            f = self.fman.read_file_stream(fp)
            ini = self.parse_ini(f)
            self.invntr = ini["ALL"]
            self.invntrord = ini["__order__"]["ALL"]
            f.close()
        
