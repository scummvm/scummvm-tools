# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import os
import struct
import io

from .fman import FileManager

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
        

