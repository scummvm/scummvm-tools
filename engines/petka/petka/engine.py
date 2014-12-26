# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import os
import struct
import io

from .fman import FileManager
from . import EngineError

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

DLGOPS = {
    1:  ("BREAK",       0),
    2:  ("MENU",        1),
    3:  ("GOTO",        3),
    4:  ("MENURET",     4),
    6:  ("RETURN",      0),
    7:  ("PLAY",        5),
    8:  ("CIRCLE",      6),
}

class ScrObject:
    def __init__(self, idx, name):
        self.idx = idx
        self.name = name
        self.acts = None # action hadlers
        self.cast = None # object color (CASTS.INI)

class ScrActObject:
    def __init__(self, act_op, act_status, act_ref):
        self.act_op = act_op         # handler: opcode filter
        self.act_status = act_status # handler: status filter
        self.act_ref = act_ref       # handler: object idx filter
        self.ops = None              # operations

class ScrOpObject:
    def __init__(self, op_ref, op_code, op_arg1, op_arg2, op_arg3):
        self.op_ref = op_ref # object idx
        self.op_code = op_code # opcode
        self.op_arg1 = op_arg1 # resource id, etc
        self.op_arg2 = op_arg2
        self.op_arg3 = op_arg3

class MsgObject:
    def __init__(self, idx, wav, arg1, arg2, arg3):
        self.idx = idx
        self.msg_wav = wav   # wav filename
        self.msg_arg1 = arg1 # reference to object
        self.msg_arg2 = arg2
        self.msg_arg3 = arg3
        self.name = None

class DlgGrpObject:
    def __init__(self, idx, arg1):
        self.idx = idx
        self.grp_arg1 = arg1
        self.acts = None # dialog handlers

class DlgActObject:
    def __init__(self, opcode, ref, arg1, arg2):
        self.opcode = opcode # handler: opcode filter
        self.ref = ref       # handler: object idx filter
        self.arg1 = arg1
        self.arg2 = arg2
        self.dlgs = None     # dialogs
        self.obj = None      # handler object

class DlgObject:
    def __init__(self, op_start, arg1, arg2):
        self.op_start = op_start # start position
        self.arg1 = arg1
        self.arg2 = arg2
        self.ops = None          # operations list

class DlgOpObject:
    def __init__(self, opcode, arg, ref):
        self.opcode = opcode    # dialog opcode
        self.arg = arg          # argument (ref, offset etc.)
        self.ref = ref          # message idx
        self.msg = None         # message
        
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
        
    def init_empty(self, enc):
        self.enc = enc
        self.objects = []
        self.scenes = []
        self.obj_idx = {}
        self.scn_idx = {}
        self.msgs = []
        self.dlgs = []
        self.dlg_idx = {}
        self.dlgops = []
                
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
        self.init_empty(enc)
        self.fman = FileManager(folder)
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
        
        # load .STR
        strs = ["Flics", "Background", "Wav", "Music", "SFX", "Speech"]
        for strf in strs:
            if strf in ini:
                self.fman.load_store(ini[strf], 1)
        # load script.dat, backgrnd.bg, resources.qrc, etc
        self.load_script()
        # load persp & scenes enter points
        self.load_bgs()
        # load names & invntr
        self.load_names()
        # load dialogs
        self.load_dialogs()
        
    def load_script(self, scrname = None, bkgname = None, resname = None):
        self.objects = []
        self.scenes = []
        self.obj_idx = {}
        self.scn_idx = {}
        
        if scrname is None:
            try:
                data = self.fman.read_file(self.curr_path + "script.dat")
            except:
                raise EngineError("Can't open SCRIPT.DAT")
        else:
            try:
                f = open(scrname, "rb")
            except:
                raise EngineError("Can't open SCRIPT.DAT")
            try:
                data = f.read()
            finally:
                f.close()
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
                act_op, act_status, act_ref, num_op = struct.unpack_from(\
                    "<HBHI", data[off:off + 9])
                off += 9
                act = ScrActObject(act_op, act_status, act_ref)
                act.ops = []
                for j in range(num_op):
                    op = struct.unpack_from("<5H", data[off:off + 10])
                    off += 10
                    op = ScrOpObject(*op)
                    act.ops.append(op)
                acts.append(act)
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
            
        if bkgname is None:
            try:    
                data = self.fman.read_file(self.curr_path + "backgrnd.bg")
            except:
                data = None
        else:
            try:
                f = open(bkgname, "rb")
            except:
                data = None
            try:
                data = f.read()
            finally:
                f.close()

        if data:        
            num_rec = struct.unpack_from("<I", data[:4])[0]
        else:
            num_rec = 0
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
                    raise EngineError("DEBUG: Scene ref 0x{:x} not found".\
                        format(obj[0]))
                        
        if resname is None:
            try:
                f = self.fman.read_file_stream(self.curr_path + "resource.qrc")
            except:
                f = None
        else:
            try:
                f = open(resname, "rb")
            except:
                f = None
        try:            
            if f:            
                self.res, self.resord = self.parse_res(f)
            else:
                self.res = {}
                self.resord = []
        finally:
            if f:
                f.close()
        
    def load_names(self):
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

        self.casts = {}
        self.castsord = []
        fp = self.curr_path + "cast.ini"
        if self.fman.exists(fp):
            f = self.fman.read_file_stream(fp)
            ini = self.parse_ini(f)
            self.casts = ini["all"]
            self.castsord = ini["__order__"]["all"]
            f.close()

        # bind casts to objects
        for obj in self.objects:
            if obj.name in self.casts:
                # parse color
                try:
                    val = self.casts[obj.name].split(" ")
                    val = [x for x in val if x]
                    r = int(val[0])
                    g = int(val[1])
                    b = int(val[2])
                except:
                    r, g, b = 255, 255, 255
                obj.cast = (r, g, b)
            
    def load_bgs(self):
        # load BGS.INI
        self.bgs_ini = {}
        self.start_scene = None
        bgsfn = self.curr_path + "bgs.ini"
        if self.fman.exists(bgsfn):
            f = self.fman.read_file_stream(bgsfn)
            try:
                self.bgs_ini = self.parse_ini(f)
            finally:
                f.close()
        
        settings = self.bgs_ini.get("Settings", {})
        self.start_scene = settings.get("StartRoom", None)

        # bgs.ini: parse enter areas and perspective
        for scene in self.scenes:
            scene.entareas = None
            areas = self.bgs_ini.get(scene.name, {})
            if scene.name in self.bgs_ini:
                scene.entareas = []
                for key in areas.keys():
                    # search scene
                    sf = None
                    for scenefrom in self.scenes:
                        if scenefrom.name == key:
                            sf = scenefrom
                            break
                    value = areas[key]
                    # search objects
                    oo = None
                    for objon in self.objects:
                        if objon.name == value:
                            oo = objon
                            break
                    if sf and oo:
                        scene.entareas.append((sf, oo))
            # persp
            persp = settings.get(scene.name, "")
            persp = persp.split(" ")
            persp = [x for x in persp if x]
            try:
                persp = [float(persp[0]), float(persp[1]),
                    int(persp[2]), int(persp[3]), float(persp[4])]
            except:
                persp = None
            scene.persp = persp
    
            
    def load_dialogs(self, fixname = None, lodname = None, noobjref = False):
        self.msgs = []
        # DIALOGUES.LOD
        
        if lodname is None:
            try:    
                f = self.fman.read_file_stream(self.curr_path + "dialogue.lod")
            except:
                f = None
        else:
            try:
                f = open(lodname, "rb")
            except:
                f = None

        if f:
            try:
                temp = f.read(4)
                num_msg = struct.unpack_from("<I", temp)[0]
                for i in range(num_msg):
                    temp = f.read(24)
                    arg1, wav, arg2, arg3 = struct.unpack_from("<I12sII", temp)
                    msg = MsgObject(len(self.msgs), \
                        wav.decode(self.enc).strip(), arg1, arg2, arg3)
                    # scan objects
                    if not noobjref:
                        msg.obj = self.obj_idx.get(arg1, None)
                        if not msg.obj:
                            raise EngineError("DEBUG: Message ref = 0x{:x} not found".\
                            format(arg1))
                    self.msgs.append(msg)
                for i, capt in enumerate(f.read().split(b"\x00")):
                    if i < len(self.msgs):
                        self.msgs[i].name = capt.decode(self.enc)
            finally:
                if f:
                    f.close()

        self.dlgs = []
        self.dlg_idx = {}
        self.dlgops = []
        # DIALOGUES.FIX
        if fixname is None:
            try:    
                f = self.fman.read_file_stream(self.curr_path + "dialogue.fix")
            except:
                f = None
        else:
            try:
                f = open(fixname, "rb")
            except:
                f = None

        if f:
            try:
                temp = f.read(4)
                num_grps = struct.unpack_from("<I", temp)[0]
                for i in range(num_grps):
                    temp = f.read(12)
                    idx, num_acts, arg1 = struct.unpack_from("<III", temp)
                    grp = DlgGrpObject(idx, arg1)
                    grp.num_acts = num_acts
                    self.dlgs.append(grp)
                opref = {}
                for grp in self.dlgs:
                    self.dlg_idx[grp.idx] = grp
                    grp.acts = []
                    for i in range(grp.num_acts):
                        temp = f.read(16)
                        opcode, ref, num_dlgs, arg1, arg2 = \
                            struct.unpack_from("<2H3I", temp)
                        act = DlgActObject(opcode, ref, arg1, arg2)
                        act.num_dlgs = num_dlgs
                        if not noobjref:
                            if ref not in self.obj_idx:
                                raise EngineError("Dialog group 0x{:x} refered "\
                                    "to unexisted object 0x{:x}".format(
                                    grp.idx, ref))
                            act.obj = self.obj_idx[act.ref]
                        grp.acts.append(act)
                    for act in grp.acts:
                        act.dlgs = []
                        for i in range(act.num_dlgs):
                            temp = f.read(12)
                            op_start, arg1, arg2 = \
                                struct.unpack_from("<3I", temp)
                            if op_start in opref:
                                raise EngineError(
                                    "Multiple dialog opcodes reference")
                            dlg = DlgObject(op_start, arg1, arg2)
                            opref[op_start] = dlg
                            dlg.ops = None
                            act.dlgs.append(dlg)
                temp = f.read(4)
                num_ops = struct.unpack_from("<I", temp)[0]
                for oidx, i in enumerate(range(num_ops)):
                    temp = f.read(4)
                    ref, arg, code  = struct.unpack_from("<HBB", temp)
                    dlgop = DlgOpObject(code, arg, ref)
                    dlgop.pos = oidx
                    if ref < len(self.msgs):
                        dlgop.msg = self.msgs[ref]
                    self.dlgops.append(dlgop)
                dlg = None
                oparr = []
                for idx, oprec in enumerate(self.dlgops):
                    if idx in opref:
                        if len(oparr) > 0:
                            dlg.ops = oparr
                            oparr = []
                        dlg = opref[idx]
                    oparr.append(oprec)    
                if len(oparr) > 0:
                    dlg.ops = oparr
            finally:
                if f:
                    f.close()
                
    def write_script(self, f):
        f.write(struct.pack("<II", len(self.objects), len(self.scenes)))

        def write_rec(rec):
            ename = rec.name.encode(self.enc)
            f.write(struct.pack("<HI", rec.idx, len(ename)))
            f.write(ename)
            f.write(struct.pack("<I", len(rec.acts)))
            for act in rec.acts:
                f.write(struct.pack("<HBHI", act.act_op, act.act_status, 
                    act.act_ref, len(act.ops)))
                for op in act.ops:
                    f.write(struct.pack("<5H", op.op_ref, op.op_code,
                        op.op_arg1, op.op_arg2, op.op_arg3))
        
        for rec in self.objects + self.scenes:
            write_rec(rec)
        
    def write_backgrnd(self, f):
        lst = [scn for scn in self.scenes if scn.refs is not None]
        f.write(struct.pack("<I", len(lst)))
        for scn in lst:
            f.write(struct.pack("<HI", scn.idx, len(scn.refs)))
            for ref in scn.refs:
                f.write(struct.pack("<H5I", ref[0].idx, ref[1], ref[2], 
                    ref[3], ref[4], ref[5]))
                

    def write_lod(self, f):
        f.write(struct.pack("<I", len(self.msgs)))
        for msg in self.msgs:
            wav = msg.msg_wav.encode(self.enc)
            while len(wav) < 12:
                wav += b"\0"
            f.write(struct.pack("<I12sII", msg.msg_arg1, wav, msg.msg_arg2, 
                msg.msg_arg3))
        for msg in self.msgs:
            txt = msg.name.encode(self.enc)
            f.write(txt + b"\0")

    def write_fix(self, f):
        f.write(struct.pack("<I", len(self.dlgs)))
        for grp in self.dlgs:
            f.write(struct.pack("<3I", grp.idx, len(grp.acts), grp.grp_arg1))
        for grp in self.dlgs:
            for act in grp.acts:
                f.write(struct.pack("<2H3I", act.opcode, act.ref, 
                    len(act.dlgs), act.arg1, act.arg2))
            for act in grp.acts:
                for dlg in act.dlgs:
                    f.write(struct.pack("<3I", dlg.op_start, dlg.arg1, 
                        dlg.arg2))
        f.write(struct.pack("<I", len(self.dlgops)))
        for op in self.dlgops:
            f.write(struct.pack("<H2B", op.ref, op.arg, op.opcode))

    def load_save(self, ls):
        pass

