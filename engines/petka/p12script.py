#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import os, sys
import traceback
import argparse
import io
import hashlib

import petka
import petka.engine

APPNAME = "P1&2 Compiler and decompiler"
VERSION = "v0.3h 2015-01-12"

class ScriptSyntaxError(Exception): pass

def find_in_folder(folder, name, ifnot = True):
    for item in os.listdir(folder):
        if item.upper() == name.upper():
            return os.path.join(folder, item)
    if ifnot:
        return os.path.join(folder, name)
    else:
        return None

class P12Compiler:
    def __init__(self):
        pass
        
    # =======================================================================
    # compiler utils
    # =======================================================================

    def tokenizer(self, source, enc):
        for lineno, line in enumerate(source.readlines(), 1):
            line = line.decode(enc or "UTF-8").strip()
            # eliminate comment
            nline = []
            nmode = 0 # 0 - common, 2 - in string
            nitem = ""
            nitemesc = False
            for ch in line:
                if nmode == 1:
                    if nitemesc:
                        nitem += ch
                        nitemesc = False
                    else:
                        if ch == "\\":
                            # esc-sequence
                            nitemesc = True
                            continue
                        elif ch == "\"":
                            nitem += ch
                            nmode = 0
                            if nitem[:1] == nitem[-1:] == "\"":
                                nitem = nitem[1:-1]
                            nline.append(nitem)
                            nitem = ""
                            nitemesc = False
                            continue
                        nitem += ch
                else:    
                    if ch == "\"":
                        nmode = 1
                    elif ch == "#":
                        break
                    elif ch == " " or ch == "\t":
                        if len(nitem) > 0:
                            nline.append(nitem)
                            nitem = ""
                        continue
                    nitem += ch

            if len(nitem) > 0:
                nline.append(nitem)
                
            yield lineno, nline
        

    def check8(self, value, name, lineno):
        if value == -1:
            return 0xff
        if value < 0:
            raise ScriptSyntaxError("Error at {}: value for \"{}\" too "\
                "small - {}".format(lineno, name, value))
        if value > 0xff:
            raise ScriptSyntaxError("Error at {}: value for \"{}\" too "\
                "big - {} > 0xff".format(lineno, name, value))
        return value
    
    def check16(self, value, name, lineno):
        if value == -1:
            return 0xffff
        if value < 0:
            raise ScriptSyntaxError("Error at {}: value for \"{}\" too "\
                "small - {}".format(lineno, name, value))
        if value > 0xffff:
            raise ScriptSyntaxError("Error at {}: value for \"{}\" too "\
                "big - {} > 0xffff".format(lineno, name, value))
        return value

    def check32(self, value, name, lineno):
        if value == -1:
            return 0xffffffff
        if value < 0:
            raise ScriptSyntaxError("Error at {}: value for \"{}\" too "\
                "small - {}".format(lineno, name, value))
        if value > 0xffffffff:
            raise ScriptSyntaxError("Error at {}: value for \"{}\" too "\
                "big - {} > 0xffffffff".format(lineno, name, value))
        return value
    
    def convertnum(self, value):
        num = None
        if value[:2].upper() == "0X":
            try:
                num = int(value[2:], 16)
            except:
                pass
        else:                            
            try:
                num = int(value, 10)
            except:
                pass
        return num

    def checkusedid(self, ident, lineno):
        if ident in self.usedid:
            raise ScriptSyntaxError("Error at {}: identificator \"{}\" "\
                "already used at line {}".format(lineno, ident, \
                self.usedid[ident][0]))
        self.usedid[ident] = [lineno, None]
    
    def setidentvalue(self, ident, value):
        if self.usedid[ident][1] is None:
            self.usedid[ident][1] = value
        else:
            raise ScriptSyntaxError("Redefine value for \"{}\" ({}) from "\
                "\"{}\" to \"{}\"".format(ident, self.usedid[ident][0], \
                self.usedid[ident][0], value))
    
    def getidentvalue(self, ident):
        return self.usedid[ident][1]
    
    def checkident(self, ident, name, lineno):
        if ident.upper() in self.reservedid:
            raise ScriptSyntaxError("Error at {}: identificator \"{}\" "\
                "in {} forbidden".format(lineno, ident, name))
        if ident not in self.usedid:
            raise ScriptSyntaxError("Error at {}: identificator \"{}\" "\
                "in {} not found".format(lineno, ident, name))
        if self.usedid[ident][1] is None:
            raise ScriptSyntaxError("Error at {}: identificator \"{}\" "\
                "in {} has no value (internal error)".\
                format(lineno, ident, name))

    def checkstruct(self, name, struct, data, lineno):
        try:
            struct.build(data)
        except Exception as e:
            msg = "Internal error {}".format(name)
            if lineno is not None:
                msg += ", at line {}".format(lineno)
            raise ScriptSyntaxError(msg + "\n" + str(e))

    def convertargs(self, fmt, args, lineno):
        # fmt = [(name1, check1, withident),]
        pargs = []
        argnum = [None] * len(args)
        for i, arg in enumerate(args):
            argnum[i] = self.convertnum(arg)
            if argnum[i] is None:
                if not fmt[i][2]:
                    raise ScriptSyntaxError("Error at {}: bad number for {}".\
                        format(lineno, fmt[i][0]))
                self.checkident(arg, fmt[i][0], lineno)
                argnum[i] = self.getidentvalue(arg)
            argnum[i] = fmt[i][1](argnum[i], fmt[i][0], lineno)
        return argnum

    # =======================================================================
    # compile SCRIPT.DAT
    # =======================================================================

    def compile_script(self, source, destfolder, enc = None, \
            st_scr = None, st_bkg = None, st_res = None):

        pe = petka.Engine()
        pe.init_empty("cp1251")
        
        mode = 0 # 0 - common, 1 - object/scene, 2 - on
        mode1tp = None

        # used identificators
        self.usedid = {}
        # compiled resources
        compres = []
        # compiled obj
        compobj = []
        # compled scenes
        compscene = []
        # current item
        compitem = None
        # obj/scene used numbers
        compitemused = {}
        # current action
        compact = None
                   
        revOPS = {}
        for ok, ov in petka.OPCODES.items():
            revOPS[ov[0]] = ok
        self.reservedid = ["THIS", "OBJ", "ENDOBJ", "SCENE", "ENDSCENE", \
            "RES", "REF", "ON", "ENDON"] + list(revOPS.keys())

        for lineno, tokens in self.tokenizer(source, enc):
            if len(tokens) == 0:
                continue
            if mode == 0:
                # accept: RES, OBJ, SCENE
                cmd = tokens[0].upper()
                if cmd == "RES":
                    if len(tokens) != 4:
                        raise ScriptSyntaxError("Error at {}: RES syntax "\
                            "error".format(lineno, cmd))
                    self.checkusedid(tokens[1], lineno)
                    compres.append((lineno, tokens[1], \
                        tokens[2], tokens[3]))
                elif cmd == "OBJ" or cmd == "SCENE":
                    # object / scene
                    mode = 1
                    mode1tp = cmd
                    # check syntax
                    if len(tokens) != 4:
                        raise ScriptSyntaxError("Error at {}: {} syntax "\
                            "error".format(lineno, cmd))
                    self.checkusedid(tokens[1], lineno)
                    num = self.convertnum(tokens[2])
                    if num is not None:
                        if num in compitemused:
                            raise ScriptSyntaxError("Error at {}: {} "\
                                "number {} already used at {}".format(\
                                    lineno, cmd.lower(), num, \
                                    compitemused[num]))
                        else:
                            compitemused[num] = lineno
                        num = self.check16(num, "{} number".format(cmd.lower()),
                            lineno)
                        self.setidentvalue(tokens[1], num)
                    else:
                        raise ScriptSyntaxError("Error at {}: {} number bad "\
                            "format \"{}\" ".format(lineno, cmd.lower(),
                                tokens[2]))
                    compitem = {"tp": cmd, "lineno": lineno, \
                        "ident": tokens[1], "num": num, \
                        "name": tokens[3], "ref": None, "acts": []}
                else:
                    raise ScriptSyntaxError("Error at {}: unknown syntax "\
                        "\"{}\"".format(lineno, cmd))
            elif mode == 1:                            
                # accept: REF, ON, ENDOBJ, ENDSCENE
                cmd = tokens[0].upper()
                if cmd == "REF" and mode1tp == "SCENE":
                    if compitem["ref"] is None:
                        compitem["ref"] = []
                    # check format
                    if len(tokens) < 4 or len(tokens) > 7:
                        raise ScriptSyntaxError("Error at {}: unknown REF "\
                            "syntax in {}".format(lineno, mode1tp))
                    while len(tokens) < 7:
                        tokens.append("-1")
                    compitem["ref"].append([lineno] + tokens[1:])
                elif cmd == "ZEROREF" and mode1tp == "SCENE":
                    # zero rererence at all
                    if compitem["ref"] is not None:
                        # can't empty any
                        raise ScriptSyntaxError("Error at {}: ref already "\
                            "specified".format(lineno))
                    compitem["ref"] = []
                elif cmd == "ON":
                    # on action
                    if len(tokens) != 2 and len(tokens) != 4:
                        raise ScriptSyntaxError("Error at {}: unknown ON "\
                            "syntax in {}".format(lineno, mode1tp))
                    son = self.convertnum(tokens[1])
                    if son is None:
                        if tokens[1].upper() in revOPS:
                            son = revOPS[tokens[1].upper()]
                        else:
                            raise ScriptSyntaxError("Error at {}: unknown ON "\
                                "OPREF \"{}\" in {}".\
                                    format(lineno, tokens[1], mode1tp))
                    else:
                        son = self.check16(son, "ON opref", lineno)
                    if len(tokens) == 2:
                        tokens += ["-1", "-1"]
                    status = self.convertnum(tokens[2])
                    if status is None:
                        raise ScriptSyntaxError("Error at {}: unknown ON "\
                            "status \"{}\" in {}".\
                                format(lineno, tokens[2], mode1tp))
                    status = self.check8(status, "ON status", lineno)
                    compact = {"son": son, "ops": [], "status": status, \
                        "sonref": tokens[3], "lineno": lineno}
                    mode = 2
                elif cmd == "END" + mode1tp and len(tokens) == 1:
                    if mode1tp == "OBJ":
                        compobj.append(compitem)                        
                    else:
                        compscene.append(compitem)                        
                    compitem = None
                    # return
                    mode = 0
                else:
                    raise ScriptSyntaxError("Error at {}: unknown syntax "\
                        "\"{}\" in {}".\
                        format(lineno, cmd, mode1tp))
            elif mode == 2:
                # accept: ENDON, OPCODE, any number
                cmd = tokens[0].upper()
                if cmd == "ENDON" and len(tokens) == 1:
                    compitem["acts"].append(compact)
                    compact = None                
                    mode = 1
                else:
                    # check format
                    if len(tokens) < 2 or len(tokens) > 5:
                        raise ScriptSyntaxError("Error at {}: unknown OP "\
                            "syntax in {}".format(lineno, mode1tp))
                    while len(tokens) < 5:
                        tokens.append("-1")
                    op = {}
                    if cmd in revOPS:
                        # opcode
                        opcode = revOPS[cmd]
                    else:
                        opcode = self.convertnum(cmd)
                        if opcode is None:
                            raise ScriptSyntaxError("Error at {}: unknown OP "\
                                "\"{}\" in ON".\
                                format(lineno, cmd))
                        opcode = self.check16(opcode, "opcode", lineno)
                    compact["ops"].append({"opcode": opcode, "lineno": lineno, \
                        "obj_ref": tokens[1], "args": tokens[2:]})
            else:
                raise ScriptSyntaxError("Error at {}: unknown parser mode {}".\
                    format(lineno, mode))

        # check unclosed objects
        if mode != 0:
            raise ScriptSyntaxError("Error at {}: unfinished structure".\
                format(lineno))
    
        # second stage - RES
        # 1. capture res with numbers
        resused = {}
        for lineno, ident, resnum, path in compres:
            num = self.convertnum(resnum)
            if num is not None:
                if num in resused:
                    raise ScriptSyntaxError("Error at {}: RES number {} "\
                        "already used at {}".format(lineno, num, resused[num]))
                else:
                    resused[num] = lineno
                num = self.check16(num, "RES id", lineno)
                self.setidentvalue(ident, num)
            #elif resnum == "_":
            #    pass
            else:
                raise ScriptSyntaxError("Error at {}: RES number bad "\
                    "format \"{}\" ".format(lineno, resnum))
        # disabled, unsure about valid numbers range
        # 2. autonumber rest
        #autoresnum = 0xfffe
        #for lineno, ident, resnum, path in compres:
        #    if resnum == "_":
        #        while autoresnum > 0:
        #            num = autoresnum
        #            if num in resused:
        #                autoresnum -= 1
        #                continue
        #            else:
        #                self.setidentvalue(ident, num)
        #                resused[num] = lineno
        #                break                    
        #        if autoresnum <= 0:
        #            raise ScriptSyntaxError("Error at {}: out of resources "\
        #                "number".format(lineno))
        # 3. compile

        if destfolder is not None:
            f = open(os.path.join(destfolder, "resource.qrc"), "wb")
        else:
            f = st_res
        try:
            def writer(msg):
                f.write((msg + "\r\n").encode("CP1251"))
            writer("")
            for lineno, ident, resnum, path in compres:
                writer("{:d} == {}".format(self.getidentvalue(ident), path))
        finally:
            if destfolder is not None:
                f.close()
        print("RESOURCE.QRC saved: {} items".format(len(resused)))
            
        # second stage - OBJ
        def makerec(citem):
            item = petka.engine.ScrObject(citem["num"], citem["name"])
            item.acts = []
            for act in citem["acts"]:
                if act["sonref"].upper() == "THIS":
                    sonref = item.idx
                else:
                    sonref = self.convertnum(act["sonref"])
                if sonref is not None:
                    # direct number
                    sonref = self.check16(sonref, "ON object ref", 
                        act["lineno"])
                else:
                    # get by ident
                    self.checkident(act["sonref"], "ON object ref",
                        act["lineno"])
                    sonref = self.getidentvalue(act["sonref"])
                onrec = petka.engine.ScrActObject(act["son"], 
                    act["status"], sonref)
                onrec.ops = []
                for op in act["ops"]:
                    # object ref
                    if op["obj_ref"].upper() == "THIS":
                        opref = item.idx
                    else:
                        opref = self.convertnum(op["obj_ref"])
                    if opref is not None:
                        # direct number
                        opref = self.check16(opref, "OP object", op["lineno"])
                    else:
                        # get by ident
                        self.checkident(op["obj_ref"], "OP object",
                            op["lineno"])
                        opref = self.getidentvalue(op["obj_ref"])
                    # arguments
                    fmt = []
                    for i in range(3):
                        fmt.append(("OP argument {}".format(i + 1),
                            self.check16, True))
                    argnum = self.convertargs(fmt, op["args"], op["lineno"])
                    oprec =  petka.engine.ScrOpObject(opref, op["opcode"], 
                        argnum[0], argnum[1], argnum[2])
                    onrec.ops.append(oprec)
                item.acts.append(onrec)
            return item
            
        for citem in compobj:
            objrec = makerec(citem)
            pe.objects.append(objrec)
            pe.obj_idx[objrec.idx] = objrec
                  
        # second stage - SCENE
        backgrnd = []
        num_bkg = 0
        for citem in compscene:
            scenerec = makerec(citem)
            scenerec.refs = None
            pe.scenes.append(scenerec)

            if citem["ref"] is not None:
                scenerec.refs = []
                num_bkg += 1
                for ref in citem["ref"]:
                    fmt = [("REF object", self.check16, True)]
                    for i in range(5):
                        fmt.append(("REF argument {}".format(i + 1), \
                            self.check32, True))
                    argnum = self.convertargs(fmt, ref[1:], ref[0])
                    # build REF
                    if argnum[0] not in pe.obj_idx:
                        raise ScriptSyntaxError("Error at {}: referenced "
                            "object 0x{:x} not found".\
                            format(lineno, argnum[0]))
                    scenerec.refs.append([pe.obj_idx[argnum[0]], argnum[1],
                         argnum[2],  argnum[3],  argnum[4],  argnum[5]])


        if destfolder is not None:
            f = open(os.path.join(destfolder, "backgrnd.bg"), "wb")
        else:
            f = st_bkg
        try:
            pe.write_backgrnd(f)
        finally:
            if destfolder is not None:
                f.close()
        print("BACKGRND.BG saved: {} items".format(num_bkg))

        if destfolder is not None:
            f = open(os.path.join(destfolder, "script.dat"), "wb")
        else:
            f = st_scr
        try:
            pe.write_script(f)
        finally:
            if destfolder is not None:
                f.close()
        print("SCRIPT.DAT saved: {} objects, {} scenes".\
            format(len(pe.objects), len(pe.scenes)))


    # =======================================================================
    # compile DIALOGUE.FIX
    # =======================================================================
    def compile_dialog(self, source, destfolder, enc = None,
            st_fix = None, st_lod = None):

        pe = petka.Engine()
        pe.init_empty("cp1251")

        mode = 0 # 0 - common, 1 - grp, 2 - act, 3 - dlg
                 # 10 - msg (autoreset to 0)
                 
        # used identificators
        self.usedid = {}
                   
        revOPS = {}
        for ok, ov in petka.OPCODES.items():
            revOPS[ov[0]] = ok
        revDLGOPS = {}
        for ok, ov in petka.DLGOPS.items():
            revDLGOPS[ov[0]] = ok
        self.reservedid = ["MSG", "DLG", "DLGGRP", "ON", "ENDDLG",
            "ENDDLGGRP", "ENDON"] + list(revDLGOPS.keys())

        # msg array
        compmsg = []
        # current msg
        compmsgitem = None
        # grp array
        compgrp = []
        # current grp
        compgrpitem = None
        # current act
        compactitem = None
        # current dlg
        compdlgitem = None
        # dlgop count
        compdlgops = 0

        for lineno, tokens in self.tokenizer(source, enc):
            if len(tokens) == 0:
                continue
            if mode == 0:
                # accept: MSG, DLGGRP
                cmd = tokens[0].upper()
                if cmd == "MSG":
                    if len(tokens) < 3 or len(tokens) > 6:
                        raise ScriptSyntaxError("Error at {}: unknown MSG "\
                            "syntax".format(lineno))
                    while len(tokens) < 6:
                        tokens.append("0")
                    # check ident
                    self.checkusedid(tokens[1], lineno)    
                    self.setidentvalue(tokens[1], len(compmsg))                
                    # check wavfile name (1..12)
                    if len(tokens[2]) < 1 or len(tokens[2]) > 12:
                        raise ScriptSyntaxError("Error at {}: bad filename "\
                            "in MSG \"{}\"".format(lineno, tokens[2]))
                    mode = 10
                    compmsgitem = {"ident": tokens[1], "wav": tokens[2],
                        "args": tokens[3:], "lineno": lineno}
                    compmsg.append(compmsgitem)
                elif cmd == "DLGGRP":
                    # dlggrp
                    mode = 1
                    # check syntax
                    if len(tokens) < 2 or len(tokens) > 3:
                        raise ScriptSyntaxError("Error at {}: unknown DLGGRP "\
                            "syntax".format(lineno))
                    while len(tokens) < 3:
                        tokens.append("0")
                    compgrpitem = {"grp_id": tokens[1], "arg": tokens[2],
                        "acts": [], "lineno": lineno}
                else:
                    raise ScriptSyntaxError("Error at {}: unknown syntax "\
                        "\"{}\"".format(lineno, cmd))
            elif mode == 10:
                # MSG, 2nd string, one token - msg for subtitle
                if len(tokens) != 1:
                    raise ScriptSyntaxError("Error at {}: MSG syntax error, "\
                        "message required".format(lineno))
                compmsgitem["msg"] = tokens[0]
                compmsgitem = None
                mode = 0
            elif mode == 1:
                # accept: ON, ENDDLGGRP
                cmd = tokens[0].upper()
                if cmd == "ON":
                    # on
                    mode = 2
                    # check syntax
                    if len(tokens) < 3 or len(tokens) > 5:
                        raise ScriptSyntaxError("Error at {}: unknown ON "\
                            "syntax".format(lineno))
                    while len(tokens) < 5:
                        tokens.append("0")
                    if tokens[1] in revOPS:
                        # opcode
                        don = revOPS[tokens[1]]
                    else:
                        don = self.convertnum(tokens[1])
                        if don is None:
                            raise ScriptSyntaxError("Error at {}: unknown ON "\
                                "OPREF ""\"{}\" in ON".\
                                format(lineno, tokens[1]))
                        don = self.check16(don, "ON opref", lineno)
                    compactitem = {"don": don, "donref": tokens[2], 
                        "args": tokens[3:], "dlgs": [], "lineno": lineno}
                elif cmd == "ENDDLGGRP" and len(tokens) == 1:
                    mode = 0
                    compgrp.append(compgrpitem)
                    compgrpitem = None
                else:
                    raise ScriptSyntaxError("Error at {}: unknown DLGGRP "\
                        "syntax \"{}\"".format(lineno, cmd))
            elif mode == 2:
                # accept: DLG, ENDON
                cmd = tokens[0].upper()
                if cmd == "DLG":
                    # dlg
                    mode = 3
                    # check syntax
                    if len(tokens) < 1 or len(tokens) > 3:
                        raise ScriptSyntaxError("Error at {}: unknown DLG "\
                            "syntax".format(lineno))
                    while len(tokens) < 3:
                        tokens.append("0")
                    compdlgitem = {"args": tokens[1:], "dlgops": [],
                        "lineno": lineno}
                elif cmd == "ENDON" and len(tokens) == 1:
                    mode = 1
                    compgrpitem["acts"].append(compactitem)
                    compactitem = None
                else:
                    raise ScriptSyntaxError("Error at {}: unknown ON "\
                        "syntax \"{}\"".format(lineno, cmd))
            elif mode == 3:
                # dlgopcode or ENDDLG
                cmd = tokens[0].upper()
                if cmd == "ENDDLG" and len(tokens) == 1:
                    compactitem["dlgs"].append(compdlgitem)
                    compdlgitem = None
                    mode = 2
                elif len(tokens) == 1 and cmd[-1:] == ":":
                    # label case
                    label = cmd[:-1]
                    self.checkusedid(tokens[0][:-1], lineno)
                    self.setidentvalue(tokens[0][:-1], compdlgops)
                else:
                    # check format
                    if len(tokens) < 1 or len(tokens) > 3:
                        raise ScriptSyntaxError("Error at {}: unknown DLGOP "\
                            "syntax in DLG".format(lineno))
                    while len(tokens) < 3:
                        tokens.append("0")
                    op = {}
                    if cmd in revDLGOPS:
                        # opcode
                        opcode = revDLGOPS[cmd]
                    else:
                        opcode = self.convertnum(cmd)
                        if opcode is None:
                            raise ScriptSyntaxError("Error at {}: unknown "\
                                "DLGOP \"{}\" in DLG".\
                                format(lineno, cmd))
                        opcode = self.check8(opcode, "dlgopcode", lineno)
                    compdlgops += 1
                    compdlgitem["dlgops"].append({"opcode": opcode,
                        "lineno": lineno, \
                        "msg_ref": tokens[2], "arg": tokens[1]})
            else:
                raise ScriptSyntaxError("Error at {}: unknown parser mode {}".\
                    format(lineno, mode))
            
        # check unclosed objects
        if mode != 0:
            raise ScriptSyntaxError("Error at {}: unfinished structure".\
                format(lineno))

        # second stage - MSG
        for msg in compmsg:
            # msg arguments
            fmt = []
            for i in range(3):
                fmt.append(("MSG argument {}".format(i + 1), \
                    self.check32, True))
            argnum = self.convertargs(fmt, msg["args"], msg["lineno"])
            # build MSGREC
            msgrec = petka.engine.MsgObject(
                len(pe.msgs), msg["wav"], argnum[0], argnum[1], argnum[2])
            msgrec.name = msg["msg"]
            pe.msgs.append(msgrec)

        if destfolder is not None:
            f = open(os.path.join(destfolder, "dialogue.lod"), "wb")
        else:
            f = st_lod
        try:
            pe.write_lod(f)
        finally:
            if destfolder is not None:
                f.close()
        print("DALOGUE.LOD saved: {} messages".\
            format(len(pe.msgs)))

        for grp in compgrp:
            fmt = [("DLGGRP number", self.check32, False),\
                ("DLGGRP argument", self.check32, True)]
            argnum = self.convertargs(fmt, [grp["grp_id"], grp["arg"]], \
                grp["lineno"])
            # build DLGGRP
            grprec = petka.engine.DlgGrpObject(argnum[0], argnum[1])
            grprec.acts = []
            for act in grp["acts"]:
                # act objref
                if act["donref"].upper() == "THIS":
                    donref = grprec.grp_id
                else:
                    donref = self.convertnum(act["donref"])
                if donref is not None:
                    # direct number
                    donref = self.check16(donref, "ON object ref", 
                        act["lineno"])
                else:
                    # get by ident
                    self.checkident(act["donref"], "ON object ref",
                        act["lineno"])
                    donref = self.getidentvalue(act["donref"])
                # act arguments
                fmt = []
                for i in range(2):
                    fmt.append(("ON argument {}".format(i + 1),
                        self.check32, True))
                argnum = self.convertargs(fmt, act["args"], act["lineno"])
                # build ON
                actrec = petka.engine.DlgActObject(act["don"], donref,
                    argnum[0], argnum[1])
                actrec.dlgs = []
                for dlg in act["dlgs"]:
                    # dlg arguments
                    fmt = []
                    for i in range(2):
                        fmt.append(("DLG argument {}".format(i + 1),
                            self.check32, True))
                    argnum = self.convertargs(fmt, dlg["args"], dlg["lineno"])
                    # build DLG
                    dlgrec = petka.engine.DlgObject(len(pe.dlgops), 
                        argnum[0], argnum[1])
                    for op in dlg["dlgops"]:
                        fmt = [("DLGOP argument", self.check8, False),
                            ("DLGOP ref", self.check16, True)]
                        argnum = self.convertargs(fmt, [op["arg"],
                            op["msg_ref"]], op["lineno"])
                        oprec = petka.engine.DlgOpObject(op["opcode"],
                            argnum[0], argnum[1])
                        pe.dlgops.append(oprec)
                    actrec.dlgs.append(dlgrec)
                grprec.acts.append(actrec)
            pe.dlgs.append(grprec)

        if destfolder is not None:
            f = open(os.path.join(destfolder, "dialogue.fix"), "wb")
        else:
            f = st_fix
        try:
            pe.write_fix(f)
        finally:
            if destfolder is not None:
                f.close()
        print("DALOGUE.FIX saved: {} groups, {} dialog opcodes".\
            format(len(pe.dlgs), len(pe.dlgops)))


    # =======================================================================
    # decompile utils
    # =======================================================================
    def fmtnum16(self, num):
        if num < 10:
            return "{}".format(num)
        elif num == 0xffff:
            return "-1"
        else:
            return "0x{:x}".format(num)

    def fmtnum32(self, num):
        if num < 10:
            return "{}".format(num)
        elif num == 0xffffffff:
            return "-1"
        else:
            return "0x{:x}".format(num)

    def fmtop(self, num):
        if num in petka.OPCODES:
            return petka.OPCODES[num][0]
        return "0x{:x}".format(num)

    def fmtdlgop(self, num):
        if num in petka.DLGOPS:
            return petka.DLGOPS[num][0]
        return "0x{:x}".format(num)

    def escstr(self, value):
        return value.replace("\\", "\\\\").replace("\"", "\\\"")

    # =======================================================================
    # decompile SCRIPT.DAT
    # =======================================================================
    def pretty_print_scr(self, scrname, stream, enc = None, decsort = False):
        def pprint(msg):
            if stream is None:
                print(msg)
            else:
                stream.write((msg + "\n").encode(enc or "UTF-8"))

        pe = petka.Engine()
        pe.init_empty("cp1251")
        bkgname = find_in_folder(os.path.dirname(scrname), "backgrnd.bg")
        resname = find_in_folder(os.path.dirname(scrname), "resource.qrc")
        pe.load_script(scrname, bkgname, resname)

        # define lists of used items
        used_obj = []
        used_res = []

        def fmtfor(num, objid):
            if num == objid:
                return "THIS"
            if num in pe.obj_idx:
                return "obj_{}".format(num)
            if num in pe.scn_idx:
                return "scene_{}".format(num)
            return self.fmtnum16(num)
        
        def printres(resid):
            pprint("RES res_{} 0x{:x} \"{}\"".format(resid, resid,
                self.escstr(pe.res[resid])))
            pprint("")

        def printrescheck(resid, opcode):
            tp = petka.OPCODES.get(opcode, ("", 0))[1]
            if tp == 1 and resid in pe.res:
                if resid in used_res:
                    return
                used_res.append(resid)
                printres(resid)

        def printitem(item, itemtype):
            pprint("{} {}_{} 0x{:x} \"{}\"".format(itemtype.upper(), itemtype,
                item.idx, item.idx, self.escstr(item.name)))
                
            # sub objects
            if itemtype == "scene":
                if len(item.refs) == 0:
                    pprint("  ZEROREF")
                for obj, a1, a2, a3, a4, a5 in item.refs:
                    if obj.idx in pe.obj_idx:
                        ref = "obj_{}".format(obj.idx)
                    elif obj.idx in pe.scn_idx:
                        ref = "scene_".format(obj.idx)
                    else:
                        pprint("  # unknown reference to 0x{:x}".format(
                           obj.idx))
                        ref = "0x{:x}".format(obj.idx)
                    pprint("  REF {} {} {} {} {} {}".format(ref, 
                        self.fmtnum32(a1), self.fmtnum32(a2), 
                        self.fmtnum32(a3), self.fmtnum32(a4), 
                        self.fmtnum32(a5)))
            
            for act in item.acts:
                actif = ""
                if act.act_status != 0xff or act.act_ref != 0xffff:
                    actif = " 0x{:02x} ".format(act.act_status)
                    if act.act_ref == item.idx:
                        actif += "THIS"
                    else:
                        actif += self.fmtnum16(act.act_ref)
                pprint("  ON {}{}".format(self.fmtop(act.act_op), actif))
                # list actions
                for op in act.ops:
                    if op.op_arg1 in pe.res and \
                            petka.OPCODES.get(op.op_code, ["", 0])[1] == 1:
                        res = "res_{}".format(op.op_arg1)
                    else:
                        res = self.fmtnum16(op.op_arg1)
                    pprint("    {} {} {} {} {}".format(self.fmtop(op.op_code),
                        fmtfor(op.op_ref, item.idx), res,
                        self.fmtnum16(op.op_arg2),
                        self.fmtnum16(op.op_arg3)))
                pprint("  ENDON")
        
            pprint("END{} # {}_{}".format(itemtype.upper(), itemtype, item.idx))
            pprint("")
            
        pprint("# Decompile SCRIPT \"{}\"".format(scrname))
        pprint("# Version: {}".format(VERSION))
        pprint("# Encoding: {}".format(enc))
        
        if decsort:
            for idx, scene in enumerate(pe.scenes):
                pprint("# Scene {} / {}".format(idx + 1, len(pe.scenes)))
                # display used objects
                if len(scene.idx.refs) > 0:
                    pprint("# referenced objects {}:".format(len(scene.refs)))
                    for ref in scene.refs:
                        if ref[0].idx in used_obj: 
                            pprint("# object 0x{:x} already defined".\
                                format(ref[0].idx))
                            continue
                        used_obj.append(ref[0].idx)
                        if ref[0].idx in pe.obj_idx:
                            for act in ref[0].acts_array:
                                for op in act.ops_array:
                                    printrescheck(op.op_arg1, op.op_code)                
                            printitem(obj, "obj")                
                else:        
                    pprint("# No referenced objects")
                # display res
                for act in scene.acts:
                    for op in act.ops:
                        printrescheck(op.op_arg1, op.op_code)                
                printitem(scene, "scene")
            # list unused
            msg = False
            for obj in pe.objects:
                if obj.idx in used_obj: continue
                if not msg:
                    pprint("# Note: Following objects not listed anywhere")
                    msg = True
                printitem(obj, "OBJ")
            msg = False
            for res in self.res:
                if res in used_res: continue
                if not msg:
                    pprint("# Note: Following resources not listed anywhere")
                    msg = True
                printres(res)
        else:
            for obj in pe.objects:
                printitem(obj, "obj")
            for scene in pe.scenes:
                printitem(scene, "scene")
            for res in pe.resord:
                printres(res)

    # =======================================================================
    # decompile DIALOGUE.FIX
    # =======================================================================
    def pretty_print_dlg(self, fixname, stream, enc = None, verbose = False):
        def pprint(msg):
            if stream is None:
                print(msg)
            else:
                stream.write((msg + "\n").encode(enc or "UTF-8"))

        pe = petka.Engine()
        pe.init_empty("cp1251")
        lodname = find_in_folder(os.path.dirname(fixname), "dialogue.lod")
        pe.load_dialogs(fixname, lodname, True)

        pprint("# Decompile DIALOGUE \"{}\"".format(fixname))
        pprint("# Version: {}".format(VERSION))
        pprint("# Encoding: {}".format(enc))

        for msg in pe.msgs:
            pprint("# {} = 0x{:x}".format(msg.idx, msg.idx))
            pprint("MSG msg_{} \"{}\" 0x{:x} 0x{:x} 0x{:x}".format(\
                msg.idx, msg.msg_wav, msg.msg_arg1, msg.msg_arg2, msg.msg_arg3))
            pprint(" \"{}\"".format(self.escstr(msg.name)))
            pprint("")
    
        for gidx, grp in enumerate(pe.dlgs, 1):
            pprint("# {} = 0x{:x}".format(gidx, gidx))
            pprint("DLGGRP 0x{:x} {}".format(grp.idx, \
                self.fmtnum32(grp.grp_arg1)))
            for sidx, act in enumerate(grp.acts, 1):
                pprint("  ON {} 0x{:x} 0x{:x} 0x{:x} # {}".format(\
                    self.fmtop(act.opcode), act.ref, act.arg1, 
                        act.arg2, sidx))
                # print code
                for didx, dlg in enumerate(act.dlgs, 1):
                    #print(bsrec)
                    pprint("    DLG 0x{:x} 0x{:x} # {}".format(\
                        dlg.arg1, dlg.arg2, didx))
                    # scan used addr
                    usedadr = []
                    for op in dlg.ops:
                        if op.opcode == 0x3 or \
                            op.opcode == 0x4: # GOTO or MENURET
                            if op.ref not in usedadr:
                                usedadr.append(op.ref)
                    if len(usedadr) > 0:
                        usedadr.append(dlg.op_start)
                    usedmenu = {}
                    usedcase = {}
                    for oidx, op in enumerate(dlg.ops):
                        cmt = ""
                        opref = "0x{:X}".format(op.ref)
                        opcode = self.fmtdlgop(op.opcode)
                        if op.pos in usedadr:
                             pprint("      label_{:X}:".format(
                                op.pos))
                        if op.opcode == 0x1: # BREAK
                            if op.pos in usedcase:
                                if len(usedadr) > 0:
                                    cmt = "# end select "\
                                        "label_{:X}, case=0x{:}"\
                                        "".format(*usedcase[op.pos])
                                else:
                                    cmt = "# end "\
                                        "select case=0x{:}".\
                                        format(usedcase[op.pos][1])
                        elif op.opcode == 0x2 or op.opcode == 0x8: # MENU or CIRCLE
                            cmt = "# select "
                            doarr = []
                            docurr = []
                            sellen = op.ref % 0x100
                            skiptobrk = False
                            menuactstart = None
                            for oidx2, op2 in enumerate(dlg.ops[oidx + 1:]):
                                if op2.opcode == 0x1: # BREAK
                                    usedcase[op2.pos] = (op.pos, len(doarr))
                                    doarr.append(docurr)
                                    skiptobrk = False
                                    if len(doarr) == sellen:
                                        if op.opcode == 0x2:
                                            menuactstart = oidx2 + oidx + 2
                                        break
                                    docurr = []
                                elif op2.opcode == 0x7 and not skiptobrk: # PLAY
                                    docurr.append("msg_{}".format(op2.ref))
                                else:
                                    docurr = ["complex"]
                            if len(doarr) < sellen:
                                cmt = "# {} select broken, "\
                                    "required={}, got={}".\
                                    format(opcode, sellen, len(doarr))
                            else:
                                cmt += ",".join(["+".join(x) for x in doarr])
                            if menuactstart is not None:
                                for oidx2, op2 in enumerate(dlg.ops[\
                                        menuactstart:menuactstart + sellen]):
                                    usedmenu[op2.pos] = (op.pos, oidx2)
                        elif op.opcode == 0x3 or \
                            op.opcode == 0x4: # GOTO or MENURET
                            opref = "label_{:X}".format(op.ref)
                            if op.pos in usedmenu:
                                cmt = "# action menu="\
                                    "label_{:X}, case=0x{:}".\
                                    format(*usedmenu[op.pos])
                        elif op.opcode == 0x7:
                            opcode = "PLAY"
                            if op.msg:
                                opref = "msg_{}".format(op.ref)
                                if op.ref < len(pe.msgs):
                                    cmt = "# {}".format(self.escstr(
                                        pe.msgs[op.ref].name))
                        oparg = " 0x{:X} ".format(op.arg)
                        if (op.opcode == 0x1 or op.opcode == 0x6) and \
                                op.arg == 0 and op.ref == 0:
                            oparg = ""
                            opref = ""
                        if cmt and verbose:
                            pprint("        " + cmt)
                        pprint("        {}{}{}".\
                            format(opcode, oparg, opref))
                    pprint("    ENDDLG # {}".format(didx))
                pprint("  ENDON # {}".format(sidx))
            pprint("ENDDLGGRP # {}".format(gidx))
            pprint("")


# check if file already exists and flag for overwrite not set
def ckeckoverwrite(fn, args):
    if os.path.exists(fn) and not args.fo:
        print("File \"{}\" already exists, use -fo to overwrite".\
            format(fn))
        return True
    return False

# check files are the same
def checksame(f1, n1, f2, n2):
    if os.path.abspath(f1) == os.path.abspath(f2):
        print("Error: {} and {} are the same \"{}\"".\
            format(f1))
        return True
    return False
       
       
def action_dec(args):
    print("Decompile SCRIPT.DAT file")
    destpath = args.destpath
    encoding = args.encoding
    
    if destpath:
        if ckeckoverwrite(destpath, args): return -1
        if checksame(args.sourcepath, "source", destpath, "destination"): 
            return -2
        if not encoding:
            encoding = "UTF-8"
        
    print("Input:\t{}".format(args.sourcepath))
    print("Output:\t{}".format(destpath or "-"))
    if destpath:
        print("Enc:\t{}".format(encoding))
    if args.decompile_sorted:
        print("Flag decompile_sorted enabled")
    
    dcs = P12Compiler()
    if destpath:
        f = open(destpath, "wb")
        try:
            dcs.pretty_print_scr(args.sourcepath, f, enc = encoding, \
                decsort = args.decompile_sorted)
        finally:
            f.close()
    else:
        dcs.pretty_print_scr(args.sourcepath, None)

def action_comp(args):
    print("Compile SCRIPT.DAT file")
    print("Input:\t{}".format(args.sourcepath))
    print("Output:\t{}".format(args.destfolder))
    print("Enc:\t{}".format(args.encoding or "UTF-8"))

    dcs = P12Compiler()
    if os.path.exists(args.destfolder) and not args.fo:
        cnt = 0
        lst = ["script.dat", "backgrnd.bg", "resource.qrc"]
        for item in lst:
            if find_in_folder(args.destfolder, item, False):
                print("Error: destination file \"{}\" already "\
                    "exists, use -fo to overwrite".format(item))
                return

    if not os.path.exists(args.destfolder):
        os.makedirs(args.destfolder)
    f = open(args.sourcepath, "rb")
    try:
        dcs.compile_script(f, args.destfolder, args.encoding)
    except ScriptSyntaxError as e:
        if args.trace_error:
            traceback.print_exc()
        print(e, file = sys.stderr)
    finally:
        f.close()

def action_decd(args):
    print("Decompile DIALOGUE.FIX file")
    destpath = args.destpath
    encoding = args.encoding
    
    if destpath:
        if ckeckoverwrite(destpath, args): return -1
        if checksame(args.sourcepath, "source", destpath, "destination"):
            return -2
        if not encoding:
            encoding = "UTF-8"
        
    print("Input:\t{}".format(args.sourcepath))
    print("Output:\t{}".format(destpath or "-"))
    if destpath:
        print("Enc:\t{}".format(encoding))
    if args.verbose:
        print("Flag verbose enabled")
    
    dcs = P12Compiler()
    if destpath:
        f = open(destpath, "wb")
        try:
            dcs.pretty_print_dlg(args.sourcepath, f, enc = encoding, \
                verbose = args.verbose)
        finally:
            f.close()
    else:
        dcs.pretty_print_dlg(args.sourcepath, None, None, \
                verbose = args.verbose)

def action_compd(args):
    print("Compile DIALOGUE.FIX file")
    print("Input:\t{}".format(args.sourcepath))
    print("Output:\t{}".format(args.destfolder))
    print("Enc:\t{}".format(args.encoding or "UTF-8"))

    dcs = P12Compiler()
    if os.path.exists(args.destfolder) and not args.fo:
        cnt = 0
        lst = ["dialogue.fix", "dialogue.lod"]
        for item in lst:
            if find_in_folder(args.destfolder, item, False):
                print("Error: destination file \"{}\" already "\
                    "exists, use -fo to overwrite".format(item))
                return

    if not os.path.exists(args.destfolder):
        os.makedirs(args.destfolder)
    f = open(args.sourcepath, "rb")
    try:
        dcs.compile_dialog(f, args.destfolder, args.encoding)
    except ScriptSyntaxError as e:
        if args.trace_error:
            traceback.print_exc()
        print(e, file = sys.stderr)
    finally:
        f.close()

def internaltest(folder):
    test_arr = [
      "p1demo",
      "p1-0", "p1-1", "p1-2", "p1-3",
      "p2-0", "p2-1", "p2-2"
    ]
    def compare(fn, mem):
        f = open(fn, "rb")
        hf = hashlib.md5()
        try:
            hf.update(f.read())
        finally:
            f.close()
        mem.seek(0)
        hm = hashlib.md5()
        hm.update(mem.read())
        return hm.hexdigest() == hf.hexdigest()
            
    for test in test_arr:
        print("=== Test: " + test + " ===")
        testbase = os.path.join(folder, test)
        path = find_in_folder(testbase, "script.dat")
        dcs = P12Compiler()
        mems = io.BytesIO()        
        dcs.pretty_print_scr(path, mems)
        print("Decompiled script:", mems.tell())
        # compile back
        memscr = io.BytesIO()
        membkg = io.BytesIO()
        memres = io.BytesIO()
        mems.seek(0)
        ndcs = P12Compiler()
        ndcs.compile_script(mems, None, None, \
            memscr, membkg, memres)
        # compare
        if not compare(path, memscr):
            print("SCRIPT.DAT - mismatch")
            break
        if not compare(find_in_folder(testbase, "backgrnd.bg"), membkg):
            print("BACKGRND.BG - mismatch")
            break

        # dialogue
        dcs = P12Compiler()
        path = find_in_folder(testbase, "dialogue.fix")
        if not os.path.exists(path):
            print("All ok - no dialogue")
            continue
            
        mems = io.BytesIO()        
        dcs.pretty_print_dlg(path, mems)
        print("Decompiled dialogue:", mems.tell())
        # compile back
        memfix = io.BytesIO()
        memlod = io.BytesIO()
        mems.seek(0)
        ndcs = P12Compiler()
        ndcs.compile_dialog(mems, None, None, \
            memfix, memlod)
        # compare
        if not compare(path, memfix):
            print("DIALOGUE.FIX - mismatch")
            break
        if not compare(find_in_folder(testbase, "dialogue.lod"), memlod):
            print("DIALOGUE.LOD - mismatch")
            break

        print("All ok")

def action_version(args):
    print("Version: " + VERSION)
    
def main():
    print(APPNAME + ", " + VERSION)
    print("\tRoman Kharin (romiq.kh@gmail.com)")
    if len(sys.argv) < 2:
        print("Use -h for help.")
        return
        
    if len(sys.argv) >= 3:
        if sys.argv[1] == "test":
            internaltest(sys.argv[2])
            return
    
    parser = argparse.ArgumentParser(epilog = \
        "For actions help try: <action> -h")
    subparsers = parser.add_subparsers(title = 'actions')
    
    # decompile - <script.dat> [[--enc <encoding>] -o <decompiled.txt>]
    parser_dec = subparsers.add_parser("decompile", aliases = ['d'], \
        help = "decompile script.dat")
    parser_dec.add_argument('-fo', action = 'store_true', \
        help = "force overwrite existing output file")
    parser_dec.add_argument('--decompile-sorted', action = 'store_true', \
        help = "display objects and scenes in sorted way (can change order)")
    parser_dec.add_argument('-o', action = 'store', dest = "destpath",\
        help = "output path for decompiled (default: stdout)")
    parser_dec.add_argument('-e', "--enc", action = 'store', dest = "encoding",\
        help = "output encoding (default: UTF-8)")
    parser_dec.add_argument('sourcepath', help = "path to SCRIPT.DAT file")
    parser_dec.set_defaults(func = action_dec)

    # compile - <source.txt> <destination folder> [--enc <encoding>]
    parser_comp = subparsers.add_parser("compile", aliases = ['c'], \
        help = "compile script.dat")
    parser_comp.add_argument('-fo', action = 'store_true', \
        help = "force overwrite existing output files")
    parser_comp.add_argument('-e', "--enc", action = 'store', \
        dest = "encoding", help = "output encoding (default: UTF-8)")
    parser_comp.add_argument('-te', "--trace-error", action = 'store_true', \
        help = "trace syntax error")
    parser_comp.add_argument('sourcepath', help = "path to SOURCE.TXT file")
    parser_comp.add_argument('destfolder', help = "path to output folder")
    parser_comp.set_defaults(func = action_comp)

    # decompiledialog - <dialogue.fix> [[--enc <encoding>] -o <decompiled.txt>]
    parser_decd = subparsers.add_parser("decompiledialog", aliases = ['dd'], \
        help = "decompile dialogue.fix")
    parser_decd.add_argument('-fo', action = 'store_true', \
        help = "force overwrite existing output file")
    parser_decd.add_argument("-v", '--verbose', action = 'store_true', \
        help = "enable more verbose comments")
    parser_decd.add_argument('-o', action = 'store', dest = "destpath",\
        help = "output path for decompiled (default: stdout)")
    parser_decd.add_argument('-e', "--enc", action = 'store', \
        dest = "encoding", help = "output encoding (default: UTF-8)")
    parser_decd.add_argument('sourcepath', help = "path to DIALOGUE.FIX file")
    parser_decd.set_defaults(func = action_decd)

    # compiledialog - <source.txt> <destination folder> [--enc <encoding>]
    parser_compd = subparsers.add_parser("compiledialog", aliases = ['cd'], \
        help = "compile dialogue.fix")
    parser_compd.add_argument('-fo', action = 'store_true', \
        help = "force overwrite existing output files")
    parser_compd.add_argument('-e', "--enc", action = 'store', \
        dest = "encoding", help = "output encoding (default: UTF-8)")
    parser_compd.add_argument('-te', "--trace-error", action = 'store_true', \
        help = "trace syntax error")
    parser_compd.add_argument('sourcepath', help = "path to SOURCE.TXT file")
    parser_compd.add_argument('destfolder', help = "path to output folder")
    parser_compd.set_defaults(func = action_compd)

    # version
    parser_version = subparsers.add_parser("version", help = "program version")
    parser_version.set_defaults(func = action_version)

    args = parser.parse_args()
    args.func(args)

if __name__ == "__main__":
    main()
