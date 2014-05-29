#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import os, sys
import traceback
import argparse
import io
import hashlib

import petka

APPNAME = "P1&2 Compiler and decompiler"
VERSION = "v0.3 2014-06-01"

def find_in_folder(folder, name, ifnot = True):
    for item in os.listdir(folder):
        if item.upper() == name.upper():
            return os.path.join(folder, item)
    if ifnot:
        return os.path.join(folder, name)
    else:
        return None

# ===========================================================================
# decompile utils
# ===========================================================================
def fmtnum16(num):
    if num < 10:
        return "{}".format(num)
    elif num == 0xffff:
        return "-1"
    else:
        return "0x{:x}".format(num)

def fmtnum32(num):
    if num < 10:
        return "{}".format(num)
    elif num == 0xffffffff:
        return "-1"
    else:
        return "0x{:x}".format(num)

def fmtop(num):
    if num in petka.OPCODES:
        return petka.OPCODES[num][0]
    return "0x{:x}".format(num)

def fmtdlgop(num):
    if num in petka.DLGOPS:
        return petka.DLGOPS[num][0]
    return "0x{:x}".format(num)

def escstr(value):
    return value.replace("\\", "\\\\").replace("\"", "\\\"")

# ===========================================================================
# decompile SCRIPT.DAT
# ===========================================================================
def pretty_print_scr(pe, name, stream, enc = None, decsort = False):
    def pprint(msg):
        if stream is None:
            print(msg)
        else:
            stream.write((msg + "\n").encode(enc or "UTF-8"))

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
        return fmtnum16(num)
    
    def printres(resid):
        pprint("RES res_{} 0x{:x} \"{}\"".format(resid, resid,
            escstr(pe.res[resid])))
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
            item.idx, item.idx, escstr(item.name)))
            
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
                pprint("  REF {} {} {} {} {} {}".format(ref, fmtnum32(a1), 
                    fmtnum32(a2), fmtnum32(a3),
                    fmtnum32(a4), fmtnum32(a5)))
        
        for act in item.acts:
            actif = ""
            if act.act_status != 0xff or act.act_ref != 0xffff:
                actif = " 0x{:02x} ".format(act.act_status)
                if act.act_ref == item.idx:
                    actif += "THIS"
                else:
                    actif += fmtnum16(act.act_ref)
            pprint("  ON {}{}".format(fmtop(act.act_op), actif))
            # list actions
            for op in act.ops:
                if op.op_arg1 in pe.res and \
                        petka.OPCODES.get(op.op_code, ["", 0])[1] == 1:
                    res = "res_{}".format(op.op_arg1)
                else:
                    res = fmtnum16(op.op_arg1)
                pprint("    {} {} {} {} {}".format(fmtop(op.op_code),
                    fmtfor(op.op_ref, item.idx), res,
                    fmtnum16(op.op_arg2),
                    fmtnum16(op.op_arg3)))
            pprint("  ENDON")
    
        pprint("END{} # {}_{}".format(itemtype.upper(), itemtype, item.idx))
        pprint("")
        
    pprint("# Decompile SCRIPT \"{}\"".format(name))
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
    
    pe = petka.Engine()
    pe.init_empty("cp1251")
    bkgname = find_in_folder(os.path.dirname(args.sourcepath), "backgrnd.bg")
    resname = find_in_folder(os.path.dirname(args.sourcepath), "resource.qrc")
    pe.load_script(args.sourcepath, bkgname, resname)
    if destpath:
        f = open(destpath, "wb")
        try:
            pretty_print_scr(pe, args.sourcepath, f, enc = encoding, \
                decsort = args.decompile_sorted)
        finally:
            f.close()
    else:
        pretty_print_scr(pe, args.sourcepath, None)

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

    # version
    parser_version = subparsers.add_parser("version", help = "program version")
    parser_version.set_defaults(func = action_version)

    args = parser.parse_args()
    args.func(args)

if __name__ == "__main__":
    main()
