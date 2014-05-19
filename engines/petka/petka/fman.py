# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import os
import struct
import io

from . import EngineError

# manage files data
class FileManager:
    def __init__(self, root):
        self.root = os.path.abspath(root)
    
        self.strfd = []
        self.strtable = {}
    
    def find_path(self, path):
        # search case insensive from root
        dpath = []
        npath = self.root
        path = path.replace("\\", "/")
        for item in path.split("/"):
            if not item: continue
            ok = False
            for ritem in os.listdir(npath):
                if item.lower() != ritem.lower(): continue
                npath = os.path.join(npath, ritem)
                ok = True
                break
            if not ok: return None
        return npath
    
    def load_store(self, name, tag = 0):
        path = self.find_path(name)
        if path is None:
            print("DEBUG: Store \"{}\" not found".format(name))
            return
        # scan table
        f = open(path, "rb")
        # check magic string "StOR"
        magic = f.read(4)
        if magic != b"StOR":
            raise EngineError("Bad magic in store \"{}\"".format(name))
            return
        # read index table ref
        temp = f.read(4)
        index_ref = struct.unpack_from("<I", temp)[0]
        f.seek(index_ref)
        # index table length
        temp = f.read(4)
        index_len = struct.unpack_from("<I", temp)[0]
        index_table = []
        for iref in range(index_len):
            temp = f.read(12) 
            data = struct.unpack_from("<III", temp)
            index_table.append((data[1], data[2]))
        data = f.read().decode("latin-1")
        for idx, fname in enumerate(data.split("\x00")):
            fname = fname.lower().replace("\\", "/")
            if idx < index_len and fname not in self.strtable:
                self.strtable[fname] = (len(self.strfd),) + index_table[idx]
            else:
                if len(fname) > 0:
                    print("DEBUG:Extra file record \"{}\" in \"{}\"".\
                        format(fname, name))
        # add file descriptor
        self.strfd.append((f, name, tag))
        print("DEBUG: Loaded store \"{}\"".format(name))
        
    def read_file(self, fname):
        sf = fname.lower().replace("\\", "/")
        if sf in self.strtable:
            fnum, st, ln = self.strtable[sf]
            print("Load file \"{}\" from store \"{}\"".\
                format(fname, self.strfd[fnum][1]))
            self.strfd[fnum][0].seek(st)
            return self.strfd[fnum][0].read(ln)
        else:
            print("Load file \"{}\" from filesystem".format(fname))
            pf = self.find_path(fname)
            if not pf:
                print("DEBUG: Can't open file \"{}\"".format(fname))
            # file in filesystem
            f = open(pf, "rb")
            try:
                data = f.read()
            finally:
                f.close()
            return data
            
    def read_file_stream(self, fname):
        data = self.read_file(fname)
        mems = io.BytesIO()
        mems.write(data)
        mems.seek(0)
        return mems        
            
    def exists(self, fname):
        sf = fname.lower().replace("\\", "/")
        if sf in self.strtable:
            return True
        else:
            return self.find_path(fname) is not None       
        
    def unload_stores(self, flt = None):
        strfd = []
        strtable = {}
        for idx, (fd, name, tag) in enumerate(self.strfd):
            if flt is not None:
                if tag != flt:
                    for k, v in self.strtable.items():
                        if v[0] == idx:
                            strtable[k] = (len(strfd), v[1], v[2])
                    strfd.append((fd, name, tag))
                    continue
            print("DEBUG: Unload store \"{}\"".format(name))
            try:
                if fd: fd.close()
            except Exception as e:
                print("DEBUG: Can't unload \"{}\":".format(name) + str(e))
        self.strfd = strfd
        self.strtable = strtable
        
