# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import os
import struct

# manage files data
class FileManager:
    def __init__(self, root):
        self.root = root
    
        self.strfd = []
        self.strtable = {}
    
    def find_path(self, path):
        # search case insensive from root
        dpath = []
        npath = self.root
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
        
        
    
    def load_store(self, name):
        path = self.find_path(name)
        if path is None:
            print("DEBUG: Store \"{}\" not found".format(name))
            return
        # scan table
        f = open(path, "rb")
        # check magic string "StOR"
        magic = f.read(4)
        if magic != b"StOR":
            print("DEBUG: Bad magic in \"{}\"".format(name))
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
        data = f.read().decode("ascii")
        for idx, fname in enumerate(data.split("\x00")):
            if idx < index_len and fname not in self.strtable:
                self.strtable[fname] = (len(self.strfd), ) + \
                    tuple(index_table[idx])
        # add file descriptor
        self.strfd.append((f, name))
        
        
    def unload_stores(self):
        for fd, name in self.strfd:
            try:
                if fd: fd.close()
            except Exception as e:
                print("DEBUG: Can't unload \"{}\":".format(name) + str(e))

