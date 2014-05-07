# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import os
import struct

from . import FileManager

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
        for line in f.readlines():
            line = line.decode(self.enc).strip()
            if len(line) == 0: continue
            if line[:1] == ";": continue
            if line[:1] == "[" and line[-1:] == "]":
                curr_sect = line[1:-1].strip()
                ini[curr_sect] = {}
                continue
            kv = line.split("=", 1)
            if len(kv) != 2: continue
            ini[curr_sect][kv[0].strip()] = kv[1].strip()
        return ini
        
    def load_data(self, folder, enc):
        self.fman = FileManager(folder)
        self.enc = enc
        # load PARTS.INI
        pf = self.fman.find_path("parts.ini")
        f = open(pf, "rb")
        try:
            self.parts_ini = self.parse_ini(f)
        finally:
            f.close()
        if "All" in self.parts_ini:
            if "Part" in self.parts_ini["All"]:
                self.start_part = int(self.parts_ini["All"]["Part"])
            if "Chapter" in self.parts_ini["All"]:
                self.start_chap = int(self.parts_ini["All"]["Chapter"])
        for sect, data in self.parts_ini.items():
            if sect == "All":
                if "Part" in data:
                    self.start_part = int(data["Part"]) - 1
                if "Chapter" in data:
                    self.start_chap = int(data["Chapter"]) - 1
            elif sect[:5] == "Part ":
                self.parts.append(data)
        # load BGS.INI
        pf = self.fman.find_path("bgs.ini")
        f = open(pf, "rb")
        try:
            self.bgs_ini = self.parse_ini(f)
        finally:
            f.close()
        if "Settings" in self.bgs_ini:
            if "StartRoom" in self.bgs_ini["Settings"]:
                self.start_scene = self.bgs_ini["Settings"]["StartRoom"]

    def open_part(self, part, chap):
        pname = "Part {}".format(part)
        pcname = pname
        if chap:
            pcname += " Chapter {}".format(chap)
        self.curr_path = self.parts_ini[pname]["CurrentPath"]
        self.curr_speech = self.parts_ini[pname]["PathSpeech"]
        self.curr_diskid = self.parts_ini[pname]["DiskID"]
        
        
