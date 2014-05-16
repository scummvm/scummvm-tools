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
        
    def load_data(self, f):
        self.image = Image.open(f)
            
