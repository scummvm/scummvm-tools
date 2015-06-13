#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2015

import sys, os
import traceback
import tkinter

from tkguibrowser import TkBrowser, hlesc, cesc, fmt_hl, fmt_hl_len, fmt_arg, \
    fmt_dec, fmt_dec_len

APPNAME = "Test tkinter gui browser"
VERSION = "v0.1 2015-06-20"

class App(TkBrowser):
    def __init__(self, master):
        super().__init__(master)            
                
    def init_gui(self):
        self.master.title(APPNAME)
        # path
        if hasattr(sys, 'frozen'):
            self.app_path = sys.executable
        else:
            self.app_path = __file__
        self.app_path = os.path.abspath(os.path.dirname(self.app_path))
                
    def create_widgets(self):
        super().create_widgets()
        self.open_path("/")


    def create_menu(self):
        super().create_menu()
        self.menufile = tkinter.Menu(self.master, tearoff = 0)
        self.menubar.add_cascade(menu = self.menufile,
                label = "File")
        self.menufile.add_separator()
        self.menufile.add_command(
                command = self.on_exit,
                label = "Quit")    

def main():
    root = tkinter.Tk()
    app = App(master = root)
    argv = sys.argv[1:]
    while len(argv) > 0:
        if argv[0] == "-d": # open data
            app.start_act.append(["load", argv[1]])
            argv = argv[2:]
        elif argv[0] == "-s": # open str file
            app.start_act.append(["str", argv[1]])
            argv = argv[2:]
        elif argv[0] == "-sd": # open savex.dat file
            app.start_act.append(["savedat", argv[1]])
            argv = argv[2:]
        elif argv[0] == "-t": # open translation
            app.start_act.append(["tran", argv[1]])
            argv = argv[2:]
        else:
            app.start_act.append(["open", argv[0]])
            argv = argv[1:]
    app.mainloop()

    
if __name__ == "__main__":
    main()
