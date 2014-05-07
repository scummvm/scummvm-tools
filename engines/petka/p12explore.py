#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import sys, os
import tkinter
from tkinter import ttk

import petka

APPNAME = "P1&2 Explorer"

class App(tkinter.Frame):
    def __init__(self, master):
        tkinter.Frame.__init__(self, master)
        master.title(APPNAME)
        self.pack(fill = tkinter.BOTH, expand = 1)
        self.createWidgets()
        self.createMenu()
        
        self.sim = None
        
    def createWidgets(self):
        
        ttk.Style().configure("Tool.TButton", width = -1) # minimal width
        #ttk.Style().configure("TLabel", padding = PADDING)


    def createMenu(self):
        self.menubar = tkinter.Menu(self.master)
        self.master.configure(menu = self.menubar)

        self.menufile = tkinter.Menu(self.master, tearoff = 0)
        self.menubar.add_cascade(menu = self.menufile,
                label="File")
        self.menufile.add_command(
                command = self.on_open_data,
                label="Open data...")
        self.menufile.add_separator()
        self.menufile.add_command(
                command = self.on_exit,
                label="Quit")    

        self.menuedit = tkinter.Menu(self.master, tearoff = 0)
        self.menubar.add_cascade(menu = self.menuedit,
                label="Edit")
        self.menuedit.add_command(
                command = self.on_select_chapter,
                label="Select chapter")

    def on_exit(self):
        self.master.destroy()

    def on_open_data(self):
        # open data - select TODO
        pass
        
    def on_select_chapter(self):
        # TODO
        pass        
        
    def open_data_from(self, folder):
        self.sim = petka.Engine()
        self.sim.load_data(folder, "cp1251")
        self.sim.open_part(0, 0)

def main():
    root = tkinter.Tk()
    app = App(master = root)
    if len(sys.argv) > 1:
        fn = sys.argv[1]
    else:
        fn = "."
    app.open_data_from(fn)
    
    app.mainloop()
    #fman = petka.FileManager(".")
    #fman.load_store("patch.str")
    #fman.load_store("main.str")
    #for k, v in fman.strtable.items():
    #    print(k, "=", v)
    # cleanup
    #fman.unload_stores()
    
if __name__ == "__main__":
    main()
