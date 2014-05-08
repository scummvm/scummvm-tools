#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import sys, os
import tkinter
from tkinter import ttk, font

import petka

APPNAME = "P1&2 Explorer"

class App(tkinter.Frame):
    def __init__(self, master):
        tkinter.Frame.__init__(self, master)
        master.title(APPNAME)
        self.pack(fill = tkinter.BOTH, expand = 1)
        #self.createWidgets()
        #self.createMenu()
        self.pad = None
        self.sim = None
        self.curr_mode = 0 
        self.curr_width = 0
        self.curr_height = 0
        self.last_width = 1
        self.last_height = 1
        self.need_update = False
        self.main_image = tkinter.PhotoImage(width = 1, height = 1)
        self.after_idle(self.on_first_display)
        
    def create_widgets(self):
        
        ttk.Style().configure("Tool.TButton", width = -1) # minimal width
        ttk.Style().configure("TLabel", padding = self.pad)

        # canvas
        self.frm_view = ttk.Frame(self)
        self.frm_view.pack(side = tkinter.TOP, expand = 1, fill = tkinter.BOTH)
        self.frm_view.grid_rowconfigure(0, weight = 1)
        self.frm_view.grid_columnconfigure(0, weight = 1)
        self.scr_view_x = ttk.Scrollbar(self.frm_view, 
            orient = tkinter.HORIZONTAL)
        self.scr_view_x.grid(row = 1, column = 0, \
            sticky = tkinter.E + tkinter.W)
        self.scr_view_y = ttk.Scrollbar(self.frm_view)
        self.scr_view_y.grid(row = 0, column = 1, sticky = \
            tkinter.N + tkinter.S)
        self.canv_view = tkinter.Canvas(self.frm_view, height = 150, 
            scrollregion = (0, 0, 50, 50),
            xscrollcommand = self.scr_view_x.set,
            yscrollcommand = self.scr_view_y.set)
        self.canv_view_w = 0
        self.canv_view_h = 0
        self.canv_view_fact = 1
        self.canv_view.grid(row = 0, column = 0, \
            sticky = tkinter.N + tkinter.S + tkinter.E + tkinter.W)
        self.scr_view_x.config(command = self.canv_view.xview)
        self.scr_view_y.config(command = self.canv_view.yview)
        # don't forget
        #   canvas.config(scrollregion=(left, top, right, bottom))
        self.canv_view.bind('<Configure>', self.on_resize_view)
        self.canv_view.bind('<ButtonPress-1>', self.on_mouse_view)

        self.update_after()
        self.update_gui(None)

    def create_menu(self):
        self.menubar = tkinter.Menu(self.master)
        self.master.configure(menu = self.menubar)

        self.menufile = tkinter.Menu(self.master, tearoff = 0)
        self.menubar.add_cascade(menu = self.menufile,
                label = "File")
        self.menufile.add_command(
                command = self.on_open_data,
                label = "Open data...")
        self.menufile.add_separator()
        self.menufile.add_command(
                command = self.on_exit,
                label = "Quit")    

        self.menuedit = tkinter.Menu(self.master, tearoff = 0)
        self.menubar.add_cascade(menu = self.menuedit,
                label = "Edit")
        self.menuedit.add_command(
                command = self.on_select_chapter,
                label = "Select chapter")

    def update_after(self):
        if not self.need_update:
            self.after_idle(self.on_idle)
            self.need_update = True

    def on_idle(self):
        self.need_update = False
        self.update_canvas()

    def on_first_display(self):
        fnt = font.Font()
        try:
            self.pad = fnt.measure(":")
        except:
            self.pad = 5
        self.create_widgets()
        self.create_menu()

    def on_exit(self):
        self.master.destroy()

    def on_mouse_view(self, event):
        #self.currMode += 1
        #if self.currMode > 1:
        #    self.currMode = 0
        self.last_width = -1
        self.last_height = -1
        self.update_after()
        
    def on_resize_view(self, event):
        self.canv_view_w = event.width
        self.canv_view_h = event.height
        self.update_after()

    def update_canvas(self):
        # rebuild image
        c = self.canv_view
        c.delete(tkinter.ALL)

        if self.sim is None: return
                    
        #if (self.last_width != self.curr_width) or \
        #   (self.last_height != self.curr_height):
        #       self.build_image()
        
        # Preview image        
        #print("Update %d x %d" % (self.currWidth, self.currHeight))
        self.canv_image = self.main_image.copy()
        w = self.canv_view_w
        h = self.canv_view_h
        if (w == 0) and (h == 0): 
            return
        
        scale = 0 #self.RadioGroupScale.get()
        if scale == 0: # Fit
            try:
                psc = w / h
                isc = self.curr_width / self.curr_height
                if psc < isc:
                    if w > self.curr_width:
                        fact = w // self.curr_width
                    else:
                        fact = -self.curr_width // w
                else:
                    if h > self.curr_height:
                        fact = h // self.curr_height
                    else:
                        fact = -self.curr_height // h
            except:
                fact = 1
        else:
            fact = scale

        # place on canvas
        if fact > 0:
            pw = self.curr_width * fact
            ph = self.curr_height * fact
        else:
            pw = self.curr_width // -fact
            ph = self.curr_height // -fact

        cw = max(pw, w)
        ch = max(ph, h)
    
        c.config(scrollregion = (0, 0, cw - 2, ch - 2))
    
        if fact > 0:
            self.canv_image = self.canv_image.zoom(fact)
        else:
            self.canv_image = self.canv_image.subsample(-fact)
        self.canv_image_fact = fact
        #print("Place c %d %d, p %d %d" % (cw, ch, w, h))
        c.create_image(cw // 2, ch // 2, image = self.canv_image)

    def build_image(self):
        # rebuild main_image
        width = self.curr_width
        height = self.curr_height
        self.last_width = width
        self.last_height = height

        return
        
    def make_image(self, width, height, data):
        # create P6
        phdr = ("P6\n{} {}\n255\n".format(width, height))
        rawlen = width * height * 3 # RGB
        #phdr = ("P5\n{} {}\n255\n".format(width, height))
        #rawlen = width * height
        phdr = phdr.encode("UTF-8")

        if len(data) > rawlen:
            # truncate
            pdata = data[:rawlen]
        if len(data) < rawlen:
            # fill gap
            gap = bytearray()
            data += b"\xff" * (rawlen - len(data))
        p = bytearray(phdr)
        # fix UTF-8 issue
        for ch in data:
            if ch > 0x7f:
                p += bytes((0b11000000 |\
                    ch >> 6, 0b10000000 |\
                    (ch & 0b00111111)))               
            else:
                p += bytes((ch,))
        image = tkinter.PhotoImage(width = width, height = height, \
            data = bytes(p))
        return image                

    def update_gui(self, fn):
        pass

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
        # load static image
        #for item in self.sim.fman.strtable:
        #    print(item)
        bmpdata = self.sim.fman.read_file("MAIN/INTRFACE.BG/INSTHERO.BMP")
        bmp = petka.BMPLoader()
        bmp.load_data(bmpdata)
        self.main_image = self.make_image(640, 480, bmp.rgb)
        self.curr_width = 640
        self.curr_height = 480
        #self.sim.open_part(1, 0)
        #self.sim.open_part(2, 0)
        #self.sim.open_part(3, 0)

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
