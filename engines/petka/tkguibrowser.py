#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2015

import math
import traceback

import tkinter
from tkinter import ttk, font, filedialog, messagebox
from idlelib.WidgetRedirector import WidgetRedirector

# Image processing
try:
    from PIL import Image
except ImportError:
    Image = None

try:
    from PIL import ImageTk
except ImportError:
    ImageTk = None

def hlesc(value):
    if value is None:
        return "None"
    return value.replace("\\", "\\\\").replace("<", "\\<").replace(">", "\\>")

def cesc(value):
    return value.replace("\\", "\\\\").replace("\"", "\\\"")

def fmt_hl(loc, desc):
    return "<a href=\"{}\">{}</a>".format(loc, desc)

def fmt_hl_len(loc, desc, ln):
    sz = max(ln - len(desc), 0)
    return " "*sz + fmt_hl(loc, desc)

def fmt_arg(value):
    if value < 10:
        return "{}".format(value)
    elif value == 0xffff:
        return "-1"
    else:
        return "0x{:X}".format(value)
    
def fmt_dec(value, add = 0):
    return "{{:{}}}".format(fmt_dec_len(value, add))
        
def fmt_dec_len(value, add = 0):
    if value == 0:
        d = 1
    else:
        d = int(math.log10(value)) + 1
    d += add
    return d

# thanx to http://effbot.org/zone/tkinter-text-hyperlink.htm
class HyperlinkManager:

    def __init__(self, text):
        self.text = text
        self.text.tag_config("hyper", foreground = "blue", underline = 1)
        self.text.tag_bind("hyper", "<Enter>", self._enter)
        self.text.tag_bind("hyper", "<Leave>", self._leave)
        self.text.tag_bind("hyper", "<Button-1>", self._click)
        bold_font = font.Font(text, self.text.cget("font"))
        bold_font.configure(weight = "bold")
        self.text.tag_config("bold", font = bold_font)
        italic_font = font.Font(text, self.text.cget("font"))
        italic_font.configure(slant = "italic")
        self.text.tag_config("italic", font = italic_font)
        self.text.tag_config("underline", underline = 1)
        self.reset()

    def reset(self):
    	self.links = {}
    	self.colors = []
    	self.bgs = []

    def add(self, action):
        # add an action to the manager.  returns tags to use in
        # associated text widget
        tag = "hyper-{}".format(len(self.links))
        self.links[tag] = action
        return "hyper", tag

    def color(self, color):
        tag = "color-{}".format(color)
        if tag not in self.colors:
            self.colors.append(tag)
            self.text.tag_config(tag, foreground = color)
            self.text.tag_raise("hyper")
        return (tag,)

    def bg(self, color):
        tag = "bg-{}".format(color)
        if tag not in self.bgs:
            self.bgs.append(tag)
            self.text.tag_config(tag, background = color)
            self.text.tag_raise("hyper")
        return (tag,)

    def _enter(self, event):
        self.text.config(cursor = "hand2")

    def _leave(self, event):
        self.text.config(cursor = "")

    def _click(self, event):
        for tag in self.text.tag_names(tkinter.CURRENT):
            if tag[:6] == "hyper-":
                self.links[tag]()
                return

    def add_markup(self, text, widget, handler):
        mode = 0 # 0 - normal, 1 - tag
        curr_tag = None
        curr_text = ""
        tags = []
        esc = False
        for ch in text:
            if mode == 0:
                if esc:
                    curr_text += ch
                    esc = False
                else:
                    if ch == "\\":
                        esc = True
                    elif ch == "<":
                        mode = 1
                        curr_tag = ""
                    elif ch == "\n":
                        curr_text += ch
                        pass
                    else:
                        curr_text += ch
            else:
                if ch == ">":
                    if len(curr_text) > 0:                    
                        widget.insert(tkinter.INSERT, curr_text, \
                            tuple(reversed([x for x in tags for x in x])))
                    if curr_tag[:7] == "a href=":
                        ref = curr_tag[7:]
                        if ref[:1] == "\"":
                            ref = ref[1:]
                        if ref[-1:] == "\"":
                            ref = ref[:-1]
                        tags.append(self.add(handler(ref)))
                    elif curr_tag[:11] == "font color=":
                        ref = curr_tag[11:]
                        if ref[:1] == "\"":
                            ref = ref[1:]
                        if ref[-1:] == "\"":
                            ref = ref[:-1]
                        tags.append(self.color(ref))
                    elif curr_tag[:8] == "font bg=":
                        ref = curr_tag[8:]
                        if ref[:1] == "\"":
                            ref = ref[1:]
                        if ref[-1:] == "\"":
                            ref = ref[:-1]
                        tags.append(self.bg(ref))
                    elif curr_tag == "b":
                        tags.append(["bold"])
                    elif curr_tag == "i":
                        tags.append(["italic"])
                    elif curr_tag == "u":
                        tags.append(["underline"])
                    elif curr_tag[:1] == "/":
                        tags = tags[:-1]
                    curr_text = ""
                    mode = 0
                else:
                    curr_tag += ch
        if len(curr_text) > 0: 
            widget.insert(tkinter.INSERT, curr_text, \
                tuple(reversed([x for x in tags for x in x])))
    

# thanx http://tkinter.unpythonic.net/wiki/ReadOnlyText
class ReadOnlyText(tkinter.Text):

    def __init__(self, *args, **kwargs):
        tkinter.Text.__init__(self, *args, **kwargs)
        self.redirector = WidgetRedirector(self)
        self.insert = \
            self.redirector.register("insert", lambda *args, **kw: "break")
        self.delete = \
            self.redirector.register("delete", lambda *args, **kw: "break")


class TkBrowser(tkinter.Frame):

    def __init__(self, master):
        tkinter.Frame.__init__(self, master)
        self.pack(fill = tkinter.BOTH, expand = 1)        
        self.pad = None

        # gui
        self.path_handler = {}
        self.curr_main = -1 # 0 - frame, 1 - canvas
        self.curr_path = []
        self.curr_help = ""
        self.last_path = [None]
        self.curr_gui = []
        self.curr_state = {} # local state for location group
        self.curr_markup = "" # current unparsed markup data (unclosed tags, etc)
        self.curr_lb_acts = None
        self.curr_lb_idx = None
        self.hist = []
        self.histf = []
        self.gl_state = {} # global state until program exit
        self.start_act = []
        self.init_gui() # init custom gui data

        # canvas
        self.need_update = False
        self.canv_view_fact = 1
        self.main_image = tkinter.PhotoImage(width = 1, height = 1)
        # add on_load handler
        self.after_idle(self.on_first_display)
    
    def init_gui(self):
        pass    

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

    def on_help(self):
        pass

    def on_back(self):
        if len(self.hist) > 1:
            np = self.hist[-2:-1][0]
            self.histf = self.hist[-1:] + self.histf
            self.hist = self.hist[:-1]
            self.open_path(np[0], False)

    def on_forward(self):
        if len(self.histf) > 0:
            np = self.histf[0]
            self.histf = self.histf[1:]
            self.hist.append(np)
            self.open_path(np[0], False)

    def create_widgets(self):
        ttk.Style().configure("Tool.TButton", width = -1) # minimal width
        ttk.Style().configure("TLabel", padding = self.pad)
        ttk.Style().configure('Info.TFrame', background = 'white', \
            foreground = "black")

        # toolbar
        self.toolbar = ttk.Frame(self)
        self.toolbar.pack(fill = tkinter.BOTH)
        btns = [
            ["Outline", lambda: self.open_path("")],
            ["Help", self.on_help],
            [None, None],
            ["<-", self.on_back],
            ["->", self.on_forward],
        ]
        for text, cmd in btns:
            if text is None:
                frm = ttk.Frame(self.toolbar, width = self.pad, 
                    height = self.pad)
                frm.pack(side = tkinter.LEFT)
                continue            
            btn = ttk.Button(self.toolbar, text = text, \
                style = "Tool.TButton", command = cmd)
            btn.pack(side = tkinter.LEFT)
        frm = ttk.Frame(self.toolbar, width = self.pad, height = self.pad)
        frm.pack(side = tkinter.LEFT)
        
        # main panel
        self.pan_main = ttk.PanedWindow(self, orient = tkinter.HORIZONTAL)
        self.pan_main.pack(fill = tkinter.BOTH, expand = 1)
        
        # leftpanel
        self.frm_left = ttk.Frame(self.pan_main)
        self.pan_main.add(self.frm_left)
        # main view
        self.frm_view = ttk.Frame(self.pan_main)
        self.pan_main.add(self.frm_view)
        self.frm_view.grid_rowconfigure(0, weight = 1)
        self.frm_view.grid_columnconfigure(0, weight = 1)
        self.scr_view_x = ttk.Scrollbar(self.frm_view, 
            orient = tkinter.HORIZONTAL)
        self.scr_view_x.grid(row = 1, column = 0, \
            sticky = tkinter.E + tkinter.W)
        self.scr_view_y = ttk.Scrollbar(self.frm_view)
        self.scr_view_y.grid(row = 0, column = 1, sticky = \
            tkinter.N + tkinter.S)
        # canvas
        self.canv_view = tkinter.Canvas(self.frm_view, height = 150, 
            bd = 0, highlightthickness = 0, 
            scrollregion = (0, 0, 50, 50),
            )
        # don't forget
        #   canvas.config(scrollregion=(left, top, right, bottom))
        self.canv_view.bind('<Configure>', self.on_resize_view)
        self.canv_view.bind('<ButtonPress-1>', self.on_mouse_view)
        
        # text
        self.text_view = ReadOnlyText(self.frm_view,
            highlightthickness = 0,
            )
        self.text_hl = HyperlinkManager(self.text_view)
        self.text_view.bind('<Configure>', self.on_resize_view)
        
    def create_menu(self):
        self.menubar = tkinter.Menu(self.master)
        self.master.configure(menu = self.menubar)

    def on_exit(self):
        self.master.destroy()

    def on_mouse_view(self, event):
        self.update_after()
        
    def on_resize_view(self, event):
        self.update_after()
 
    def parse_path(self, loc):
        if isinstance(loc, str):
            path = []
            if loc[:1] == "/":
                loc = loc[1:]
            if loc != "":
                for item in loc.split("/"):
                    try:
                        path.append(int(item, 10))
                    except:
                        path.append(item)
        else:
            path = loc
        path = tuple(path)
        while path[-1:] == ("",):
            path = path[:-1]
        return path    
 
    def desc_path(self, loc):
        path = self.parse_path(loc)
        if len(path) > 0:
            if path[0] in self.path_handler:
                desc = self.path_handler[path[0]][1]
                if callable(desc):
                    return desc(path)
                elif desc:
                    return desc
        return self.desc_default(path)

    def update_canvas(self):
        if self.curr_main == 0:
            return
        # draw grahics
        c = self.canv_view
        c.delete(tkinter.ALL)

        w = self.canv_view.winfo_width() 
        h = self.canv_view.winfo_height()
        if (w == 0) or (h == 0): 
            return
        
        scale = 0

        # Preview image
        if not isinstance(self.main_image, tkinter.PhotoImage):
            mw, mh = self.main_image.size
            if scale == 0: # Fit
                try:
                    psc = w / h
                    isc = mw / mh
                    if psc < isc:
                        fact = w / mw
                    else:
                        fact = h / mh
                except:
                    fact = 1.0
            else:
                fact = scale
            pw = int(mw * fact)
            ph = int(mh * fact)
            img = self.main_image.resize((pw, ph), Image.ANTIALIAS)
            self.canv_image = ImageTk.PhotoImage(img)
        else:
            mw = self.main_image.width()
            mh = self.main_image.height()
            if scale == 0: # Fit
                try:
                    psc = w / h
                    isc = mw / mh
                    if psc < isc:
                        if w > mw:
                            fact = w // mw
                        else:
                            fact = -mw // w
                    else:
                        if h > mh:
                            fact = h // mh
                        else:
                            fact = -mh // h
                except:
                    fact = 1
            else:
                fact = scale
            self.canv_image = self.main_image.copy()
            if fact > 0:
                self.canv_image = self.canv_image.zoom(fact)
            else:
                self.canv_image = self.canv_image.subsample(-fact)
            self.canv_image_fact = fact

            # place on canvas
            if fact > 0:
                pw = mw * fact
                ph = mh * fact
            else:
                pw = mw // -fact
                ph = mh // -fact

        cw = max(pw, w)
        ch = max(ph, h)
        c.config(scrollregion = (0, 0, cw - 2, ch - 2))
        #print("Place c %d %d, p %d %d" % (cw, ch, w, h))
        c.create_image(cw // 2, ch // 2, image = self.canv_image)
       
    def make_image(self, imgobj):
        if imgobj.image is not None:
            return imgobj.image
        width = imgobj.width
        height = imgobj.height
        data = imgobj.rgb
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
   
    def update_gui(self, text = "<Undefined>"):
        self.last_path = self.curr_path
        # cleanup
        for item in self.curr_gui:
            item()
        self.curr_gui = []
        self.curr_state = {} # save state across moves
        # left listbox
        lab = tkinter.Label(self.frm_left, text = text)
        lab.pack()
        frm_lb = ttk.Frame(self.frm_left)
        frm_lb.pack(fill = tkinter.BOTH, expand = 1)
        frm_lb.grid_rowconfigure(0, weight = 1)
        frm_lb.grid_columnconfigure(0, weight = 1)
        scr_lb_x = ttk.Scrollbar(frm_lb, orient = tkinter.HORIZONTAL)
        scr_lb_x.grid(row = 1, column = 0, sticky = tkinter.E + tkinter.W)
        scr_lb_y = ttk.Scrollbar(frm_lb)
        scr_lb_y.grid(row = 0, column = 1, sticky = tkinter.N + tkinter.S)
        frmlbpad = ttk.Frame(frm_lb, borderwidth = self.pad)
        lb = tkinter.Listbox(frm_lb,
            highlightthickness = 0,
            xscrollcommand = scr_lb_x.set,
            yscrollcommand = scr_lb_y.set)
        lb.grid(row = 0, column = 0, \
            sticky = tkinter.N + tkinter.S + tkinter.E + tkinter.W)
        scr_lb_x.config(command = lb.xview)
        scr_lb_y.config(command = lb.yview)
        self.curr_gui.append(lambda:lb.grid_remove())
        self.curr_gui.append(lambda:lab.pack_forget())
        self.curr_gui.append(lambda:frm_lb.pack_forget())
        lb.bind("<Double-Button-1>", self.on_left_listbox)
        lb.bind("<Return>", self.on_left_listbox)
        # actions on listbox
        self.curr_lb = lb
        self.curr_lb_acts = []
        self.curr_lb_idx = {}

    def switch_view(self, main):
        # main view
        if main == self.curr_main: return
        last = self.curr_main
        self.curr_main = main
        rw = None
        rh = None
        if main == 0:
            self.canv_view.delete(tkinter.ALL)
            self.canv_view.grid_forget()
            self.text_view.grid(row = 0, column = 0, \
                sticky = tkinter.N + tkinter.S + tkinter.E + tkinter.W)
            self.text_view.configure(
                xscrollcommand = self.scr_view_x.set,
                yscrollcommand = self.scr_view_y.set
            )
            self.scr_view_x.config(command = self.text_view.xview)
            self.scr_view_y.config(command = self.text_view.yview)
        else:        
            if last == 0:
                rw = self.text_view.winfo_width()
                rh = self.text_view.winfo_height()
            self.canv_view.delete(tkinter.ALL)
            self.text_view.grid_forget()
            self.canv_view.grid(row = 0, column = 0, \
                sticky = tkinter.N + tkinter.S + tkinter.E + tkinter.W)
            self.canv_view.configure(
                xscrollcommand = self.scr_view_x.set,
                yscrollcommand = self.scr_view_y.set
            )
            self.scr_view_x.config(command = self.canv_view.xview)
            self.scr_view_y.config(command = self.canv_view.yview)
            if rh:
                print(rh)
                self.canv_view.height = rh
                print(self.canv_view.winfo_height())

    def clear_info(self):
        self.text_view.delete(0.0, tkinter.END)

    def add_text(self, text):
        self.end_markup()
        self.text_view.insert(tkinter.INSERT, text)

    def add_info(self, text):
        self.curr_markup += text
        
    def end_markup(self):
        if not self.curr_markup: return
        def make_cb(path):
            def cb():
                if path[:5] == "http:" or path[:6] == "https:":
                    return self.open_http(path)
                return self.open_path(path)
            return cb
        self.text_hl.add_markup(self.curr_markup, self.text_view, make_cb)
        self.curr_markup = ""
        
    def insert_lb_act(self, name, act, key = None):
        if key is not None:
            self.curr_lb_idx[key] = len(self.curr_lb_acts)
        self.curr_lb_acts.append((name, act))
        if name == "-" and act is None:
            self.curr_lb.insert(tkinter.END, "")
        else:
            self.curr_lb.insert(tkinter.END, " " + name)

    def select_lb_item(self, key):
        idx = self.curr_lb_idx.get(key, None)
        need = (idx is not None)
        idxs = "{}".format(idx)
        for sel in self.curr_lb.curselection():
            if sel == idxs:
                need = False
            else:
                self.curr_lb.selection_clear(sel)
        if need:
            self.curr_lb.selection_set(idxs)
        if idx is not None:
            self.curr_lb.see(idxs)
            
    def on_left_listbox(self, event):
        def currsel():
            try:
                num = self.curr_lb.curselection()[0]
                num = int(num)
            except:
                return None
            return num

        if self.curr_lb_acts:
            act = self.curr_lb_acts[currsel()]
            if act[1] is not None:
                self.open_path(act[1])

    def add_toolbtn(self, text, cmd):
        if text is None:
            frm = ttk.Frame(self.toolbar, width = self.pad, height = self.pad)
            frm.pack(side = tkinter.LEFT)
            self.curr_gui.append(lambda:frm.pack_forget())    
            return
        btn = ttk.Button(self.toolbar, text = text, \
            style = "Tool.TButton", command = cmd)
        btn.pack(side = tkinter.LEFT)
        self.curr_gui.append(lambda:btn.pack_forget())
        return btn
        
    def add_toollabel(self, text):
        lab = ttk.Label(self.toolbar, text = text)
        lab.pack(side = tkinter.LEFT)
        self.curr_gui.append(lambda:lab.pack_forget())
        return lab

    def add_toolgrp(self, label, glkey, items, cbupd):
        def makecb(v, g):
            def btncb():
                self.gl_state[g] = v
                cbupd()
            return btncb
        if label:
            self.add_toollabel(label)
        kl = list(items.keys())
        kl.sort()
        res = []
        for k in kl:
            b = self.add_toolbtn(items[k], makecb(k, glkey))
            res.append([b, k])
        return res

    def upd_toolgrp(self, btns, state):
        for btn, idx in btns:
            if idx != state and state != -1:
                btn.config(state = tkinter.NORMAL)
            else:
                btn.config(state = tkinter.DISABLED)
    
    def clear_hist(self):
        self.hist = self.hist[-1:]
        self.histf = []

    def open_http(self, path):
        messagebox.showinfo(parent = self, title = "URL", message = path)
 
    def open_path(self, loc, withhist = True):
        path = self.parse_path(loc)        
        if withhist:
            self.hist.append([path])
            self.histf = []
        print("DEBUG: Open", path)
        self.curr_path = path
        if len(path) > 0:
            self.curr_help = path[0]
        else:
            self.curr_help = ""
        try:
            if len(path) > 0 and path[0] in self.path_handler:
                res = self.path_handler[path[0]][0](path)
            else:
                res = self.path_default(path)
        except Exception:
            self.switch_view(0)
            self.add_text("\n" + "="*20 + "\n" + traceback.format_exc())
            res = True
        self.end_markup()

    def path_default(self, path):
        self.switch_view(0)
        self.clear_info()
        self.add_info("Open path\n\n" + str(path))
        return True
        

