#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# romiq.kh@gmail.com, 2014

import sys, os
import tkinter
from tkinter import ttk, font, filedialog, messagebox
from idlelib.WidgetRedirector import WidgetRedirector
import traceback

try:
    from PIL import Image
except ImportError:
    Image = None
try:
    from PIL import ImageTk
except ImportError:
    ImageTk = None

import petka

APPNAME = "P1&2 Explorer"
VERSION = "v0.2f 2014-05-16"

def hlesc(value):
    if value is None:
        return "None"
    return value.replace("\\", "\\\\").replace("<", "\\<").replace(">", "\\>")

def fmt_opcode(opcode):
    return petka.OPCODES.get(opcode, ["OP{:04X}".format(opcode)])[0]

def fmt_dlgop(opcode):
    return petka.DLGOPS.get(opcode, ["OP{:02X}".format(opcode)])[0]

def fmt_hl(loc, desc):
    return "<a href=\"{}\">{}</a>".format(loc, desc)


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

    def add(self, action):
        # add an action to the manager.  returns tags to use in
        # associated text widget
        tag = "hyper-{}".format(len(self.links))
        self.links[tag] = action
        return "hyper", tag

    def _enter(self, event):
        self.text.config(cursor = "hand2")

    def _leave(self, event):
        self.text.config(cursor = "")

    def _click(self, event):
        for tag in self.text.tag_names(tkinter.CURRENT):
            if tag[:6] == "hyper-":
                self.links[tag]()
                return
		
		
# thanx http://tkinter.unpythonic.net/wiki/ReadOnlyText
class ReadOnlyText(tkinter.Text):
    def __init__(self, *args, **kwargs):
        tkinter.Text.__init__(self, *args, **kwargs)
        self.redirector = WidgetRedirector(self)
        self.insert = \
            self.redirector.register("insert", lambda *args, **kw: "break")
        self.delete = \
            self.redirector.register("delete", lambda *args, **kw: "break")
		
class App(tkinter.Frame):
    def __init__(self, master):
        tkinter.Frame.__init__(self, master)
        master.title(APPNAME)
        self.pack(fill = tkinter.BOTH, expand = 1)
        self.pad = None
        self.sim = None
        # path
        if hasattr(sys, 'frozen'):
            self.app_path = sys.executable
        else:
            self.app_path = __file__
        self.app_path = os.path.abspath(os.path.dirname(self.app_path))
        self.start_act = []
        # gui
        self.path_handler = {}
        self.curr_main = -1 # 0 - frame, 1 - canvas
        self.curr_path = []
        self.last_path = [None]
        self.last_fn = ""
        self.curr_mode = 0
        self.curr_mode_sub = None
        self.curr_gui = []
        self.curr_lb_acts = None
        self.curr_lb_idx = None
        self.hist = []
        self.histf = []
        # canvas
        self.need_update = False
        self.canv_view_fact = 1
        self.main_image = tkinter.PhotoImage(width = 1, height = 1)
        self.after_idle(self.on_first_display)
        
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
                frm = ttk.Frame(self.toolbar, width = self.pad, height = self.pad)
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
        
        # bind path handlers
        self.path_handler["parts"] = self.path_parts
        self.path_handler["res"] = self.path_res
        self.path_handler["objs"] = self.path_objs_scenes
        self.path_handler["scenes"] = self.path_objs_scenes
        self.path_handler["names"] = self.path_names
        self.path_handler["invntr"] = self.path_invntr
        self.path_handler["msgs"] = self.path_msgs
        self.path_handler["dlgs"] = self.path_dlgs
        self.path_handler["casts"] = self.path_casts
        self.path_handler["test"] = self.path_test
        self.path_handler["about"] = self.path_about
        self.path_handler["support"] = self.path_support
        self.path_handler["help"] = self.path_help
        self.path_handler["info"] = self.path_info
        
        self.update_after()
        repath = "/about"
        for cmd, arg in self.start_act:
            if cmd == "load":
                self.open_data_from(arg)
                repath = "/"
            elif cmd == "open":
                self.open_path(arg)
                repath = ""
        if repath:
            self.open_path(repath)

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
                command = lambda: self.open_path("/parts"),
                label = "Select part")
        self.menuedit.add_separator()
        self.menuedit.add_command(
                command = lambda: self.open_path("/res"),
                label = "Resources")
        self.menuedit.add_command(
                command = lambda: self.open_path("/objs"),
                label = "Objects")
        self.menuedit.add_command(
                command = lambda: self.open_path("/scenes"),
                label = "Scenes")
        self.menuedit.add_command(
                command = lambda: self.open_path("/names"),
                label = "Names")
        self.menuedit.add_command(
                command = lambda: self.open_path("/invntr"),
                label = "Invntr")
        self.menuedit.add_command(
                command = lambda: self.open_path("/casts"),
                label = "Casts")
        self.menuedit.add_command(
                command = lambda: self.open_path("/msgs"),
                label = "Messages")
        self.menuedit.add_command(
                command = lambda: self.open_path("/dlgs"),
                label = "Dialog groups")

        self.menunav = tkinter.Menu(self.master, tearoff = 0)
        self.menubar.add_cascade(menu = self.menunav,
                label = "Navigation")
        self.menunav.add_command(
                command = self.on_back,
                label = "Back")
        self.menunav.add_command(
                command = self.on_forward,
                label = "Forward")
        self.menunav.add_separator()
        self.menunav.add_command(
                command = lambda: self.open_path(""),
                label = "Outline")
        self.menunav.add_separator()
        self.menunav.add_command(
                command = lambda: self.open_path("/hist"),
                label = "History")

        self.menuhelp = tkinter.Menu(self.master, tearoff = 0)
        self.menubar.add_cascade(menu = self.menuhelp,
                label = "Help")
        self.menuhelp.add_command(
                command = lambda: self.open_path("/help/index"),
                label = "Contents")
        self.menuhelp.add_separator()
        self.menuhelp.add_command(
                command = lambda: self.open_path("/support"),
                label = "Support")
        self.menuhelp.add_command(
                command = lambda: self.open_path("/info"),
                label = "Info")
        self.menuhelp.add_command(
                command = lambda: self.open_path("/about"),
                label = "About")

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
        self.update_after()
        
    def on_resize_view(self, event):
        self.update_after()
 
    def open_path(self, loc, withhist = True):
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

        if withhist:
            self.hist.append([path])
            self.histf = []

        print("DEBUG: Open", path)
        self.curr_path = path
        if len(path) > 0:
            if path[0] in self.path_handler:
                return self.path_handler[path[0]](path)
        return self.path_default(path)

    def update_canvas(self):
        if self.curr_main == 0:          
            return
        # draw grahics
        c = self.canv_view
        c.delete(tkinter.ALL)
        if self.sim is None: return

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
        self.curr_main = main
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

    def clear_info(self):
        self.text_view.delete(0.0, tkinter.END)

    def add_info(self, text):
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
                    else:
                        curr_text += ch
            else:
                if ch == ">":
                    if len(curr_text) > 0:                    
                        self.text_view.insert(tkinter.INSERT, curr_text, \
                            tuple([x for x in tags for x in x]))
                    if curr_tag[:7] == "a href=":
                        ref = curr_tag[7:]
                        if ref[:1] == "\"":
                            ref = ref[1:]
                        if ref[-1:] == "\"":
                            ref = ref[:-1]
                        def make_cb(path):
                            def cb():
                                return self.open_path(path)
                            return cb
                        tags.append(self.text_hl.add(make_cb(ref)))
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
            self.text_view.insert(tkinter.INSERT, curr_text, \
                tuple([x for x in tags for x in x]))
        
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
                pass
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

    def clear_hist(self):
        self.hist = self.hist[-1:]
        self.histf = []

    def on_help(self):
        if len(self.curr_path) > 0:
            self.open_path(["help", self.curr_path[0]])
        else:
            self.open_path(["help"])

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

    def fmt_hl_rec(self, lst_idx, pref, rec_id, full = False):
        if rec_id in lst_idx:
            fmt = fmt_hl("/{}/{}".format(pref, rec_id), str(rec_id))
            if full:
                try:
                    fmt += " (0x{:X}) - {}".format(rec_id, 
                        hlesc(lst_idx[rec_id].name))
                except:
                    fmt += " (0x{:X})".format(rec_id)
            return fmt
        return "{} (0x{:X})".format(rec_id, rec_id)
        
    def fmt_hl_obj(self, obj_id, full = False):
        return self.fmt_hl_rec(self.sim.obj_idx, "objs", obj_id, full)
        
    def fmt_hl_scene(self, scn_id, full = False):
        return self.fmt_hl_rec(self.sim.scn_idx, "scenes", scn_id, full)

    def fmt_hl_obj_scene(self, rec_id, full = False):
        if rec_id in self.sim.obj_idx:
            return self.fmt_hl_rec(self.sim.obj_idx, "objs", rec_id, full)
        return self.fmt_hl_rec(self.sim.scn_idx, "scenes", rec_id, full)
        
    def find_path_name(self, key):
        for name_id, name in enumerate(self.sim.namesord):
            if name == key:
                return "/names/{}".format(name_id)
        return "/no_name/{}".format(key)

    def find_path_invntr(self, key):
        for inv_id, name in enumerate(self.sim.invntrord):
            if name == key:
                return "/invntr/{}".format(inv_id)
        return "/no_invntr/{}".format(key)

    def fmt_hl_msg(self, msg_id, full = False):
        msg_idx = {}
        if msg_id < len(self.sim.msgs):
            msg_idx[msg_id] = self.sim.msgs[msg_id]
        return self.fmt_hl_rec(msg_idx, "msgs", msg_id, full)

    def fmt_hl_dlg(self, grp_id, full = False):
        return self.fmt_hl_rec(self.sim.dlg_idx, "dlgs", grp_id, full)

    def path_info_outline(self):
        if self.sim is None:
            self.add_info("No data loaded. Open PARTS.INI or SCRIPT.DAT first.")
            return
        self.add_info("Current part {} chapter {}\n\n".\
                format(self.sim.curr_part, self.sim.curr_chap))
        self.add_info("  Resources:     <a href=\"/res\">{}</a>\n".\
            format(len(self.sim.res)))
        self.add_info("  Objects:       <a href=\"/objs\">{}</a>\n".\
            format(len(self.sim.objects)))
        self.add_info("  Scenes:        <a href=\"/scenes\">{}</a>\n".\
            format(len(self.sim.scenes)))
        self.add_info("  Names:         <a href=\"/names\">{}</a>\n".\
            format(len(self.sim.names)))
        self.add_info("  Invntr:        <a href=\"/invntr\">{}</a>\n".\
            format(len(self.sim.invntr)))
        self.add_info("  Casts:         <a href=\"/casts\">{}</a>\n".\
            format(len(self.sim.casts)))
        self.add_info("  Messages       <a href=\"/msgs\">{}</a>\n".\
            format(len(self.sim.msgs)))
        self.add_info("  Dialog groups: <a href=\"/dlgs\">{}</a>\n".\
            format(len(self.sim.dlgs)))
    

    def path_default(self, path):
        self.switch_view(0)
        self.update_gui("Outline")
        self.clear_info()
        if len(path) != 0:
            spath = ""
            for item in path:
                spath += "/" + str(item)
            self.add_info("Path {} not found\n\n".format(spath))
        if self.sim is not None:
            self.add_info("Select from <b>outline</b>\n\n")
        self.path_info_outline()
        if self.sim is not None:
            acts = [
                ("Parts ({})".format(len(self.sim.parts)), "/parts"),
                ("Resources ({})".format(len(self.sim.res)), "/res"),
                ("Objects ({})".format(len(self.sim.objects)), "/objs"),
                ("Scenes ({})".format(len(self.sim.scenes)), "/scenes"),
                ("Names ({})".format(len(self.sim.names)), "/names"),
                ("Invntr ({})".format(len(self.sim.invntr)), "/invntr"),
                ("Casts ({})".format(len(self.sim.casts)), "/casts"),
                ("Messages ({})".format(len(self.sim.msgs)), "/msgs"),
                ("Dialog groups ({})".format(len(self.sim.dlgs)), "/dlgs"),
                #("-", None),
                #("Test image", ["test", "image"]),
                #("Test info", ["test","info"]),
            ]
            for name, act in acts:
                self.insert_lb_act(name, act)

    def path_parts(self, path):
        if self.sim is None:
            return self.path_default([])

        self.switch_view(0)
        if self.last_path[:1] != ("parts",):
            self.update_gui("Parts ({})".format(len(self.sim.parts)))
            for name in self.sim.parts:
                pnum = name[5:]
                cnum = pnum.split("Chapter", 1)
                if len(cnum) > 1:
                    pnum = int(cnum[0].strip(), 10)
                    cnum = int(cnum[1].strip(), 10)
                else:
                    cnum = 0
                part_id = "{}.{}".format(pnum, cnum)
                self.insert_lb_act(name, ["parts", part_id], part_id)
            if len(self.sim.parts) == 0:
                # Option to fix paths
                def fix_paths():
                    self.sim.curr_path = ""
                    path = self.sim.fman.root
                    while self.sim.curr_path == "":
                       self.sim.curr_path = os.path.basename(path)
                       path2 = os.path.dirname(path)
                       if path2 == path: break
                       path = path2
                    path = self.sim.fman.root = path
                    self.sim.curr_path += "\\"
                self.add_toolbtn("Fix paths", fix_paths)

        # change
        part = None
        if len(path) > 1:
            # parts
            self.select_lb_item(path[1])
            try:
                part = path[1]
                part = part.split(".", 1)
                part[0] = int(part[0])
                part[1] = int(part[1])
            except:
                part = None
                pass
        else:
            self.select_lb_item(None)
        # display
        self.clear_info()
        if not part:
            self.add_info("Select <b>part</b>\n\n")
            self.path_info_outline()
        else:
            try:
                self.clear_hist()
                self.sim.open_part(part[0], part[1])
                self.path_info_outline()
            except:
                self.add_info("Error open part {} chapter {} - \n\n{}".\
                    format(part[0], part[1], hlesc(traceback.format_exc())))
        

    def path_res(self, path):
        # res - full list
        # res/flt/<ext> - list by <ext>
        # res/all/<id> - display res by id
        if self.sim is None:
            return self.path_default([])
        if path == ("res",):
            path = ("res", "all")
        if path[1] == "flt":
            return self.path_res_flt(path)
        elif path[1] == "all":
            return self.path_res_all(path)
        else:
            return self.path_default(path)

    def path_res_open(self, pref, res_id, mode):
        if res_id not in self.sim.res:        
            self.switch_view(0)
            self.clear_info()
            self.add_info("<b>Resource</b> \"{}\" not found\n".format(res_id))
            return
        self.select_lb_item(res_id)
        resref = "/" + "/".join([str(x) for x in pref])
        if len(mode) == 0:
            self.switch_view(0)
            fn = self.sim.res[res_id]
            self.clear_info()
            self.add_info("<b>Resource</b>: {} (0x{:X}) - \"{}\"\n\n".\
                format(res_id, res_id, hlesc(fn)))
            self.add_info("<a href=\"{}/view\">View</a>\n\n".\
                format(resref, resref))
            try:
                if fn[-4:].lower() == ".bmp":
                    self.add_info("<b>BMP image</b>: ")
                    bmpf = self.sim.fman.read_file_stream(fn)
                    bmp = petka.BMPLoader()
                    bmp.load_info(bmpf)
                    if bmp.image:
                        # PIL
                        self.add_info("Python Imaging\n")
                        self.add_info("  Mode: {}\n  Size: {}x{}".\
                            format(bmp.image.mode, \
                                bmp.image.size[0], bmp.image.size[1]))
                    else:    
                        self.add_info("internal BMP loader\n"\
                            "  Mode: 16-bit\n  Size: {}x{}".\
                            format(bmp.width, bmp.height))
                elif fn[-4:].lower() == ".flc":
                    self.add_info("<b>FLC animation</b>: ")
                    flcf = self.sim.fman.read_file_stream(fn)
                    flc = petka.FLCLoader()
                    flc.load_info(flcf)
                    if flc.image:
                        # PIL
                        self.add_info("Python Imaging\n")
                        self.add_info("  Mode:   {}\n  Size:   {}x{}\n"
                            "  Frames: {}\n  Delay:  {}".\
                            format(flc.image.mode, \
                                flc.image.size[0], flc.image.size[1],
                                flc.frame_num, flc.image.info["duration"]))
                    else:    
                        self.add_info("internal FLC loader\n  "\
                            "  Mode:   P\n  Size:   {}x{}\n"\
                            "  Frames: {}\nDelay: {}".\
                            format(flc.width, flc.height, \
                                flc.frame_num, flc.delay))
                else:
                    self.add_info("No information availiable")

            except:
                self.add_info("Error loading {} - \"{}\" \n\n{}".\
                    format(res_id, hlesc(fn), hlesc(traceback.format_exc())))

            def usedby(lst):
                for idx, rec in enumerate(lst):
                    ru = False
                    for act_id, act_cond, act_arg, ops in rec.acts:
                        if ru: break
                        for op_id, op_code, op_res, op4, op5 in ops:
                            if res_id == op_res:
                                self.add_info("  " + 
                                    self.fmt_hl_obj_scene(rec.idx, True) + "\n")
                                ru = True
                                break
                            #print(op_id, op_code, op_res, op4, op5)

            self.add_info("\n\n<b>Used by objects</b>:\n")
            usedby(self.sim.objects)
            self.add_info("\n<b>Used by scenes</b>:\n")
            usedby(self.sim.scenes)
                    
        elif mode[0] == "view":
            self.path_res_view(res_id)
                
    def path_res_view(self, res_id):
        fn = self.sim.res[res_id]
        try:
            dataf = self.sim.fman.read_file_stream(fn)
            if fn[-4:].lower() == ".bmp":
                bmp = petka.BMPLoader()
                bmp.load_data(dataf)
                self.main_image = \
                    self.make_image(bmp)
                self.switch_view(1)
                self.update_canvas()
            elif fn[-4:].lower() == ".flc":
                flcf = self.sim.fman.read_file_stream(fn)
                flc = petka.FLCLoader()
                flc.load_data(dataf)
                self.main_image = \
                    self.make_image(flc)
                self.switch_view(1)
                self.update_canvas()
            else:
                self.switch_view(0)
                self.clear_info()
                self.add_info("Resource {} - \"{}\" cannot be displayed\n".\
                    format(res_id, hlesc(fn)))
        except:
            self.switch_view(0)
            self.clear_info()
            self.add_info("Error loading {} - \"{}\" \n\n{}".\
                format(res_id, hlesc(fn), hlesc(traceback.format_exc())))
            dataf = None
        finally:
            if dataf:
                dataf.close()

    def path_res_status(self):
        self.switch_view(0)
        self.clear_info()
        self.add_info("<b>Resources</b>: <a href=\"/res\">{}</a>\n"\
            "Filetypes:\n".format(len(self.sim.res)))
        fts = {}
        for res in self.sim.res.values():
            fp = res.rfind(".")
            if fp >= 0:
                ft = res[fp + 1:].upper()
                fts[ft] = fts.get(ft, 0) + 1
        ftk = list(fts.keys())
        ftk.sort()
        for ft in ftk:
            self.add_info("  <a href=\"/res/flt/{}\">{}</a>: {}\n".format(
                ft, ft, fts[ft]))
        self.select_lb_item(None)
    
    def on_path_res_info(self):
        self.switch_view(0)
        
    def on_path_res_view(self):
        self.switch_view(1)

    def path_res_all(self, path):
        if self.last_path[:2] != ("res", "all",) and self.last_path != ("res",):
            self.update_gui("Resources ({})".format(len(self.sim.res)))
            #self.add_toolbtn("Info", self.on_path_res_info)
            #self.add_toolbtn("View", self.on_path_res_view)
            for res_id in self.sim.resord:
                    self.insert_lb_act("{} - {}".format(\
                res_id, self.sim.res[res_id]), ["res", "all", res_id], res_id)
        # change                
        if len(path) > 2:
            self.path_res_open(path[:3], path[2], path[3:])
        else:
            self.path_res_status()

    def path_res_flt(self, path):
        lst = []
        for idx, res_id in enumerate(self.sim.resord):
            if self.sim.res[res_id].upper().endswith("." + path[2]):
                lst.append(res_id)
        if self.last_path[:3] != ("res", "flt", path[2]):
            self.update_gui("Resources {} ({})".format(path[2], len(lst)))
            self.insert_lb_act("All", "/res")
            self.insert_lb_act("-", None)
            for res_id in lst:
                    self.insert_lb_act("{} - {}".format(\
                res_id, self.sim.res[res_id]), ["res", "flt", path[2], res_id], 
                    res_id)
        # change                
        if len(path) > 3:
            self.path_res_open(path[:4], path[3], path[4:])
        else:
            self.path_res_status()

    def path_objs_scenes(self, path):
        if self.sim is None:
            return self.path_default([])
        self.switch_view(0)
        isobj = (self.curr_path[0] == "objs")
        if isobj:
            lst = self.sim.objects
            lst_idx = self.sim.obj_idx
        else:
            lst = self.sim.scenes
            lst_idx = self.sim.scn_idx
        if self.last_path[:1] != (self.curr_path[0],):
            if isobj:
                self.update_gui("Objects ({})".format(len(lst)))
            else:
                self.update_gui("Scenes ({})".format(len(lst)))
            for rec in lst:
                self.insert_lb_act("{} - {}".format(rec.idx, rec.name), \
                    [self.curr_path[0], rec.idx], rec.idx)
        # change                
        rec = None
        if len(path) > 1:
            # index
            self.select_lb_item(path[1])
            try:
                rec = lst_idx[path[1]]
            except:
                pass
        else:
            self.select_lb_item(None)
        # display
        self.clear_info()
        if not rec:
            if len(path) > 1:
                self.add_info("Item \"{}\" at <b>{}</b> not found\n\n".format(
                    path[1], path[0]))
            self.add_info("Select item from list\n")
        else:
            # record info
            self.add_info(("<b>Object</b>" if isobj \
                else "<b>Scene</b>") + ":\n")
            self.add_info("  Index:  {} (0x{:X})\n  Name:   {}\n".\
                format(rec.idx, rec.idx, hlesc(rec.name)))
            if rec.name in self.sim.names:
                self.add_info("  " + fmt_hl(self.find_path_name(rec.name), 
                    "Alias") + ":  {}\n".format(
                        hlesc(self.sim.names[rec.name])))
            if rec.name in self.sim.invntr:
                self.add_info("  " + fmt_hl(self.find_path_invntr(rec.name), 
                    "Invntr") + ": {}\n".format(
                        hlesc(self.sim.invntr[rec.name])))
            # references / backreferences                    
            if isobj:
                # search where object used
                self.add_info("\n<b>Refered by scenes</b>:\n")
                for scn in self.sim.scenes:
                    for ref in scn.refs:
                        if ref[0].idx == rec.idx:
                            self.add_info("  " + 
                                self.fmt_hl_scene(scn.idx, True) + "\n")
                            break
            else:
                if len(rec.refs) == 0:
                    self.add_info("\nNo references\n")
                else:
                    self.add_info("\n<b>References</b>: {}\n".\
                        format(len(rec.refs)))
                for idx, ref in enumerate(rec.refs):
                    self.add_info("  {}) ".format(idx) + 
                        self.fmt_hl_obj(ref[0].idx))
                    msg = ""
                    for arg in ref[1:]:
                        msg += " "
                        if arg < 10:
                            msg += "{}".format(arg)
                        elif arg == 0xffffffff:
                            msg += "-1"
                        else:
                            msg += "0x{:X}".format(arg)
                    self.add_info(msg + " / {}\n".format(hlesc(ref[0].name)))

            resused = []
            dlgused = []
            self.add_info("\n<b>Handlers</b>: {}\n".format(len(rec.acts)))
            for idx, (act_op, act_status, act_ref, ops) in enumerate(rec.acts):
                msg = fmt_opcode(act_op)
                cmt = ""
                if act_status != 0xff or act_ref != 0xffff:
                    if act_ref == rec.idx:
                        act_ref = "THIS"
                    else:
                        if act_ref in self.sim.obj_idx:
                            cmt = " / " + self.fmt_hl_obj(act_ref, True)
                            act_ref = self.fmt_hl_obj(act_ref)
                        else:
                            act_ref = "0x{:X}".format(act_ref)
                    msg += " 0x{:02X} {}".format(act_status, act_ref)
                self.add_info("  {}) <u>on {}</u>, ops: {}{}\n".format(\
                    idx, msg, len(ops), cmt))
                for oidx, op in enumerate(ops):
                    self.add_info("    {}) {} ".format(oidx, fmt_opcode(op[1])))
                    cmt = ""
                    if op[0] == rec.idx:
                        self.add_info("THIS")
                    else:
                        self.add_info(self.fmt_hl_obj_scene(op[0]))
                        cmt = " / " + self.fmt_hl_obj_scene(op[0], True)
                    msg = ""
                    if op[2] != 0xffff:
                        if op[2] not in resused and op[2] in self.sim.res:
                            resused.append(op[2])
                    for arg in op[2:]:
                        msg += " "
                        if arg < 10:
                            msg += "{}".format(arg)
                        elif arg == 0xffff:
                            msg += "-1"
                        else:
                            msg += "0x{:X}".format(arg)
                    self.add_info("{}{}\n".format(msg, cmt))
                    if op[1] == 0x11: # DIALOG
                        if op[0] not in dlgused:
                            dlgused.append(op[0])
                    
            if len(resused) > 0:
                self.add_info("\n<b>Used resources</b>: {}\n".\
                    format(len(resused)))
                for res_id in resused:
                    self.add_info("  " + fmt_hl("/res/all/{}".format(res_id), 
                        "{}".format(res_id)) + " (0x{:X}) - {}\n".format(res_id,
                            hlesc(self.sim.res[res_id])))
            if len(dlgused) > 0:
                self.add_info("\n<b>Used dialog groups</b>: {}\n".\
                    format(len(dlgused)))
                for grp_id in dlgused:
                    self.add_info("  " + self.fmt_hl_dlg(grp_id, True)+ "\n")
            
            if isobj:
                self.add_info("\n<b>Messages</b>:\n")
                for msg in self.sim.msgs:
                    if msg.obj.idx != rec.idx: continue
                    self.add_info("  " + self.fmt_hl_msg(msg.idx, True) + "\n")

    def path_std_items(self, path, level, guiname, guiitem, lst, lst_idx, 
            lbmode, cb):
        self.switch_view(0)
        if self.last_path[:level] != path[:level]:
            self.update_gui("{} ({})".format(guiname, len(lst)))
            for idx, name in enumerate(lst_idx):
                lb = name
                if lbmode == 1:
                    lb = "{} - {}".format(name, lst[name])
                self.insert_lb_act(lb, path[:level] + tuple([idx]), idx)
        # change
        name = None
        if len(path) > 1:
            # lb
            self.select_lb_item(path[1])
            try:
                name = lst_idx[path[1]]
            except:
                pass
        else:
            self.select_lb_item(None)
        # display
        self.clear_info()
        if not name:
            self.add_info("Select <b>{}</b> from list\n".format(guiitem))
        else:
            # info
            cb(name)
        
    def path_names(self, path):
        if self.sim is None:
            return self.path_default([])
        def info(name):
            self.add_info("<b>Alias</b>: {}\n".format(hlesc(name)))
            self.add_info("Value: {}\n\n".format(self.sim.names[name]))
            # search for objects
            self.add_info("<b>Applied for</b>:\n")
            for obj in self.sim.objects:
                if obj.name == name:
                    self.add_info("  " + self.fmt_hl_obj(obj.idx, True) + "\n")
        return self.path_std_items(path, 1, "Names", "name", self.sim.names, 
            self.sim.namesord, 0, info)
                            
    def path_invntr(self, path):
        if self.sim is None:
            return self.path_default([])
        def info(name):
            self.add_info("<b>Invntr</b>: {}\n".format(hlesc(name)))
            self.add_info("{}\n\n".format(self.sim.invntr[name]))
            # search for objects
            self.add_info("<b>Applied for</b>:\n")
            for obj in self.sim.objects:
                if obj.name == name:
                    self.add_info("  " + self.fmt_hl_obj(obj.idx, True) + "\n")
        return self.path_std_items(path, 1, "Invntr", "invntr", self.sim.invntr, 
            self.sim.invntrord, 0, info)


    def path_casts(self, path):
        if self.sim is None:
            return self.path_default([])
        def info(name):
            self.add_info("<b>Cast</b>: {}\n".format(hlesc(name)))
            self.add_info("Value: {}\n\n".format(self.sim.casts[name]))
            # search for objects
            self.add_info("<b>Applied for</b>:\n")
            for idx, obj in enumerate(self.sim.objects):
                if obj.name == name:
                    self.add_info("  " + self.fmt_hl_obj(obj.idx, True) + "\n")
        return self.path_std_items(path, 1, "Cast", "cast", self.sim.casts, 
            self.sim.castsord, 0, info)

    def path_msgs(self, path):
        if self.sim is None:
            return self.path_default([])
        self.switch_view(0)
        if self.last_path[:1] != ("msgs",):
            self.update_gui("Messages ({})".format(len(self.sim.msgs)))
            for idx, msg in enumerate(self.sim.msgs):
                capt = msg.name
                if len(capt) > 25:
                    capt = capt[:25] + "|"
                self.insert_lb_act("{} - {}".format(msg.idx, capt),
                    ["msgs", idx], idx)
        # change
        msg = None
        if len(path) > 1:
            # index
            self.select_lb_item(path[1])
            try:
                msg = self.sim.msgs[path[1]]
            except:
                pass
        else:
            self.select_lb_item(None)
        # display
        self.clear_info()
        if not msg:
            if len(path) > 1:
                self.add_info("<b>Message</b> \"{}\" not found\n\n".format(
                    path[1]))
            self.add_info("Select <b>message</b> from list\n")
        else:
            # msg info
            self.add_info("<b>Message</b>: {}\n".format(path[1]))
            self.add_info("  wav:    {}\n".format(msg.wav))
            self.add_info("  object: " + self.fmt_hl_obj(msg.obj.idx, True) + 
                "\n")
            self.add_info("  arg2:   {} (0x{:X})\n".format(msg.arg2, msg.arg2))
            self.add_info("  arg3:   {} (0x{:X})\n".format(msg.arg3, msg.arg3))
            self.add_info("\n{}\n".format(hlesc(msg.name)))

            self.add_info("\n<b>Used by dialog groups</b>:\n")
            for grp in self.sim.dlgs:
                for act in grp.acts:
                    for dlg in act.dlgs:
                        for op in dlg.ops:
                            if not op.msg: continue
                            if op.msg.idx == msg.idx and op.opcode == 7:
                                self.add_info("  " + 
                                    self.fmt_hl_dlg(grp.idx, True) + "\n")
                
    def path_dlgs(self, path):
        if self.sim is None:
            return self.path_default([])
        self.switch_view(0)
        if self.last_path[:1] != ("dlgs",):
            self.update_gui("Dialog groups ({})".format(len(self.sim.dlgs)))
            for grp in self.sim.dlgs:
                self.insert_lb_act("{} (0x{:X})".format(grp.idx, grp.idx),
                    ["dlgs", grp.idx], grp.idx)

        # change
        grp = None
        if len(path) > 1:
            # index
            self.select_lb_item(path[1])
            try:
                grp = self.sim.dlg_idx[path[1]]
            except:
                pass
        else:
            self.select_lb_item(None)
        # display
        self.clear_info()
        if not grp:
            if len(path) > 1:
                self.add_info("<b>Dialog</b> \"{}\" not found\n\n".format(
                    path[1]))
            self.add_info("Select <b>dialog group</b> from list\n")
        else:
            # grp info
            self.add_info("<b>Dialog group</b>: {} (0x{:X})\n".format(\
                grp.idx, grp.idx))
            self.add_info("  arg1: {} (0x{:X})\n\n".format(grp.arg1, grp.arg1))
            self.add_info("<b>Dialog handlers<b>: {}\n".format(len(grp.acts)))
            for idx, act in enumerate(grp.acts):
                self.add_info("  {}) <u>on {} {} 0x{:X} 0x{:X}</u>, dlgs: "\
                    "{} / {}\n".format(idx, fmt_opcode(act.opcode), 
                        self.fmt_hl_obj(act.ref), act.arg1, act.arg2, \
                        len(act.dlgs), self.fmt_hl_obj(act.ref, True)))
                for didx, dlg in enumerate(act.dlgs):
                    self.add_info("    {}) <i>0x{:X} 0x{:X}</i>, ops: {}\n".\
                        format(didx, dlg.arg1, dlg.arg2, len(dlg.ops)))
                    for oidx, op in enumerate(dlg.ops):
                        cmt = ""
                        opref = "0x{:X}".format(op.ref)
                        opcode = fmt_dlgop(op.opcode)
                        if op.opcode == 0x7:
                            opcode = "PLAY"
                            if op.msg:
                                opref = self.fmt_hl_msg(op.ref)
                                objref = self.fmt_hl_obj(op.msg.obj.idx)
                                cmt = " / obj={}, msg={}".\
                                    format(objref, 
                                        self.fmt_hl_msg(op.ref, True))
                        if op.opcode == 8:
                            cmt = " / select from "
                            doarr = []
                            docurr = []
                            skiptobrk = False
                            for op2 in dlg.ops[oidx + 1:]:
                                #cmt += fmt_dlgop(op2.opcode) + " "
                                if op2.opcode == 0x1: # BREAK
                                    doarr.append(docurr)
                                    skiptobrk = False
                                    if len(doarr) == op.ref:
                                        break
                                    docurr = []
                                elif op2.opcode == 0x7 and not skiptobrk: # PLAY
                                    docurr.append(self.fmt_hl_msg(op2.ref))
                                else:
                                    docurr = ["complex"]
                                    
                                    
                            if len(doarr) < op.ref:
                                cmt = " / circle select broken, required={}, "\
                                    "got={}".format(op.ref, len(doarr))
                            cmt += ",".join(["+".join(x) for x in doarr])
                        self.add_info("      {} 0x{:X} {}{}\n".\
                            format(opcode, op.arg, opref, cmt))

            def usedby(lst):
                for idx, rec in enumerate(lst):
                    ru = False
                    for act_id, act_cond, act_arg, ops in rec.acts:
                        if ru: break
                        for op_id, op_code, op_res, op4, op5 in ops:
                            if op_code == 0x11 and op_id == grp.idx: # DIALOG 
                                self.add_info("  " + 
                                    self.fmt_hl_obj_scene(rec.idx, True) + "\n")
                                ru = True
                                break
                            #print(op_id, op_code, op_res, op4, op5)

            self.add_info("\n\n<b>Used by objects</b>:\n")
            usedby(self.sim.objects)
            self.add_info("\n<b>Used by scenes</b>:\n")
            usedby(self.sim.scenes)
            
        
    def path_test(self, path):
        self.update_gui("Test {}".format(path[1]))
        self.insert_lb_act("Outline", [])
        self.insert_lb_act("-", None)
        for i in range(15):
            self.insert_lb_act("{} #{}".format(path[1], i), path[:2] + (i,))
        if path[1] == "image":
            self.switch_view(1)
            self.main_image = tkinter.PhotoImage(file = "img/splash.gif")
        else:
            self.switch_view(0)
            self.clear_info()
            self.add_info("Information panel for {}\n".format(path))
            for i in range(100):
                self.add_info("  Item {}\n".format(i))

    def path_about(self, path):
        self.switch_view(0)
        self.update_gui("About")
        self.insert_lb_act("Outline", [])
        self.clear_info()
        self.add_info("Welcome to <b>Petka 1 & 2 resource explorer</b>\n\n")
        self.add_info("  " + APPNAME + " " + VERSION + "\n")
        self.add_info("  romiq.kh@gmail.com\n")
        self.add_info("  https://bitbucket.org/romiq/p12simtran\n")
        self.add_info("\n")
        self.path_info_outline()

    def path_support(self, path):
        self.switch_view(0)
        self.update_gui("Support")
        self.insert_lb_act("Outline", [])
        self.clear_info()
        self.add_info("" + APPNAME + " " + VERSION + "\n")
        self.add_info("=" * 40 + "\n")
        self.add_info("<b>App folder</b>:  {}\n".format(
            hlesc(self.app_path)))
        self.add_info("<b>Game folder</b>: {}\n".format(
            hlesc(self.last_fn)))
        if self.sim is None:
            self.add_info("<i>Engine not initialized</i>\n")
        else:
            self.add_info("<i>Engine works</i>\n\n")
            self.add_info("  <b>Path</b>:    {}\n".format(
                hlesc(self.sim.curr_path)))
            self.add_info("  <b>Speech</b>:  {}\n".format(
                hlesc(self.sim.curr_speech)))
            self.add_info("  <b>Disk ID</b>: {}\n\n".format(
                hlesc(self.sim.curr_diskid)))
            self.path_info_outline()
            
    def path_help(self, path):
        self.switch_view(0)
        if path == ("help",):
            path = ("help", "index")
        if self.last_path[:1] != ("help",):
            self.update_gui("Help")
            # build help index
            lfn = os.path.join(self.app_path, "help", "list")
            f = open(lfn, "rb")
            try:
                for item in f.readlines():
                    item = item.decode("UTF-8").strip()
                    if not item: continue
                    try:
                        hf = open(os.path.join(self.app_path, 
                            "help", item + ".txt"), "rb")
                        try:
                            for line in hf.readlines():
                                line = line.decode("UTF-8").strip()                    
                                break
                        finally:
                            hf.close()
                    except:
                        line = item                    
                    self.insert_lb_act(line, ["help", item], item)
            finally:
                f.close()

            self.insert_lb_act("-", None)
            self.insert_lb_act("Outline", [])

        # change
        if len(path) > 1:
            # parts
            self.select_lb_item(path[1])
            self.clear_info()
            hfn = os.path.join(self.app_path, "help", path[1] + ".txt")
            try:
                hf = open(hfn, "rb")
                try:
                    for idx, line in enumerate(hf.readlines()):
                        line = line.decode("UTF-8").rstrip()                    
                        if idx == 0:
                            self.add_info("<b>" + line + "</b>\n")
                        else:                                        
                            self.add_info(line + "\n")
                finally:
                    hf.close()
            except:
                self.add_info("Error loading \"{}\" \n\n{}".\
                    format(hlesc(hfn), hlesc(traceback.format_exc())))
        else:
            self.select_lb_item(None)
        
    def path_info(self, path):
        self.switch_view(0)
        if self.last_path[:1] != ("info",):
            self.update_gui("Info")
            self.insert_lb_act("Opcodes", ["info", "opcodes"], "opcodes")
            self.insert_lb_act("Dialog opcodes", ["info", "dlgops"], "dlgops")
        # change
        name = None
        if len(path) > 1:
            # lb
            self.select_lb_item(path[1])
            name = path[1]
        else:
            self.select_lb_item(None)
        # display
        self.clear_info()
        if not name:
            self.add_info("Select <b>info</b> from list\n")
        else:
            # info
            if name == "opcodes":
                self.add_info("<b>Opcodes<b>\n\n")
                k = list(petka.OPCODES.keys())
                k.sort()
                for key in k:
                    self.add_info("  {} (0x{:X}) - {}\n".format(key, key,
                        petka.OPCODES[key][0])) 
            elif name == "dlgops":
                self.add_info("<b>Dialog opcodes<b>\n\n")
                k = list(petka.DLGOPS.keys())
                k.sort()
                for key in k:
                    self.add_info("  {} (0x{:X}) - {}\n".format(key, key,
                        petka.DLGOPS[key][0]))
            else: 
                self.add_info("Unknown data type \"{}\"\n".format(hlesc(name)))

    def on_open_data(self):
        ft = [\
            ('all files', '.*')]
        fn = filedialog.askopenfilename(parent = self, 
            title = "Open PARTS.INI or SCRIPT.DAT",
            filetypes = ft,
            initialdir = os.path.abspath(os.curdir))
        if not fn: return
        os.chdir(os.path.dirname(fn))
        self.clear_hist()
        if self.open_data_from(os.path.dirname(fn)):
            self.open_path("")
            self.clear_hist()
        
        
    def open_data_from(self, folder):
        self.last_fn = folder
        try:
            self.sim = petka.Engine()
            self.sim.load_data(folder, "cp1251")
            self.sim.open_part(0, 0)
            return True
        except:
            print("DEBUG: Error opening")
            self.sim = None
            self.switch_view(0)
            self.update_gui("")
            self.clear_info()
            self.add_info("Error opening \"{}\" \n\n{}".\
                format(hlesc(folder), hlesc(traceback.format_exc())))
            self.clear_hist()

def main():
    root = tkinter.Tk()
    app = App(master = root)
    if len(sys.argv) > 1:
        app.start_act.append(["load", sys.argv[1]])
    for arg in sys.argv[2:]:
        app.start_act.append(["open", arg])
    app.mainloop()

    
if __name__ == "__main__":
    main()
