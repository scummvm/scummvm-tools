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

        self.path_handler["test"] = [self.path_test, "Tests"]

        repath = "/"
        for cmd, arg in self.start_act:
            if cmd == "open":
                repath = ""
                if not self.open_path(arg):
                    print("DEBUG: stop opening after " + arg)
                    repath = ""
                    break
        if repath:
            self.open_path(repath)       


    def create_menu(self):
        super().create_menu()
        self.menufile = tkinter.Menu(self.master, tearoff = 0)
        self.menubar.add_cascade(menu = self.menufile,
                label = "File")
        self.menufile.add_separator()
        self.menufile.add_command(
                command = self.on_exit,
                label = "Quit")    

    def path_default(self, path):
        self.switch_view(0)
        self.update_gui("Outline")
        self.clear_info()
        self.add_info("Open path\n\n" + str(path))

        self.add_info("\n\n")
        self.add_info("<a href=\"http://example.com/test\">example.com</a>\n")
        self.add_info("\n\n<b>This is multi-")
        self.add_info("line\nbold</b>\n")
        self.add_info("\n\n<u>This is multi-")
        self.add_info("line\nunderline</u>\n")       
        self.add_info("\n\n<font color=\"#ff00FF\">This is multi-")
        self.add_info("line\nunderline</u>\n")

        self.insert_lb_act("Testing", "/test")        
        return True        

    def path_test(self, path):
        if len(path) > 1:
            return self.path_test_item(path)
        if self.last_path != ("test",):
            self.update_gui("Test")
            self.insert_lb_act("Outline", [])
            self.insert_lb_act("-", None)
            self.insert_lb_act("Info", "/test/info")
            self.insert_lb_act("Image", "/test/image")
            self.switch_view(0)
            self.clear_info()
            self.add_info("Select test from outline")            
        return True

    def path_test_item(self, path):
        def display_page():
            item = None
            path = self.curr_path
            if len(path) > 2:
                item = path[2]
            self.clear_info()
            sm = self.gl_state.get("test.info.mode", 0)
            if item is None:
                sm = -1
            self.upd_toolgrp(self.curr_state["gbtns"], sm)
            if item is None:
                self.switch_view(0)
                self.add_info("Select item " + path[1])
            else:
                if path[1] == "image":
                    self.switch_view(1)
                    self.main_image = tkinter.PhotoImage(file = "img/splash.gif")
                elif path[1] == "info":
                    self.switch_view(0)
                    self.add_info("Information panel for {}\n".format(path))
                    self.add_info("Local mode {}\n".format(
                        self.curr_state.get("mode", None)))
                    self.add_info("Global mode {}\n".format(
                        self.gl_state.get("test.info.mode", None)))
                    for i in range(100):
                        self.add_info("  Item {}\n".format(i))
            
        if self.last_path[:2] != ("test", path[1]):
            self.update_gui("Test %s" % path[1])
            self.insert_lb_act("Testing", "/test")
            self.insert_lb_act("-", None)
            for i in range(15):
                self.insert_lb_act("{} #{}".format(path[1], i), 
                    path[:2] + (i,), i)
            # create mode buttons
            def sw_mode1():
                print("Mode 1")
                self.curr_state["mode"] = 1
                self.curr_state["btn1"].config(state = tkinter.DISABLED)
                self.curr_state["btn2"].config(state = tkinter.NORMAL)
                display_page()
            def sw_mode2():
                print("Mode 2")
                self.curr_state["mode"] = 2
                self.curr_state["btn1"].config(state = tkinter.NORMAL)
                self.curr_state["btn2"].config(state = tkinter.DISABLED)
                display_page()
            self.curr_state["btn1"] = self.add_toolbtn("Mode 1", sw_mode1)
            self.curr_state["btn1"].config(state = tkinter.DISABLED)
            self.curr_state["btn2"] = self.add_toolbtn("Mode 2", sw_mode2)
            # we store buttons in local state
            self.curr_state["gbtns"] = self.add_toolgrp(None, "test.info.mode",
                {0: "mode 1", 1: "mode 2", 2: "mode 3"}, display_page)

        # change
        item = None
        if len(path) > 2:
            # index
            self.select_lb_item(path[2])
        else:
            self.select_lb_item(None)
        # display
        display_page()
        return True

def main():
    root = tkinter.Tk()
    app = App(master = root)
    argv = sys.argv[1:]
    while len(argv) > 0:
        if argv[0] == "--nooutline": # open data
            app.gui_setup["outline"] = False
            argv = argv[1:]
        else:
            app.start_act.append(["open", argv[0]])
            argv = argv[1:]
    app.mainloop()
    
if __name__ == "__main__":
    main()
