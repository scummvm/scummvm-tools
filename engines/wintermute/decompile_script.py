#! /usr/bin/env python3

# ScummVM Tools
#
# ScummVM Tools is the legal property of its developers, whose names
# are too numerous to list here. Please refer to the COPYRIGHT
# file distributed with this source distribution.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# WinterMute Engine scripts decompiler v1.1
#
# Copied from https://raw.githubusercontent.com/lolbot-iichan/decompile.wintermute/refs/heads/master/decompile_wintermute.py
# Made by lolbot
#
# Pipeline is like this:
# 1. wmeasm:       transform binary format to asm-like format
# 2. wmemedium:    evaluate stack operations to transform non-branching code to "actor.GoTo(this.WalkToX, this.WalkToY);"
# 3. wmehigh:      use some rules of thumb to detect while/switch/break/else constructions
# 4. final result: transform internal structs to readable code

from struct import unpack
from copy import deepcopy

import re
import os
import sys
import traceback

def safe_decode(b):
    for enc in ("cp1252", "latin-1", "utf-8"):
        try:
            return b.decode(enc)
        except UnicodeDecodeError:
            continue
    return b.decode("latin-1", errors="replace")

if  len(sys.argv) == 1 or len(sys.argv) > 3:
    print("Usage: %s <path-to-compiled-script> [<path-to-inc-folder>]")
    sys.exit(-1)
fn = sys.argv[1]
inc = {}
if  len(sys.argv) == 3:
    for i in os.listdir(sys.argv[2]):
        with open(sys.argv[2]+os.sep+i,"r") as f:
            txt = f.read().rstrip()
            if  txt:
                inc[i] = [t+"\n" for t in txt.split("\n")]

LB_WRITE_HEADERS = False
LB_WRITE_DISASM  = False
LB_WRITE_MEDIUM  = False
LB_WRITE_HIGHLVL = False
LB_WRITE_RESULT  = True

class WinterMuteDecompiler:
    opcodes = [
            "II_DEF_VAR",              #0
            "II_DEF_GLOB_VAR",
            "II_RET",
            "II_RET_EVENT",
            "II_CALL",
            "II_CALL_BY_EXP",          #5
            "II_EXTERNAL_CALL",
            "II_SCOPE",
            "II_CORRECT_STACK",
            "II_CREATE_OBJECT",
            "II_POP_EMPTY",            #10
            "II_PUSH_VAR",
            "II_PUSH_VAR_REF",
            "II_POP_VAR",
            "II_PUSH_VAR_THIS",
            "II_PUSH_INT",             #15
            "II_PUSH_BOOL",
            "II_PUSH_FLOAT",
            "II_PUSH_STRING",
            "II_PUSH_NULL",
            "II_PUSH_THIS_FROM_STACK", #20
            "II_PUSH_THIS",
            "II_POP_THIS",
            "II_PUSH_BY_EXP",
            "II_POP_BY_EXP",
            "II_JMP",                  #25
            "II_JMP_FALSE",
            "II_ADD",
            "II_SUB",
            "II_MUL",
            "II_DIV",                  #30
            "II_MODULO",
            "II_NOT",
            "II_AND",
            "II_OR",
            "II_CMP_EQ",               #35
            "II_CMP_NE",
            "II_CMP_L",
            "II_CMP_G",
            "II_CMP_LE",
            "II_CMP_GE",               #40
            "II_CMP_STRICT_EQ",
            "II_CMP_STRICT_NE",
            "II_DBG_LINE",
            "II_POP_REG1",
            "II_PUSH_REG1",            #45
            "II_DEF_CONST_VAR"
        ]

    varops = {
            "II_DEF_VAR":       "var",
            "II_DEF_GLOB_VAR":  "global",
            "II_DEF_CONST_VAR": "const",
        }

    binops = {
            "II_ADD":    "+",
            "II_SUB":    "-",
            "II_MUL":    "*",
            "II_DIV":    "/",
            "II_MODULO": "%",
            "II_AND":    "&&",
            "II_OR":     "||",
            "II_CMP_EQ": "==",
            "II_CMP_NE": "!=",
            "II_CMP_L":  "<",
            "II_CMP_G":  ">",
            "II_CMP_LE": "<=",
            "II_CMP_GE": ">=",
            "II_CMP_STRICT_EQ": "===",
            "II_CMP_STRICT_NE": "!==",
        }

    exttypes = ["","bool","long","char","string","float","double","membuffer"]

    table_names = ["function","symbol","event","external","method"]

    def __init__(self, data):
        self.data = data
        self.offsets = unpack("IIIIIIII",data[:32])

    def read_int(self):
        result = unpack("I",self.data[self.ptr:self.ptr+4])[0]
        self.ptr += 4
        return result

    def read_float(self):
        result = unpack("d",self.data[self.ptr:self.ptr+8])[0]
        self.ptr += 8
        return result

    def read_string(self):
        l = self.data[self.ptr:].find(b"\0")
        result = self.data[self.ptr:self.ptr+l].decode('cp1251')
        self.ptr += l+1
        return result

    def read_header(self):
        self.fname = safe_decode(self.data[32:self.offsets[2]].strip())
        self.tables = {}
        self.externals = []
        for i,title in enumerate(self.table_names):
            self.tables[title] = {}
            self.ptr = self.offsets[i+3]
            l = self.read_int()
            if  title != "external":
                for i in range(l):
                    idx = self.read_int()
                    self.tables[title][idx] = self.read_string()
            else:
                for i in range(l):
                    dllname = self.read_string()
                    fname = self.read_string()
                    ftype = ["stdcall","cdecl","thiscall"][self.read_int()]
                    fret = self.exttypes[self.read_int()]
                    fargs = [self.exttypes[self.read_int()] for i in range(self.read_int())]
                    self.externals += [(dllname, fname, ftype, fret, fargs)]

    def table_lookup(self, id):
        for t in self.table_names:
            if  id in self.tables[t]:
                return [t,self.tables[t][id]]

    def read_asm(self):
        self.disasm = {}

        self.ptr = self.offsets[2]
        while self.ptr < self.offsets[3]:
            ptr_old = self.ptr
            op = self.opcodes[self.read_int()]

            if  op in ["II_PUSH_FLOAT"]:
                param = self.read_float()
            elif op in ["II_DBG_LINE","II_PUSH_INT","II_PUSH_BOOL","II_JMP","II_JMP_FALSE","II_CORRECT_STACK"]:
                param = self.read_int()
            elif op in self.varops or op in ["II_PUSH_VAR_REF","II_POP_VAR","II_PUSH_VAR","II_PUSH_THIS","II_EXTERNAL_CALL"]:
                param = self.tables["symbol"][self.read_int()]
            elif op in ["II_CALL"]:
                param = self.table_lookup(self.read_int())[1]
            elif op in ["II_SCOPE"]:
                param = self.table_lookup(ptr_old)
            elif  op in ["II_PUSH_STRING"]:
                param = self.read_string()
            else:
                param = None

            self.disasm[ptr_old] = [op, param]

    def access(self,x,y):
        if  y[0] == '"' and y[-1] == '"' and not '"' in y[1:-1]:
            return x+"."+y[1:-1]
        return x+"["+y+"]"

    def create_medium(self):
        self.medium = {}
        stack_var = []
        stack_this = []
        reg1  = "<<<ERROR>>>"
        for ptr,(op,param) in sorted(self.disasm.items()):
            if  op in ["II_DBG_LINE"]:
                pass
            elif op in self.varops:
                self.medium[ptr] = ["III_DEF",(self.varops[op],param)]
            elif op in ["II_SCOPE"]:
                self.medium[ptr] = ["III_SCOPE",param]
            elif op in ["II_CORRECT_STACK"]:
                stack_var = ["<<<PARAM%d>>>"%(param-i-1) for i in range(param)]
                self.medium[ptr] = ["III_CORRECT_STACK",param]
            elif op in ["II_RET_EVENT"]:
                self.medium[ptr] = ["III_RET",None]
            elif op in ["II_RET"]:
                if  ptr != self.offsets[3]-4:
                    self.medium[ptr] = ["III_RET",stack_var.pop()]
                else:
                    self.medium[ptr] = ["III_RET_EOF",None]
            elif op in ["II_POP_BY_EXP","II_POP_VAR","II_POP_EMPTY"]:
                if  op == "II_POP_BY_EXP":
                    key = stack_var.pop()
                    param = self.access(stack_var.pop(),key)
                self.medium[ptr] = ["III_POP",(param,stack_var.pop())]
            elif op in ["II_JMP_FALSE"]:
                self.medium[ptr] = ["III_JMP_FALSE",[stack_var.pop(),param-1]]
            elif op in ["II_JMP"]:
                self.medium[ptr] = ["III_JMP",param-1]
            elif op in ["II_POP_REG1"]:
                reg1 = stack_var.pop()
                self.medium[ptr] = ["III_SCOPE",["switch",reg1]]

            elif op in ["II_PUSH_THIS"]:
                stack_this += [param]
            elif op in ["II_PUSH_THIS_FROM_STACK"]:
                stack_this += [stack_var[-1]]
            elif op in ["II_POP_THIS"]:
                stack_this.pop()

            elif op in ["II_CREATE_OBJECT"]:
                stack_var += ["<<<OBJECT>>>"]
            elif op in ["II_PUSH_INT","II_PUSH_BOOL","II_PUSH_FLOAT"]:
                stack_var += [repr(param)]
            elif op in ["II_PUSH_NULL"]:
                stack_var += ["null"]
            elif op in ["II_PUSH_STRING"]:
                stack_var += ['"'+param+'"']
            elif op in ["II_PUSH_VAR","II_PUSH_VAR_REF"]:
                stack_var += [param]
            elif op in ["II_NOT"]:
                stack_var += ["!("+stack_var.pop()+")"]
            elif op in self.binops:
                y = stack_var.pop()
                x = stack_var.pop()
                stack_var += ["("+x+" "+self.binops[op]+" "+y+")"]
            elif op in ["II_PUSH_BY_EXP"]:
                key = stack_var.pop()
                stack_var += [self.access(stack_var.pop(),key)]
            elif op in ["II_CALL","II_EXTERNAL_CALL"]:
                cnt = int(stack_var.pop())
                stack_var += [param+"("+", ".join([stack_var.pop() for i in range(cnt)])+u")"]
            elif op in ["II_CALL_BY_EXP"]:
                y = stack_var.pop()
                x = stack_var.pop()
                cnt = int(stack_var.pop())
                stack_var += [self.access(x,y)+"("+", ".join([stack_var.pop() for i in range(cnt)])+u")"]
            elif op in ["II_PUSH_REG1"]:
                stack_var += ["<<<REG1>>>"]
            else:
                print("create_medium: ",op, param, stack_var, stack_this)

    def process_medium(self):
        self.high = deepcopy(self.medium)
        self.process_medium_pop_object()
        self.process_medium_def_pop()
        self.process_medium_correct_stack()
        self.process_medium_simple_lines()
        self.process_medium_nop_jumps()
        self.process_medium_if_false()
        self.process_medium_if_to_while()
        self.process_medium_scope_ends()
        self.process_medium_switch_end()
        self.process_medium_switch_case()
        self.process_medium_switch_goto()
        self.process_medium_nop_scope_ends()
        self.process_medium_switch_default()
        self.process_medium_switch_break()
        self.process_medium_while_break()
        self.process_medium_if_else()
        return

    def process_medium_def_pop(self):
        items = sorted(self.high.items())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  idx>0:
                pptr,(pop,pparam) = items[idx-1]
                if  op == "III_POP" and pop == "III_DEF" and pparam[1] == param[0]:
                    mode = pparam[0]
                    del self.high[pptr]
                    self.high[ptr] = ["III_DEF_POP",(mode,param[0],param[1])]

    def process_medium_pop_object(self):
        items = sorted(self.high.items())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  idx>0:
                pptr,(pop,pparam) = items[idx-1]
                if  op == "III_POP" and pop == "III_POP" and pparam[0] == None and "<<<OBJECT>>>" in param[1]:
                    value = param[1].replace("<<<OBJECT>>>","new "+pparam[1],1)
                    if  not "<<<OBJECT>>>" in value:
                        del self.high[pptr]
                        self.high[ptr] = ["III_POP",(param[0],value)]

    def process_medium_correct_stack(self):
        items = sorted(self.high.items())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  idx>0:
                pptr,(pop,pparam) = items[idx-1]
                if  op == "III_CORRECT_STACK" and pop == "III_SCOPE":
                    for i in range(param):
                        if  items[idx+i+1][1][0] != "III_DEF_POP":
                            break
                        if  items[idx+i+1][1][1][0] != "var":
                            break
                        if  items[idx+i+1][1][1][2] != "<<<PARAM%d>>>"%i:
                            break
                    else:
                        params = [items[idx+i+1][1][1][1] for i in range(param)]
                        fcall  = pparam[1] + "(" + ", ".join(params) + ")"
                        self.high[pptr] = ["III_SCOPE",[pparam[0],fcall]]
                        for i in range(param+1):
                            del self.high[items[idx+i][0]]

    def process_medium_simple_lines(self):
        items = sorted(self.high.items())
        for ptr,(op,param) in items:
            if  op == "III_DEF":
                self.high[ptr] = ["III_LINE","%s %s;"%param]
            elif op == "III_DEF_POP":
                self.high[ptr] = ["III_LINE","%s %s = %s;"%param]
            elif op == "III_POP":
                if param[0] == None:
                    self.high[ptr] = ["III_LINE","%s;"%param[1]]
                else:
                    self.high[ptr] = ["III_LINE","%s = %s;"%param]
            elif op == "III_RET":
                if param == None:
                    self.high[ptr] = ["III_LINE","return;"]
                else:
                    self.high[ptr] = ["III_LINE","return %s;"%param]

    def process_medium_nop_jumps(self):
        for ptr,(op,param) in sorted(self.high.items()):
            if  op == "III_JMP_FALSE" and not param[1] in self.high:
                self.high[param[1]] = ["III_NOP",None]
            if  op == "III_JMP" and not param in self.high:
                self.high[param] = ["III_NOP",None]

    def process_medium_scope_ends(self):
        items = sorted(self.high.items())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  idx>0:
                pptr,(pop,pparam) = items[idx-1]
                if  op == "III_SCOPE" and pop == "III_JMP":
                    if  not(pparam) in self.high or self.high[pparam][0] == "III_NOP":
                        self.high[ptr][1] = param + [pparam]
                        self.high[pparam] = ["III_SCOPE_END",1]
                        del self.high[pptr]
                    elif self.high[pparam][0] == "III_SCOPE_END":
                        self.high[ptr][1] = param + [pparam]
                        self.high[pparam][1] += 1
                        del self.high[pptr]
                    else:
                        print("process_medium_scope_ends: ",self.high[pparam])

    def process_medium_if_false(self):
        items = sorted(self.high.items())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  op == "III_JMP_FALSE":
                target = param[1]
                if  not(target) in self.high or self.high[target][0] == "III_NOP":
                    self.high[ptr] = ["III_SCOPE",["if",param[0],target]]
                    self.high[target] = ["III_SCOPE_END",1]
                elif self.high[target][0] == "III_SCOPE_END":
                    self.high[ptr] = ["III_SCOPE",["if",param[0],target]]
                    self.high[target][1] += 1
                else:
                    print("process_medium_if_false: ",self.high[param[1]])

    def process_medium_if_to_while(self):
        items = sorted(self.high.items())
        keys  = sorted(self.high.keys())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  idx>0:
                pptr,(pop,pparam) = items[idx-1]
                if  op == "III_SCOPE_END" and pop == "III_JMP":
                    if  pparam < ptr and self.high[pparam][0] in ["III_NOP","III_SCOPE_END"]:
                        nxtptr = keys[keys.index(pparam)+1]
                        nxtop,nxtparam = self.high[nxtptr]
                        if  nxtop == "III_SCOPE" and nxtparam[0] == "if":
                            nxtparam[0] = "while"
                            nxtparam[2] -= 1
                            self.high[pptr] = ["III_SCOPE_END",1]
                            if  param == 1:
                                self.high[ptr] = ["III_NOP",None]
                            else:
                                self.high[ptr] = ["III_SCOPE_END",param-1]

    def count_stack(self,start,stop):
        delta = 0
        for i in range(start,stop+1):
            if  i in self.high and self.high[i][0] == "III_SCOPE":
                delta += 1
            if  i in self.high and self.high[i][0] == "III_SCOPE_END":
                delta -= self.high[i][1]
        return delta

    def process_medium_switch_end(self):
        scope_stack = ["<<<ERROR>>>"]
        items = sorted(self.high.items())
        last_switch = None
        for idx,(ptr,(op,param)) in enumerate(items):

            if  op == "III_SCOPE":
                scope_stack += [param]
#                print(ptr,"push:",param)
#                print(ptr,"stack:",scope_stack)
                if  param[0] == "switch":
                    last_switch = ptr
            if  idx>1 and last_switch:
                pptr,(pop,pparam) = items[idx-1]
                ppptr,(ppop,ppparam) = items[idx-2]
                if  op == "III_SCOPE_END" and pop == "III_JMP" and ppop != "III_JMP" and pparam == ptr and scope_stack[-1][0] == "switch":
                    if  len(self.high[last_switch][1]) == 2:
                        del self.high[pptr]
                        self.high[ptr][1] += 1
                        self.high[last_switch][1] += [ptr]
#                        print("process_medium_switch_end: rule #1:", scope_stack[-1])
                    elif self.high[last_switch][1][2] == ptr:
                        pass
                    else:
                        print(self.high[last_switch][1],ptr)
                elif  op == "III_NOP" and pop == "III_JMP" and ppop != "III_JMP" and pparam == ptr and scope_stack[-1][0] == "switch":
                    if  len(self.high[last_switch][1]) == 2:
                        del self.high[pptr]
                        self.high[ptr] = ["III_SCOPE_END",1]
                        self.high[last_switch][1] += [ptr]
#                        print("process_medium_switch_end:  rule #2:", scope_stack[-1])
                    elif self.high[last_switch][1][2] == ptr:
                        pass
                    else:
                        print(self.high[last_switch][1],ptr)
                elif  op == "III_SCOPE_END" and pop == "III_JMP" and ppop == "III_JMP":
                    if  len(self.high[last_switch][1]) == 2:
                        if self.high[ppparam][0] == "III_NOP":
                            self.high[ppparam] = ["III_SCOPE_END",1]
                        elif  self.high[ppparam][0] == "III_SCOPE_END":
                            self.high[ppparam][1] += 1
                        self.high[last_switch][1] += [ppparam]
#                        print("process_medium_switch_end:  rule #3:", scope_stack[-1])
                    elif self.high[last_switch][1][2] == ppparam:
                        pass
                    else:
                        print(self.high[last_switch][1],ppparam)
                if  (len(self.high[last_switch][1])>3):
                    print(self.high[last_switch])
            if  self.high[ptr][0] == "III_SCOPE_END":
                for i in range(self.high[ptr][1]):
                    parent = scope_stack.pop()
#                    print(ptr,"pop:",parent)
#                    print(ptr,"stack:",scope_stack)
        if  len(scope_stack) > 1:
            print("process_medium_switch_end: stack is not empty on exit: ",scope_stack)
        if  len(scope_stack) == 0:
            print("process_medium_switch_end: stack is exhausted on exit")

    def process_medium_switch_case(self):
        scope_stack = ["<<<ERROR>>>"]
        items = sorted(self.high.items())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  op == "III_SCOPE_END":
                for i in range(param):
                    scope_stack.pop()
            elif op == "III_SCOPE":
                if  param[0] == "if" and param[1].endswith(" == <<<REG1>>>)"):
                    if  scope_stack and scope_stack[-1][0] in ["switch"]:
                        param[0] = "case"
                        param[1] = param[1][1:-len(" == <<<REG1>>>)")]
                    else:
                        print("process_medium_switch_case: ", param, scope_stack)
                scope_stack += [param]
        if  len(scope_stack) > 1:
            print("process_medium_switch_case: stack is not empty on exit: ",scope_stack)
        if  len(scope_stack) == 0:
            print("process_medium_switch_case: stack is exhausted on exit")

    def process_medium_switch_goto(self):
        items = sorted(self.high.items())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  idx>2:
                pptr,(pop,pparam) = items[idx-1]
                ppptr,(ppop,ppparam) = items[idx-2]
                pppptr,(pppop,pppparam) = items[idx-3]
                if  op == "III_NOP" and pop == "III_SCOPE" and ppop == "III_SCOPE_END" and pppop == "III_JMP":
                    if  pparam[0] == "case" and ppparam == 1 and pppparam == ptr:
                        del self.high[pppptr]

    def process_medium_switch_default(self):
        scope_stack = ["<<<ERROR>>>"]
        keys = sorted(self.high.keys())
        for idx,ptr in enumerate(keys):
            op,param = self.high[ptr]
            if op == "III_SCOPE":
                scope_stack += [param]
            elif op == "III_SCOPE_END":
                for i in range(param):
                    parent = scope_stack.pop()
                if  idx>1:
                    pptr,(pop,pparam) = keys[idx-1],self.high[keys[idx-1]]
                    if  op == "III_SCOPE_END" and pop == "III_JMP":
                        if  pparam == ptr and parent[0] == "case" and scope_stack[-1][0] == "switch":
                            target = scope_stack[-1][2]
                            del self.high[pptr]
                            if  self.high[target][0] == "III_NOP" and self.high[ptr+1][0] == "III_NOP":
                                self.high[ptr+1] = ["III_SCOPE",["default",None,pparam,parent]]
                                self.high[target] = ["III_SCOPE_END",1]
                            elif self.high[target][0] == "III_SCOPE_END" and self.high[ptr+1][0] == "III_NOP":
                                self.high[target][1] += 1
                                self.high[ptr+1] = ["III_SCOPE",["default",None,pparam,parent]]
                            else:
                                print("process_medium_if_else: ",self.high[target])
        if  len(scope_stack) > 1:
            print("process_medium_if_else: stack is not empty on exit: ",scope_stack)
        if  len(scope_stack) == 0:
            print("process_medium_if_else: stack is exhausted on exit")

    def process_medium_switch_break(self):
        scope_stack = ["<<<ERROR>>>"]
        items = sorted(self.high.items())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  op == "III_SCOPE_END":
                for i in range(param):
                    scope_stack.pop()
            elif op == "III_SCOPE":
                scope_stack += [param]
            elif op == "III_JMP" and len(scope_stack) > 1:
                if  scope_stack[-1][0] in ["case","default"] and scope_stack[-2][0] in ["switch"]:
                    if  len(scope_stack[-2])>2 and param == scope_stack[-2][2]:
                        self.high[ptr] = ["III_LINE","break;"]
        if  len(scope_stack) > 1:
            print("process_medium_switch_break: stack is not empty on exit: ",scope_stack)
        if  len(scope_stack) == 0:
            print("process_medium_switch_break: stack is exhausted on exit")

    def process_medium_while_break(self):
        scope_stack = ["<<<ERROR>>>"]
        items = sorted(self.high.items())
        for idx,(ptr,(op,param)) in enumerate(items):
            if  op == "III_SCOPE_END":
                for i in range(param):
                    scope_stack.pop()
            elif op == "III_SCOPE":
                scope_stack += [param]
            elif op == "III_JMP" and len(scope_stack) > 1:
                if  scope_stack[-1][0] in ["if"] and scope_stack[-2][0] in ["while"]:
                    if  param > scope_stack[-2][2]:
                        self.high[ptr] = ["III_LINE","break;"]
                if  scope_stack[-1][0] in ["if"] and scope_stack[-2][0] in ["if"] and scope_stack[-3][0] in ["while"]:
                    if  param > scope_stack[-3][2]:
                        self.high[ptr] = ["III_LINE","break;"]
                #TODO: rewrite into something nice
        if  len(scope_stack) > 1:
            print("process_medium_switch_break: stack is not empty on exit: ",scope_stack)
        if  len(scope_stack) == 0:
            print("process_medium_switch_break: stack is exhausted on exit")

    def process_medium_nop_scope_ends(self):
        for ptr,(op,param) in sorted(self.high.items()):
            if  op == "III_SCOPE_END" and not (ptr+1) in self.high:
                self.high[ptr+1] = ["III_NOP",None]

    def process_medium_if_else(self):
        scope_stack = ["<<<ERROR>>>"]
        keys = sorted(self.high.keys())
        for idx,ptr in enumerate(keys):
            op,param = self.high[ptr]
            if op == "III_SCOPE":
                scope_stack += [param]
            elif op == "III_SCOPE_END":
                for i in range(param):
                    parent = scope_stack.pop()
                if  idx>1:
                    pptr,(pop,pparam) = keys[idx-1],self.high[keys[idx-1]]
                    if  op == "III_SCOPE_END" and pop == "III_JMP" and parent[0] == "if":
                        if  pparam > ptr:
                            if  self.high[pparam][0] == "III_NOP" and self.high[ptr+1][0] == "III_NOP":
                                self.high[pparam] = ["III_SCOPE_END",1]
                                self.high[ptr+1] = ["III_SCOPE",["else",None,pparam,parent]]
                                del self.high[pptr]
                            elif self.high[pparam][0] == "III_SCOPE_END" and self.high[ptr+1][0] == "III_NOP":
                                self.high[pparam][1] += 1
                                self.high[ptr+1] = ["III_SCOPE",["else",None,pparam,parent]]
                                del self.high[pptr]
                            else:
                                print("process_medium_if_else: ",self.high[pparam])
                        elif pparam == ptr:
                            # huh?
                            del self.high[pptr]
        if  len(scope_stack) > 1:
            print("process_medium_if_else: stack is not empty on exit: ",scope_stack)
        if  len(scope_stack) == 0:
            print("process_medium_if_else: stack is exhausted on exit")

    def process_final(self):
        self.final_text = []
        for (dllname, fname, ftype, fret, fargs) in self.externals:
            txt = 'external "%s" %s %s %s(%s);' % (dllname, ftype, fret, fname, ', '.join(fargs))
            self.final_text += [txt.replace("  "," ")+"\n"]
        scope_stack = []
        for ptr,(op,param) in sorted(self.high.items()):
            fprefix = ""
            prefix = fprefix + len(scope_stack)*"\t"
            if op == "III_SCOPE":
                if  param[0] == "case":
                    self.final_text += [prefix+"case %s:\n"%param[1]]
                elif param[0] == "default":
                    self.final_text += [prefix+"default:\n"]
                elif param[0] == "event":
                    self.final_text += [prefix+"on \"%s\" {\n"%param[1]]
                elif param[0] in ["method","function"]:
                    self.final_text += [prefix+"%s %s {\n"%(param[0],param[1])]
                elif param[0] in ["if","while","switch"]:
                    self.final_text += [prefix+"%s(%s) {\n"%(param[0],param[1])]
                elif param[0] == "else":
                    self.final_text += [prefix+"else {\n"]
                else:
                    self.final_text += [prefix+"//TODO %s\n"%repr(param)]
                scope_stack += [param]
            elif op == "III_SCOPE_END":
                for i in range(param):
                    parent = scope_stack.pop()
                    if  parent[0] not in ["case","default"]:
                        prefix = fprefix + len(scope_stack)*"\t"
                        self.final_text += [prefix+"}\n"]
            elif op == "III_LINE":
                self.final_text += [prefix+param+"\n"]
            elif op == "III_JMP":
                self.final_text += [prefix+"//TODO: unprocessed goto\n"]
            elif op == "III_RET_EOF":
                if  scope_stack:
                    self.final_text += [prefix+"//TODO: non-empty stack on EOF: %s"%scope_stack+"\n"]
            for i in inc:
                ll = len(inc[i])
                if  len(self.final_text) >= ll:
                    if  all([self.final_text[c-ll] == inc[i][c] for c in range(ll)]):
                        print("include " + i)
                        self.final_text = self.final_text[:-ll] + ['#include "include\\%s"\n' % i]
        self.final_text = "".join(self.final_text)

    def dump_header(self, fn):
        with open(fn,"w") as out:
            for t in self.table_names:
                for i,j in self.tables[t].items():
                    out.write("%s %s: %u\n"%(t,j,i))
            for (dllname, fname, ftype, fret, fargs) in self.externals:
                txt = 'external "%s" %s %s %s(%s);' % (dllname, ftype, fret, fname, ', '.join(fargs))
                out.write(txt.replace("  "," ") + "\n")

    def dump_disasm(self, fn):
        with open(fn,"w") as out:
            for ptr,item in sorted(self.disasm.items()):
                out.write("%d: %s %s\n"%(ptr,item[0],item[1]))

    def dump_medium(self, fn):
        with open(fn,"w") as out:
            for ptr,item in sorted(self.medium.items()):
                out.write("%d: %s %s\n"%(ptr,item[0],item[1]))

    def dump_high(self, fn):
        with open(fn,"w") as out:
            for ptr,item in sorted(self.high.items()):
                out.write("%d: %s %s\n"%(ptr,item[0],item[1]))

    def dump_final(self, fn):
        with open(fn,"w") as out:
            out.write(self.final_text)

print(fn)
with open(fn,"rb") as f:
    wmd = WinterMuteDecompiler(f.read())
    broken_flag = ""

    try:
        wmd.read_header()
    except Exception as ex:
        traceback.print_exc()
        broken_flag = ".broken"
    if  LB_WRITE_HEADERS or broken_flag:
        wmd.dump_header(fn.replace(".script",".wmeheader"+broken_flag))


    try:
        wmd.read_asm()
    except Exception as ex:
        traceback.print_exc()
        broken_flag = ".broken"
        wmd.dump_header(fn.replace(".script",".wmeheader"+broken_flag))
    if  LB_WRITE_DISASM or broken_flag:
        wmd.dump_disasm(fn.replace(".script",".wmeasm"+broken_flag))


    try:
        wmd.create_medium()
    except Exception as ex:
        traceback.print_exc()
        broken_flag = ".broken"
        wmd.dump_header(fn.replace(".script",".wmeheader"+broken_flag))
        wmd.dump_disasm(fn.replace(".script",".wmeasm"+broken_flag))
    if  LB_WRITE_MEDIUM or broken_flag:
        wmd.dump_medium(fn.replace(".script",".wmemedium"+broken_flag))


    try:
        wmd.process_medium()
    except Exception as ex:
        traceback.print_exc()
        broken_flag = ".broken"
        wmd.dump_header(fn.replace(".script",".wmeheader"+broken_flag))
        wmd.dump_disasm(fn.replace(".script",".wmeasm"+broken_flag))
        wmd.dump_medium(fn.replace(".script",".wmemedium"+broken_flag))
    if  LB_WRITE_HIGHLVL or broken_flag:
        wmd.dump_high(fn.replace(".script",".wmehigh"+broken_flag))

    if  LB_WRITE_RESULT or broken_flag:
        wmd.process_final()
        wmd.dump_final(fn.replace(".script",".wmescript"+broken_flag))
