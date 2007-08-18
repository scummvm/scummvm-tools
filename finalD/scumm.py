"""Scumm Disasm and ByteCode"""

import bytecode
import disasm
from iformat import *


## Exceptions ##

class Error(Exception):
    """Base class for exceptions in this module."""
    pass

class VarIndexOutOfRangeError(Error):
    """Error raised when a variable index is out of range."""
    pass

class ArgListVarNumError(Error):
    """Error raised when an argument list holds too many variables."""
    pass

class UnknownSubOpError(Error):
    """Error raised when a sub opcode is unknown."""
    pass

class InvalidExpressionCodeError(Error):
    """Error raised when an expression code is unknown."""
    pass

class ScriptTooSmallError(Error):
    """Error raised when script is too small."""
    pass

class UnknownScriptTypeError(Error):
    """Error raised when script type is unknown."""
    pass


## Disassembler for SCUMM scripts ##

class DisSCUMM(disasm.Disasm):
    """Disassembler for SCUMM."""
    pass


## Globals ##

_var_names0 = ("VAR_EGO",
               "VAR_RESULT",
               "VAR_CAMERA_POS_X",
               "VAR_HAVE_MSG",
               # 4
               "VAR_ROOM",
               "VAR_ACTIVE_ACTOR",
               "VAR_OVERRIDE",
               None,
               # 8
               "VAR_IS_SOUND_RUNNING",
               "VAR_ACTIVE_VERB",
               "VAR_CHARCOUNT",
               None)

_var_names2 = ("VAR_EGO",
               "VAR_RESULT",
               "VAR_CAMERA_POS_X",
               "VAR_HAVE_MSG",
               # 4
               "VAR_ROOM",
               "VAR_OVERRIDE",
               "VAR_MACHINE_SPEED",
               "VAR_CHARCOUNT",
               # 8
               "VAR_ACTIVE_VERB",
               "VAR_ACTIVE_OBJECT1",
               "VAR_ACTIVE_OBJECT2",
               "VAR_NUM_ACTOR",
               # 12
               "VAR_CURRENT_LIGHTS",
               "VAR_CURRENTDRIVE",
               None,
               None,
               # 16
               None,
               "VAR_MUSIC_TIMER",
               "VAR_VERB_ALLOWED",
               "VAR_ACTOR_RANGE_MIN",
               # 20
               "VAR_ACTOR_RANGE_MAX",
               None,
               None,
               "VAR_CAMERA_MIN_X",
               # 24
               "VAR_CAMERA_MAX_X",
               "VAR_TIMER_NEXT",
               "VAR_SENTENCE_VERB",
               "VAR_SENTENCE_OBJECT1",
               # 28
               "VAR_SENTENCE_OBJECT2",
               "VAR_SENTENCE_PREPOSITION",
               "VAR_VIRT_MOUSE_X",
               "VAR_VIRT_MOUSE_Y",
               # 32
               "VAR_CLICK_AREA",
               "VAR_CLICK_VERB",
               None,
               "VAR_CLICK_OBJECT",
               # 36
               "VAR_ROOM_RESOURCE",
               "VAR_LAST_SOUND",
               "VAR_BACKUP_VERB",
               "VAR_KEYPRESS",
               # 40
               "VAR_CUTSCENEEXIT_KEY",
               "VAR_TALK_ACTOR",
               None,
               None)

_var_names3 = ("VAR_RESULT",
               "VAR_EGO",
               "VAR_CAMERA_POS_X",
               "VAR_HAVE_MSG",
               # 4
               "VAR_ROOM",
               "VAR_OVERRIDE",
               "VAR_MACHINE_SPEED",
               "VAR_ME",
               # 8
               "VAR_NUM_ACTOR",
               "VAR_CURRENT_LIGHTS",
               "VAR_CURRENTDRIVE",
               "VAR_TMR_1",
               # 12
               "VAR_TMR_2",
               "VAR_TMR_3",
               "VAR_MUSIC_TIMER",
               "VAR_ACTOR_RANGE_MIN",
               # 16
               "VAR_ACTOR_RANGE_MAX",
               "VAR_CAMERA_MIN_X",
               "VAR_CAMERA_MAX_X",
               "VAR_TIMER_NEXT",
               # 20
               "VAR_VIRT_MOUSE_X",
               "VAR_VIRT_MOUSE_Y",
               "VAR_ROOM_RESOURCE",
               "VAR_LAST_SOUND",
               # 24
               "VAR_CUTSCENEEXIT_KEY",
               "VAR_TALK_ACTOR",
               "VAR_CAMERA_FAST_X",
               None,
               # 28
               "VAR_ENTRY_SCRIPT",
               "VAR_ENTRY_SCRIPT2",
               "VAR_EXIT_SCRIPT",
               "VAR_EXIT_SCRIPT2",
               # 32
               "VAR_VERB_SCRIPT",
               "VAR_SENTENCE_SCRIPT",
               "VAR_INVENTORY_SCRIPT",
               "VAR_CUTSCENE_START_SCRIPT",
               # 36
               "VAR_CUTSCENE_END_SCRIPT",
               "VAR_CHARINC",
               "VAR_WALKTO_OBJ",
               None,
               # 40
               None,
               None,
               "VAR_RESTART_KEY",
               "VAR_PAUSE_KEY",
               # 44
               "VAR_MOUSE_X",
               "VAR_MOUSE_Y",
               "VAR_TIMER",
               "VAR_TMR_4",
               # 48
               "VAR_SOUNDCARD",
               "VAR_VIDEOMODE",
               None,
               None)

_var_names4 = ("VAR_RESULT",
               "VAR_EGO",
               "VAR_CAMERA_POS_X",
               "VAR_HAVE_MSG",
               # 4
               "VAR_ROOM",
               "VAR_OVERRIDE",
               "VAR_MACHINE_SPEED",
               "VAR_ME",
               # 8
               "VAR_NUM_ACTOR",
               "VAR_CURRENT_LIGHTS",
               "VAR_CURRENTDRIVE",
               "VAR_TMR_1",
               # 12
               "VAR_TMR_2",
               "VAR_TMR_3",
               "VAR_MUSIC_TIMER",
               "VAR_ACTOR_RANGE_MIN",
               # 16
               "VAR_ACTOR_RANGE_MAX",
               "VAR_CAMERA_MIN_X",
               "VAR_CAMERA_MAX_X",
               "VAR_TIMER_NEXT",
               # 20
               "VAR_VIRT_MOUSE_X",
               "VAR_VIRT_MOUSE_Y",
               "VAR_ROOM_RESOURCE",
               "VAR_LAST_SOUND",
               # 24
               "VAR_CUTSCENEEXIT_KEY",
               "VAR_TALK_ACTOR",
               "VAR_CAMERA_FAST_X",
               "VAR_SCROLL_SCRIPT",
               # 28
               "VAR_ENTRY_SCRIPT",
               "VAR_ENTRY_SCRIPT2",
               "VAR_EXIT_SCRIPT",
               "VAR_EXIT_SCRIPT2",
               # 32
               "VAR_VERB_SCRIPT",
               "VAR_SENTENCE_SCRIPT",
               "VAR_INVENTORY_SCRIPT",
               "VAR_CUTSCENE_START_SCRIPT",
               # 36
               "VAR_CUTSCENE_END_SCRIPT",
               "VAR_CHARINC",
               "VAR_WALKTO_OBJ",
               "VAR_DEBUGMODE",
               # 40
               "VAR_HEAPSPACE",
               None,
               "VAR_RESTART_KEY",
               "VAR_PAUSE_KEY",
               # 44
               "VAR_MOUSE_X",
               "VAR_MOUSE_Y",
               "VAR_TIMER",
               "VAR_TMR_4",
               # 48
               "VAR_SOUNDCARD",
               "VAR_VIDEOMODE",
               "VAR_MAINMENU_KEY",
               "VAR_FIXEDDISK",
               # 52
               "VAR_CURSORSTATE",
               "VAR_USERPUT",
               "VAR_V5_TALK_STRING_Y",
               None,
               # 56
               None,
               None,
               None,
               None,
               # 60
               "VAR_NOSUBTITLES",
               None,
               None,
               None,
               # 64
               "VAR_SOUNDPARAM",
               "VAR_SOUNDPARAM2",
               "VAR_SOUNDPARAM3",
               None)

_var_names5 = ("VAR_RESULT",
               "VAR_EGO",
               "VAR_CAMERA_POS_X",
               "VAR_HAVE_MSG",
               # 4
               "VAR_ROOM",
               "VAR_OVERRIDE",
               "VAR_MACHINE_SPEED",
               "VAR_ME",
               # 8
               "VAR_NUM_ACTOR",
               "VAR_CURRENT_LIGHTS",
               "VAR_CURRENTDRIVE",
               "VAR_TMR_1",
               # 12
               "VAR_TMR_2",
               "VAR_TMR_3",
               "VAR_MUSIC_TIMER",
               "VAR_ACTOR_RANGE_MIN",
               # 16
               "VAR_ACTOR_RANGE_MAX",
               "VAR_CAMERA_MIN_X",
               "VAR_CAMERA_MAX_X",
               "VAR_TIMER_NEXT",
               # 20
               "VAR_VIRT_MOUSE_X",
               "VAR_VIRT_MOUSE_Y",
               "VAR_ROOM_RESOURCE",
               "VAR_LAST_SOUND",
               # 24
               "VAR_CUTSCENEEXIT_KEY",
               "VAR_TALK_ACTOR",
               "VAR_CAMERA_FAST_X",
               "VAR_SCROLL_SCRIPT",
               # 28
               "VAR_ENTRY_SCRIPT",
               "VAR_ENTRY_SCRIPT2",
               "VAR_EXIT_SCRIPT",
               "VAR_EXIT_SCRIPT2",
               # 32
               "VAR_VERB_SCRIPT",
               "VAR_SENTENCE_SCRIPT",
               "VAR_INVENTORY_SCRIPT",
               "VAR_CUTSCENE_START_SCRIPT",
               # 36
               "VAR_CUTSCENE_END_SCRIPT",
               "VAR_CHARINC",
               "VAR_WALKTO_OBJ",
               "VAR_DEBUGMODE",
               # 40
               "VAR_HEAPSPACE",
               None,
               "VAR_RESTART_KEY",
               "VAR_PAUSE_KEY",
               # 44
               "VAR_MOUSE_X",
               "VAR_MOUSE_Y",
               "VAR_TIMER",
               "VAR_TMR_4",
               # 48
               "VAR_SOUNDCARD",
               "VAR_VIDEOMODE",
               "VAR_MAINMENU_KEY",
               "VAR_FIXEDDISK",
               # 52
               "VAR_CURSORSTATE",
               "VAR_USERPUT",
               None,
               None,
               # 56
               "VAR_SOUNDRESULT",
               "VAR_TALKSTOP_KEY",
               None,
               "VAR_FADE_DELAY",
               # 60
               "VAR_NOSUBTITLES",
               None,
               None,
               None,
               # 64
               "VAR_SOUNDPARAM",
               "VAR_SOUNDPARAM2",
               "VAR_SOUNDPARAM3",
               "VAR_INPUTMODE",
               # 68
               "VAR_MEMORY_PERFORMANCE",
               "VAR_VIDEO_PERFORMANCE",
               "VAR_ROOM_FLAG",
               "VAR_GAME_LOADED",
               # 72
               "VAR_NEW_ROOM",
               None,
               None,
               None,
               # 76
               None,
               None,
               None,
               None)

pmasks = (0x80, 0x40, 0x20)  # paramater bit masks


## ByteCode implementations ##

class SCUMM(bytecode.ByteCode):
    """SCUMM bytecode variants base."""

    # get_complex constants
    b = 0      # byte
    w = 1      # word
    v = 2      # var
    vob = 3    # var or byte
    vow = 4    # var or word
    asc = 5    # ascii
    lst = 6    # list
    dps = 7    # decode parse string

    def __init__(self, init, version,
                 zak_flag=False, indy_flag=False,
                 unblocked=False, halt_on_error=True):
        self.version = version
        self.zak_flag = zak_flag
        self.indy_flag = indy_flag
        self.unblocked = unblocked
        self.halt_on_error = halt_on_error
        self.script_start = 0

        # called last because ByteCode.__init__() calls self.populate()
        bytecode.ByteCode.__init__(self, init, SCUMM.get_byte)

    def get_pos(self):
        """Return self._pos - self.script_start."""
        return self._pos - self.script_start

    def skip_verb_header(self, off, ofetch, cfetch=0):
        min_offset = 255
        offset = off
        cfetch = cfetch or self.fetch_u8
        self.seek(offset)
        print "Events:"
        code = cfetch()
        while code != 0:
            offset = ofetch()
            print "  %2X - %.4X" % (code, offset)
            if min_offset > offset:
                min_offset = offset
            code = cfetch()
        try:
            self.seek(min_offset, 1)
        except bytecode.OutOfBoundsError:
            self.seek(len(self), 1)

    def parse_header(self):
        def calc_magic(string):
            m = 0
            bits = len(string) * 8
            for b in map(ord, string):
                bits -= 8
                m |= b << bits
            return m
        ssize = len(self)
        if self.unblocked:
            if ssize < 4:
                raise ScriptTooSmallError
            if self.fetch_le_u32(True) == len(self):
                if self.version == 0:
                    self.skip_verb_header(14, self.fetch_u8)
                elif self.version <= 2:
                    self.skip_verb_header(15, self.fetch_u8)
                else:
                    self.skip_verb_header(17, self.fetch_le_u16)
            else:
                self.seek(4)
        elif self.version >= 5:
            if ssize < ((self.version == 5 and 8) or 9):
                raise ScriptTooSmallError
            magic = self.fetch_be_u32(True)
            if magic == calc_magic('LSC2'):
                if ssize <= 12:
                    raise ScriptTooSmallError
                self.seek(8)
                print "Script# %d" % self.fetch_le_u32()
            elif magic == calc_magic('LSCR'):
                if self.version == 8:
                    if ssize <= 12:
                        raise ScriptTooSmallError
                    self.seek(8)
                    print "Script# %d" % self.fetch_le_u32()
                elif self.version == 7:
                    if ssize <= 10:
                        raise ScriptTooSmallError
                    self.seek(8)
                    print "Script# %d" % self.fetch_le_u16()
                else:
                    if ssize <= 9:
                        raise ScriptTooSmallError
                    self.seek(8)
                    print "Script# %d" % self.fetch_u8()
            elif magic == calc_magic('SCRP'):
                self.seek(8)
            elif magic == calc_magic('ENCD'):
                self.seek(8)
            elif magic == calc_magic('EXCD'):
                self.seek(8)
            elif magic == calc_magic('VERB'):
                if self.version == 8:
                    self.seek(8)  # funny ...
                    self.skip_verb_header(0, self.fetch_le_u32, self.fetch_le_u32)
                else:
                    self.skip_verb_header(8, self.fetch_le_u16)
            else:
                raise UnknownScriptTypeError
        else:
            if ssize < 6:
                raise ScriptTooSmallError
            self.seek(4)
            magic = self.fetch_be_u16(True)
            self.seek(0, 1)
            if magic == calc_magic('LS'):
                self.seek(6)
                print "Script# %d" % self.fetch_u8()
            elif magic == calc_magic('SC'):
                self.seek(6)
            elif magic == calc_magic('EN'):
                self.seek(6)
            elif magic == calc_magic('EX'):
                self.seek(6)
            elif magic == calc_magic('OC'):
                self.skip_verb_header(19, self.fetch_le_u16)
            else:
                raise UnknownScriptTypeError
        self.script_start = self.get_pos()

    def get_byte(self):
        return self.fetch_u8()

    def get_word(self):
        return self.fetch_le_u16()

    def get_num_string(self, i):
        s = ""
        if i & 0x8000:
            i &= 0xfff
            if i >= 0x800:
                s = "??Bit??"
            else:
                s = "Bit"
        elif i & 0x4000:
            i &= 0xfff
            if i > 0x10:
                s = "??Local??"
            else:
                s = "Local"
        else:
            i &= 0xfff
            if i >= 0x320:
                s = "??Var??"
            else:
                s = "Var"
        if self.halt_on_error and s[0] == '?':
            raise VarIndexOutOfRangeError, "%s, was %d" % (s, i)
        return s

    def get_var(self):
        i = 0
        if self.version <= 2:
            i = self.get_byte()
        else:
            i = self.get_word()
        assert(i >= 0)

        ret = ''
        if self.version >= 5 and i < len(_var_names5) and \
                _var_names5[i]:
            return _var_names5[i]
        elif self.version >= 4 and i < len(_var_names4) and \
                _var_names4[i]:
            return _var_names4[i]
        elif self.version >= 3 and i < len(_var_names3) and \
                _var_names3[i]:
            return _var_names3[i]
        elif self.version >= 1 and i < len(_var_names2) and \
                _var_names2[i]:
            return _var_names2[i]
        elif self.version >= 0 and i < len(_var_names0) and \
                _var_names0[i]:
            return _var_names0[i]
        elif self.version <= 2 and self.zak_flag and \
                (i == 234 or i == 235):
            return (i == 234 and "ZERO") or "ONE"
        elif (i & 0x8000) and (self.unblocked or self.zak_flag):
            ret = "Var[%d Bit %d" % ((i & 0x0fff) >> 4, i & 0x000f)
        else:
            ret = "%s[%d" % (self.get_num_string(i), i & 0xfff)

        if i & 0x2000:
            j = self.get_word()
            if j & 0x2000:
                j ^= 0x2000
                ret += " + %s[%d]" % (self.get_num_string(j), j & 0xfff)
            else:
                ret += " + %d" % (j & 0xfff)

        ret += "]"
        return ret

    def get_var_or_byte(self, op, mask):
        if op & mask:
            return self.get_var()
        else:
            return self.get_byte()

    def get_var_or_word(self, op, mask):
        if op & mask:
            return self.get_var()
        else:
            return self.get_word()

    def get_list(self):
        first = 1
        j = 0
        ret = '['
        while 1:
            i = self.get_byte()
            if i == 0xff:
                break
            if not first:
                ret += ', '
            first = 0
            ret += str(self.get_var_or_word(i, pmasks[0]))
            j += 1
            if j > 16:
                if self.halt_on_error:
                    raise ArgListVarNumError, \
                        "too many variables in argument list"
                break
        ret += ']'
        return ret

    def get_ascii(self):
        def pascii(i):
            if i > 31 and i < 128:
                return chr(i)
            else:
                return "^%d" % i
        buf = '"'
        while 1:
            i = self.get_byte()
            if not i:
                break
            buf += pascii(i)
            if i == 0xff:
                i = self.get_byte()
                buf += pascii(i)
                # Workaround for a script bug in Indy3
                if i == 46 and self.version == 3 and self.indy_flag:
                    continue

                if i != 1 and i != 2 and i != 3 and i != 8:
                    buf += pascii(self.get_byte())
                    buf += pascii(self.get_byte())
        buf += '"'
        return buf

    def decode_parse_string(self):
        buf = '"'
        c = self.get_byte()
        while c:
            flag = (c & 0x80) != 0
            c &= 0x7f
            if c < 8:
                buf += "^%d" % c
                if c > 3:
                    buf += "^%d" % self.get_byte()
            else:
                buf += chr(c)
            if flag:
                buf += ' '
            c = self.get_byte()
        buf += '"'
        return buf

    def get_complex(self, opcode, specs):
        vals = []
        mask_index = 0
        for spec in specs:
            if spec == self.b:
                vals.append(self.get_byte())
            elif spec == self.w:
                vals.append(self.get_word())
            elif spec == self.v:
                vals.append(self.get_var())
            elif spec == self.vob:
                vals.append(self.get_var_or_byte(opcode, pmasks[mask_index]))
                mask_index += 1
            elif spec == self.vow:
                vals.append(self.get_var_or_word(opcode, pmasks[mask_index]))
                mask_index += 1
            elif spec == self.asc:
                vals.append(self.get_ascii())
            elif spec == self.lst:
                vals.append(self.get_list())
            elif spec == self.dps:
                vals.append(self.decode_parse_string())
        return vals

    def produce_instr(self, name, opcode, *specs):
        return Instr(name, self.get_complex(opcode, specs))

    def get_masks(self, specs):
        maskss = ((0x00,),
                  (0x00, 0x80),
                  (0x00, 0x80, 0x40, 0x80 | 0x40),
                  (0x00, 0x80, 0x40, 0x20,
                   0x80 | 0x40 | 0x20, 0x80 | 0x40, 0x80 | 0x20, 0x40 | 0x20))
        lspecs = list(specs)
        npbits = lspecs.count(self.vob)+lspecs.count(self.vow)
        assert(npbits <= 3)
        return maskss[npbits]

    def register_complex(self, name, base_code, *specs):
        def handler(opcode):
            return Instr(name, self.get_complex(opcode, specs))
        for mask in self.get_masks(specs):
            self.register_opcode(base_code | mask, handler)

    def register_complex_set(self, name, base_code, *specs):
        if not specs: specs = [self.vob]
        def handler(opcode):
            return AssgnInstr('=',
                              self.get_var(),
                              Instr(name, self.get_complex(opcode, specs)))
        for mask in self.get_masks(specs):
            self.register_opcode(base_code | mask, handler)

    def register_simple_set(self, name, opcodes, *specs):
        def handler(opcode):
            return AssgnInstr('=',
                              self.get_var(),
                              Instr(name, self.get_complex(opcode, specs)))
        self.register_opcodes(opcodes, handler)

    def register_simple(self, name, opcodes, *specs):
        def handler(opcode):
            return Instr(name, self.get_complex(opcode, specs))
        self.register_opcodes(opcodes, handler)

    def calc_abs_jump(self, relative):
        return 0x7fff & (relative + self.get_pos())

    def make_jump(self, condition=0):
        offset = self.get_word()
        to = self.calc_abs_jump(offset)
        if condition:
            return NegCondJump(to, condition)
        else:
            return Jump(to)

    def register_if(self, name, base_code, *specs):
        def handler(opcode):
            return self.make_jump(Instr(name, self.get_complex(opcode, specs)))
        for mask in self.get_masks(specs):
            self.register_opcode(base_code | mask, handler)


class SCUMM12(SCUMM):
    """ByteCode class for SCUMM versions 1 and 2."""

    def populate(self):
        """Populate self.optable with opcode handlers for versions 1 and 2."""
        self.register_simple("stopObjectCode", [0x00, 0xa0])
        self.register_complex("putActor", 0x01, self.vob, self.vob, self.vob)
        self.register_complex("startMusic", 0x02, self.vob)
        self.register_complex("startSound", 0x1c, self.vob)
        self.register_complex_set("getActorRoom", 0x03)
        self.register_opcodes([0x04, 0x84,
                               0x48, 0xc8,
                               0x78, 0xf8,
                               0x44, 0xc4,
                               0x08, 0x88,
                               0x38, 0xb8,
                               0x28, 0xa8], self.do_if_code)
        self.register_complex("drawObject", 0x05, self.vow, self.vob, self.vob)
        self.register_complex_set("getActorElevation", 0x06)
        # UNCLEAR: descumm and scummvm are in conflict over the second argument
        # based on scummvm
        self.register_complex("faceActor", 0x09, self.vob, self.vow)
        self.register_opcodes([0x2A, 0xAA, 0x3A, 0xBA,
                               0x6A, 0xEA, 0x0A, 0x8A,
                               0x1A, 0x5A, 0x9A, 0xDA], self.do_varset)
        self.register_complex("setObjPreposition", 0x0b, self.vow, self.b)
        self.register_complex("setObjPreposition", 0x4b, self.vow, self.b)
        self.register_opcodes([0x0c, 0x8c], self.do_resource)
        self.register_complex("walkActorToActor", 0x0d, self.vob, self.vob, self.b)
        self.register_complex("putActorAtObject", 0x0e, self.vob, self.vow)
        self.register_opcodes([0x3F, 0xBF,
                               0x5F, 0xDF,
                               0x2F, 0xAF,
                               0x0F, 0x8F,
                               0x7F, 0xFF,
                               0x1F, 0x9F,
                               0x6F, 0xEF,
                               0x4F, 0xCF], self.do_if_state_code)
        self.register_complex_set("getObjectOwner", 0x10, self.vow)
        self.register_complex("animateActor", 0x11, self.vob, self.vob)
        self.register_complex("panCameraTo", 0x12, self.vob)
        self.register_opcodes([0x13, 0x53, 0x93, 0xd3], self.do_actor_ops)
        self.register_complex("beginOverride", 0x58)
        self.register_complex("actorFollowCamera", 0x52, self.vob)
        self.register_complex_set("actorFromPos", 0x15, self.vob, self.vob)
        self.register_opcode(0x2c, self.do_varset)
        self.register_complex("breakHere", 0x80)
        self.register_complex("chainScript", 0x4a, self.vob)
        self.register_opcodes([0x60, 0xe0], self.do_cursor_command)
        self.register_complex("cutscene", 0x40)
        self.register_complex("endCutscene", 0xc0)
        self.register_opcodes([0x46, 0xc6], self.do_varset)
        self.register_opcode(0x2e, self.do_delay)
        self.register_complex("delayVariable", 0x2b, self.v)
        self.register_opcodes([0x19, 0x39, 0x59, 0x79,
                               0x99, 0xb9, 0xd9, 0xf9],
                              self.do_sentence)
        self.register_complex("drawSentence", 0xac)
        self.register_opcodes([0x5c, 0x6b, 0x6e, 0xab, 0xdc, 0xeb, 0xee],
                              self.do_dummy)
        self.register_complex("findObject", 0x35, self.vow, self.vow)
        self.register_complex_set("getActorCostume", 0x71)
        self.register_complex_set("getActorFacing", 0x63)
        self.register_complex_set("getActorMoving", 0x56)
        self.register_complex_set("getActorWalkBox", 0x7b)
        self.register_complex_set("getActorX", 0x43)
        self.register_complex_set("getActorY", 0x23)
        self.register_complex_set("getBitVar", 0x31, self.w, self.vob)
        self.register_complex_set("setBitVar", 0x1b, self.w, self.vob, self.vob)
        self.register_complex_set("getClosestObjActor", 0x66, self.vow)
        self.register_complex_set("getDist", 0x34, self.vow, self.vow)
        self.register_complex_set("getObjPreposition", 0x6c, self.vow)
        self.register_complex_set("getRandomNr", 0x16)
        self.register_if("classOfIs", 0x1d, self.vow, self.vob)
        self.register_complex_set("isScriptRunning", 0x68)
        self.register_complex_set("isSoundRunning", 0x7c)
        self.register_opcode(0x18, self.do_jump)
        self.register_complex("lights", 0x70, self.vob, self.b, self.b)
        self.register_complex("loadRoom", 0x72, self.vob)
        self.register_complex("loadRoomWithEgo", 0x24,
                              self.vow, self.vob, self.b, self.b)
        self.register_complex("setBoxFlags", 0x30, self.vob, self.b)
        self.register_complex("pickupObject", 0x50, self.vow)
        self.register_complex("print", 0x14, self.vob, self.dps)
        self.register_complex("printEgo", 0xd8, self.dps)
        self.register_opcode(0xcc, self.do_pseudo_room)
        self.register_complex("putActorInRoom", 0x2d, self.vob, self.vob)
        self.register_complex("restart", 0x98)
        self.register_opcodes([0x33, 0x73, 0xb3, 0xf3], self.do_room_ops_old)
        self.register_complex_set("saveLoadGame", 0x22, self.vob)
        self.register_complex("setActorElevation", 0x3d, self.vob, self.vob)
        self.register_complex("setCameraAt", 0x32, self.vob)
        self.register_complex("setObjectName", 0x54, self.vow, self.asc)
        self.register_complex("setOwnerOf", 0x29, self.vow, self.vob)
        self.register_complex("clearState01", 0x77, self.vow)
        self.register_complex("clearState02", 0x17, self.vow)
        self.register_complex("clearState04", 0x67, self.vow)
        self.register_complex("clearState08", 0x47, self.vow)
        self.register_complex("setState01", 0x37, self.vow)
        self.register_complex("setState02", 0x57, self.vow)
        self.register_complex("setState04", 0x27, self.vow)
        self.register_complex("setState08", 0x07, self.vow)
        self.register_opcodes([0x26, 0xa6], self.do_var_range)
        self.register_complex("stopScript", 0x62, self.vob)
        self.register_complex("stopSound", 0x3c, self.vob)
        self.register_complex("stopMusic", 0x20)
        self.register_opcodes([0x7a, 0xfa], self.do_verbops)
        self.register_complex("waitForActor", 0x3b, self.vob)
        self.register_complex("waitForSentence", 0x4c)
        self.register_complex("waitForMessage", 0xae)
        self.register_complex("walkActorTo", 0x1e, self.vob, self.vob, self.vob)
        self.register_complex("walkActorToObject", 0x36, self.vob, self.vow)
        self.register_complex("startScript", 0x42, self.vob)

    def do_var_range(self, opcode):
        var = self.get_var()
        i = self.get_byte()
        nums = []
        j = i
        while j > 0:
            if opcode & 0x80:
                nums.append(self.get_word())
            else:
                nums.append(self.get_byte())
            j -= 1
        return Instr("setVarRange", [var, i, nums])

    def do_jump(self, _):
        return self.make_jump()

    def do_dummy(self, opcode):
        return Instr("dummy", ["%.2X" % opcode])

    def do_sentence(self, opcode):
        if (not (opcode & 0x80)) and self.fetch_u8(True) == 0xfc:
            args = ["STOP"]
            self.seek(1)
        elif (not (opcode & 0x80)) and self.fetch_u8(True) == 0xfb:
            args = ["Reset"]
            self.seek(1)
        else:
            args = self.get_complex(opcode,
                                    [self.vob, self.vow,
                                     self.vow, self.b])
        return Instr("doSentence", args)

    def do_delay(self, _):
        d = self.get_byte()
        d |= self.get_byte() << 8
        d |= self.get_byte() << 16
        d = 0xffffff - d
        return Instr("delay", [d])

    def do_cursor_command(self, opcode):
        if opcode & 0x80:
            tmp = str(self.get_var())
            return Instr("cursorCommand", ["Hi(%s)" % tmp, "Lo(%s)" % tmp])
        else:
            tmp = self.get_word()
            return Instr("cursorCommand", [(tmp >> 8) & 0xff, tmp & 0xff])

    def do_actor_ops(self, opcode):
        args = [self.get_var_or_byte(opcode, pmasks[0])]
        arg2 = self.get_var_or_byte(opcode, pmasks[1])

        subop = self.get_byte()
        if subop == 1:
            args.append(Instr("Sound", [arg2]))
        elif subop == 2:
            if self.version == 1:
                args.append(Instr("Color", [arg2]))
            else:
                args.append(Instr("Color", [self.get_byte(), arg2]))
        elif subop == 3:
            args.append(Instr("Name", [self.get_ascii()]))
        elif subop == 4:
            args.append(Instr("Costume", [arg2]))
        elif subop == 5:
            args.append(Instr("TalkColor", [arg2]))
        else:
            raise UnknownSubOpError, \
                "SCUMM12.do_actor_ops: %d" % subop
        return Instr("ActorOps", args)

    def do_resource(self, opcode):
        res_types = ("UnkResType0",
                     "UnkResType1",
                     "Costume",
                     "Room",
                     "UnkResType4",
                     "Script",
                     "Sound")
        resid = self.get_var_or_byte(opcode, pmasks[0])
        subop = self.get_byte()
        typ = subop >> 4
        assert(0 <= typ < len(res_types))
        if (subop & 0x0f) == 0 or (subop & 0x0f) == 1:
            if subop & 1:
                buf = "load"
            else:
                buf = "nuke"
        else:
            if subop & 1:
                buf = "lock"
            else:
                buf = "unlock"
        return Instr(buf + res_types[typ], [resid])

    def do_pseudo_room(self, _):
        args = [self.get_byte()]
        while 1:
            j = self.get_byte()
            if not j:
                break
            if j & 128:
                args.append(j & 127)
            else:
                args.append('IG')
        return Instr("PseudoRoom", args)

    def do_room_ops_old(self, opcode):
        if self.version <= 2:
            a, b = self.get_complex(opcode, [self.vob, self.vob])
        elif self.version == 3:
            a, b = self.get_complex(opcode, [self.vow, self.vow])
        opc = self.get_byte()
        opcm = opc & 0x1f
        if 0x01 <= opcm <= 0x04:
            if self.version > 3:
                a, b = self.get_complex(opc, [self.vow, self.vow])
            return Instr({0x01 : "RoomScroll",
                          0x02 : "RoomColor",
                          0x03 : "SetScreen",
                          0x04 : "SetPalColor"}[opcm],
                         [a, b])
        elif 0x05:
            return Instr("ShakeOn", [])
        elif 0x06:
            return Instr("ShakeOff", [])
        else:
            raise UnknownSubOpError, \
                "SCUMM12.do_room_ops_old: unknown subop %d" % opcm

    def do_verbops(self, opcode):
        subop = self.get_byte()
        if subop == 0x00:
            return self.produce_instr("VerbOpsDelete", opcode, self.vob)
        elif subop == 0xff:
            return self.produce_instr("VerbOpsState", opcode, self.b, self.b)
        else:
            return self.produce_instr("VerbOpsNew-%d" % subop, opcode,
                                      self.b, self.b, self.vob,
                                      self.b, self.asc)

    def do_varset(self, opcode):
        if self.version <= 2 and ((opcode & 0x7f) == 0x0a or
                                  (opcode & 0x7f) == 0x2a or
                                  (opcode & 0x7f) == 0x6a):
            dst = "Var[Var[%d]]" % self.get_byte()
        else:
            dst = self.get_var()
        opcm = opcode & 0x7f
        if opcm in (0x0a, 0x1a, 0x2c):
            asgop = '='
        elif opcm == 0x1b:
            asgop = '*='
        elif opcm in (0x3a, 0x6a) or (opcm == 0x46 and opcode & 128):
            asgop = '-='
        elif opcm == 0x57:
            asgop = '|='
        elif opcm in (0x2a, 0x5a) or opcm == 0x46:
            asgop = '+='
        elif opcm == 0x5b:
            asgop = '/='
        elif opcm == 0x17:
            asgop = '&='
        else:
            raise UnknownSubOpError, \
                "SCUMM12.do_varset: unknown varset %d" % opcode
        if self.version <= 2 and opcm == 0x2c:
            val = self.get_byte()
        elif opcm == 0x46:
            val = 1
        elif self.version == 0:
            val = self.get_var_or_byte(opcode, pmasks[0])
        else:
            val = self.get_var_or_word(opcode, pmasks[0])
        return AssgnInstr(asgop, dst, val)

    def do_if_code(self, opcode):
        cmp_texts = (' >= ', ' < ', ' <= ', ' > ', ' == ', ' != ', '!', '')
        if opcode in (0x28, 0xa8):
            a = ''
        else:
            a = self.get_var()
        opcm = opcode & 0x7f
        # The following might raise a KeyError exception
        comp = cmp_texts[{ 0x38 : 0,
                           0x04 : 2,
                           0x08 : 5,
                           0x48 : 4,
                           0x78 : 1,
                           0x44 : 3,
                           0x28 : ((opcode & 128) and 7) or 6 }[opcm]]
        if opcode in (0x28, 0xa8):
            b = self.get_var()
        elif self.version == 0:
            b = self.get_var_or_byte(opcode, pmasks[0])
        else:
            b = self.get_var_or_word(opcode, pmasks[0])
        return self.make_jump("%s%s%s" % (str(a), comp, str(b)))

    def do_if_state_code(self, opcode):
        var = ''
        if self.version == 0:
            if opcode & 0x40:
                var = 'activeObject'
            else:
                var = self.get_byte()
        else:
            var = self.get_var_or_word(opcode, pmasks[0])
        if self.version > 2:
            opcm = opcode & 0x2f
            if opcm == 0x0f:
                neg = 0
            elif opcm == 0x2f:
                neg = 1
            else:
                raise Error, "SCUMM12.do_if_state_code .. weird"
            tmp = self.get_var_or_byte(opcode, pmasks[1])
        else:
            if self.version == 0:
                # The following might rais a KeyError exception
                state, neg = { 0x7f : (2, 1), 0xbf : (2, 1),
                               0x9f : (4, 1), 0xdf : (4, 1),
                               0xaf : (8, 1), 0xef : (8, 1),
                               0x3f : (2, 0), 0xff : (2, 0),
                               0x1f : (4, 0), 0x5f : (4, 0),
                               0x2f : (8, 0), 0x6f : (8, 0) }[opcode]
            else:
                state, neg = { 0x3f : (1, 1), 0xbf : (1, 1),
                               0x5f : (2, 1), 0xdf : (2, 1),
                               0x2f : (4, 1), 0xaf : (4, 1),
                               0x0f : (8, 1), 0x8f : (8, 1),
                               0x7f : (1, 0), 0xff : (1, 0),
                               0x1f : (2, 0), 0x9f : (2, 0),
                               0x6f : (4, 0), 0xef : (4, 0),
                               0x4f : (8, 0), 0xcf : (8, 0) }[opcode]
        if self.version > 2:
            return self.make_jump("getState(%s)%s%s" % \
                                  (var, (neg and ' != ') or ' == ', tmp))
        return self.make_jump("%sgetState%02d(%s)" % \
                              ((neg and '!') or '', state, var))


class SCUMM0(SCUMM12):
    """Bytecode class for SCUMM version 0."""

    def populate(self):
        """Populate self.optable with opcode handlers for version 0."""
        SCUMM12.populate(self)

        self.register_simple("doSentence", [0x03, 0x43, 0x83, 0xc3],
                             self.b, self.b, self.b)
        self.register_simple("stopCurrentScript",
                             [0x05, 0x09, 0x0A, 0x19, 0x23, 0x2C,
                              0x35, 0x39, 0x3B, 0x45, 0x49, 0x59,
                              0x63, 0x65, 0x6A, 0x6C, 0x79, 0x7A,
                              0x7B, 0x80, 0x82, 0x85, 0x89, 0x8A,
                              0x8D, 0x96, 0x99, 0xA3, 0xA6, 0xAA,
                              0xAC, 0xB5, 0xB9, 0xBB, 0xC5, 0xC9,
                              0xD8, 0xD9, 0xE3, 0xE6, 0xEA, 0xEC,
                              0xF5, 0xF9, 0xFA, 0xFB])
        self.register_complex_set("getDist", 0x34, self.vob, self.vob)
        self.register_simple_set("getDist", 0x06, self.vob, self.vob)
        self.register_simple_set("getActorRoom", 0x07, self.vob)
        self.register_complex("setActorBitVar", 0x0b, self.vob, self.vob, self.vob)
        self.register_simple("loadSound", [0x0c, 0x8c], self.b)
        self.register_simple("printEgo_c64", [0x0d, 0x75], self.dps)
        self.register_complex("putActorAtObject", 0x0e, self.vob, self.b)
        self.register_complex("putActorAtObject", 0x4e, self.vob, self.b)
        self.register_simple("breakHere", [0x10])

        def mak_state_handler(name):
            def handler(opcode):
                if opcode & 0x40:
                    return Instr(name, [])
                return Instr(name, [self.get_byte()])
            return handler
        self.register_opcodes([0x0f, 0x4f], mak_state_handler("clearState02"))
        self.register_opcodes([0x37, 0x77], mak_state_handler("clearState04"))
        self.register_opcodes([0x17, 0x57], mak_state_handler("clearState08"))
        self.register_opcodes([0x8f, 0xcf], mak_state_handler("setState02"))
        self.register_opcodes([0xb7, 0xf7], mak_state_handler("setState04"))
        self.register_opcodes([0x97, 0xd7], mak_state_handler("setState08"))
        self.register_complex("animateActor", 0x11, self.vob, self.vob, self.b)
        self.register_complex("lockCostume", 0x13, self.b)
        self.register_complex("lockRoom", 0x4d, self.b)
        self.register_complex("unlockRoom", 0xcd, self.b)
        self.register_complex("unlockCostume", 0x93, self.b)
        self.register_complex("print_c64", 0x2e, self.vob, self.dps)
        # these are just weird. funny code reuse in scummvm and "code reuse"
        # in descumm.  unnecessary checks for opcode parameter bits.
        self.register_simple("walkActorToActor",
                             [0x15, 0x55],
                             self.vob, self.vob, self.b)
        self.register_simple("getRandomNr", [0x16], self.vob)
        self.register_complex_set("getActorBitVar", 0x1b, self.vob, self.vob)
        # descumm is wrong about this.
        self.register_complex("setBitVar", 0x1d, self.vob, self.vob, self.vob)
        self.register_complex("unknown2", 0x24, self.vob)
        self.register_complex("loadRoom", 0x25, self.vob)
        # another weird one
        self.register_simple("getClosestObjActor", [0x26, 0x66], self.vob)
        self.register_complex("getActorY", 0x27)
        self.register_complex("setOwnerOf", 0x29, self.vob, self.vob)
        self.register_opcode(0x2a, self.do_delay)
        self.register_complex("loadCostume", 0x30, self.vob)
        self.register_complex_set("getBitVar", 0x31, self.vob, self.vob)
        self.register_complex("lockScript", 0x33, self.b)
        self.register_complex("cutscene", 0x40)
        self.register_complex_set("getActorX", 0x47)
        self.register_complex("loadRoom_c64", 0x4a, self.vob)
        self.register_complex("loadScript", 0x4c, self.vob)
        self.register_complex("lockRoom", 0x4d, self.b)
        self.register_simple("nop", [0x50, 0x72, 0xd0, 0xf2])
        self.register_complex("lockSound", 0x54, self.b)
        self.register_simple("setObjectName", [0x54, 0xd4], self.b, self.asc)
        self.register_complex_set("getActorMoving", 0x56)
        self.register_complex("beginOverride", 0x58, self.b, self.w)
        self.register_complex("cursorCommand", 0x60, self.vob)
        self.register_opcodes([0x64, 0xe4], self.do_if_active_object)
        self.register_complex_set("getActorFacing", 0x67, self.b, self.vob)
        self.register_complex("lights", 0x70, self.vob)
        self.register_complex_set("getObjectOwner", 0x73)
        self.register_complex("pickupObject", 0x90, self.b)
        self.register_complex("unlockCostume", 0x93, self.b)
        self.register_complex("unlockScript", 0xb3, self.b)
        self.register_complex("unlockRoom", 0xcd, self.b)
        self.register_complex("unlockSound", 0xd3, self.b)
        self.register_complex("loadRoomWithEgo", 0xe5, self.b, self.b)

    def do_if_active_object(self, _):
        return self.make_jump("activeObject2 == " % self.get_byte())


class SCUMM345(SCUMM12):
    """ByteCode class for SCUMM versions 3, 4 and 5."""

    def populate(self):
        """Populate self.optable with opcode handlers for versions 3, 4 and 5."""
        SCUMM12.populate(self)

        self.register_complex("putActor", 0x01, self.vob, self.vow, self.vow)
        if self.zak_flag:
            self.register_complex_set("startMusic", 0x02, self.vob)
        if self.version == 5:
            self.register_opcodes([0x05, 0x45, 0x85, 0xc5],
                                  self.do_draw_object_v5)
        else:
            self.register_complex("drawObject", 0x05,
                                  self.vow, self.vow, self.vow)
        self.register_complex("setState", 0x07, self.vow, self.vob)
        self.register_simple("startScript",
                             [0x0A, 0x8A, 0x2A, 0xAA, 0x4A, 0xCA, 0x6A, 0xEA],
                             self.vob, self.lst)
        self.register_complex_set("getVerbEntryPoint", 0x0b, self.vow, self.vow)
        self.register_opcodes([0x0c, 0x8c], self.do_resource)
        self.register_opcodes([0x0f, 0x8f,
                               0x2f, 0x4f,
                               0x6f, 0xaf,
                               0xcf, 0xef], self.do_if_state_code)
        if self.version == 5:
            self.register_complex_set("getObjectState", 0x0f, self.vow)
        self.register_complex("panCameraTo", 0x12, self.vow)
        self.register_opcodes([0x14, 0x94, 0xd8], self.do_print_ego)
        self.register_opcodes([0x13, 0x53, 0x93, 0xd3], self.do_actor_ops)
        self.register_complex_set("actorFromPos", 0x15, self.vow, self.vow)
        self.register_opcodes([0x17, 0x97,
                               0x1a, 0x9a,
                               0x1b, 0x9b,
                               0x3a, 0xba,
                               0x46, 0xc6,
                               0x57, 0xd7,
                               0x5a, 0xda,
                               0x5b, 0xdb], self.do_varset)
        self.register_opcodes([0x19, 0x39, 0x59, 0x79, 0x99, 0xb9, 0xd9, 0xf9],
                              self.do_sentence)
        self.register_if("classOfIs", 0x1d, self.vow, self.lst)
        self.register_complex("walkActorTo", 0x1e, self.vob, self.vow, self.vow)
        self.register_if("isActorInBox", 0x1f, self.vob, self.vob)
        if self.version == 5:
            self.register_complex_set("getAnimCounter", 0x22)
        else:
            self.register_complex_set("saveLoadGame", 0x22)
        if self.indy_flag:
            self.register_complex_set("getActorY", 0x23)
            self.register_complex_set("getActorX", 0x43)
        else:
            self.register_complex_set("getActorY", 0x23, self.vow)
            self.register_complex_set("getActorX", 0x43, self.vow)
        self.register_complex("loadRoomWithEgo", 0x24,
                              self.vow, self.vob, self.w, self.w)
        if self.version == 5:
            self.register_complex("pickupObject", 0x25, self.vow, self.vob)
        else:
            self.register_simple("drawObject",
                                 [0x25, 0x65, 0xa5, 0xe5],
                                 self.vow, self.vow, self.vow)
        self.register_opcode(0x27, self.do_string_ops)
        self.register_complex("setOwnerOf", 0x29, self.vow, self.vob)
        self.register_opcode(0x2c, self.do_cursor_command)
        self.register_complex("putActorInRoom", 0x2d, self.vob, self.vob)
        self.register_opcode(0x2e, self.do_delay)
        if self.version == 3:
            self.register_complex("setBoxFlags", 0x30, self.vob, self.b)
        else:
            self.register_opcodes([0x30, 0xb0], self.do_matrix_ops)
        self.register_complex_set("getInventoryCount", 0x31)
        self.register_complex("setCameraAt", 0x32, self.vow)
        if self.version == 5:
            self.register_opcodes([0x33, 0x73, 0xb3, 0xf3], self.do_room_ops)
        else:
            self.register_opcodes([0x33, 0x73, 0xb3, 0xf3], self.do_room_ops_old)
        self.register_complex_set("findObject", 0x35, self.vow, self.vow)
        self.register_complex("walkActorToObject", 0x36, self.vob, self.vow)
        self.register_complex("startObject", 0x37, self.vow, self.vob, self.lst)
        if self.indy_flag:
            self.register_complex("waitForActor", 0x3b, self.vob)
        else:
            self.register_complex_set("getActorScale", 0x3b)
        self.register_complex_set("findInventory", 0x3d, self.vob, self.vob)
        self.register_opcodes([0x3f, 0x7f, 0xbf, 0xff], self.do_draw_box)
        self.register_complex("cutscene", 0x40, self.lst)
        self.register_complex("chainScript", 0x42, self.vob, self.lst)
        if self.version <= 3:
            self.register_complex("waitForSentence", 0x4c)
        else:
            self.register_complex("soundKludge", 0x4c, self.lst)
        self.register_complex("pickupObject", 0x50, self.vow)
        self.register_opcode(0x58, self.do_begin_override)
        self.register_opcode(0x5c, self.do_old_room_effect)
        self.register_complex("setClass", 0x5d, self.vow, self.lst)
        self.register_complex("freezeScripts", 0x60, self.vob)
        self.register_complex("stopScript", 0x62, self.vob)
        self.register_complex_set("getStringWidth", 0x67)
        # descumm seems to miss 0xeb
        self.register_complex("debug", 0x6b, self.vow)
        self.register_complex_set("getActorWidth", 0x6c)
        self.register_complex("stopObjectScript", 0x6e, self.vow)
        self.register_complex("lights", 0x70, self.vob, self.b, self.b)
        self.register_opcodes([0x7a, 0xfa], self.do_verbops)
        self.register_complex_set("getActorWalkBox", 0x7b)
        self.register_complex("systemOps", 0x98, self.b)
        if self.version == 5:
            self.register_opcode(0xa7, self.do_dummy)
        else:
            self.register_opcode(0xa7, self.do_save_load_vars)
        self.register_opcode(0xab, self.do_save_restore_verbs)
        self.register_opcode(0xac, self.do_expression)
        self.register_opcode(0xae, self.do_wait)
        self.register_complex("endCutscene", 0xc0)

    def do_wait(self, _):
        if self.indy_flag:
            opc = 2
        else:
            opc = self.get_byte()
        if opc in (0x01, 0x81):
            return self.produce_instr("WaitForActor", opc, self.vob)
        elif opc == 0x02:
            return Instr("WaitForMessage", [])
        elif opc == 0x03:
            return Instr("WaitForCamera", [])
        elif opc == 0x04:
            return Instr("WaitForSentence", [])
        else:
            raise UnknownSubOpError, \
                "SCUMM345.do_wait: unknown subop %d" % opc

    def do_save_restore_verbs(self, _):
        opc = self.get_byte()
        opcm = opc & 0x1f
        if opcm == 0x01:
            return self.produce_instr("saveVerbs", opc,
                                      self.vob, self.vob, self.vob)
        elif opcm == 0x02:
            return self.produce_instr("restoreVerbs", opc,
                                      self.vob, self.vob, self.vob)
        elif opcm == 0x03:
            return self.produce_instr("deleteVerbs", opc,
                                      self.vob, self.vob, self.vob)
        else:
            raise UnknownSubOpError, \
                "SCUMM345.do_save_restore_verbs: unknown subop %d" % opcm

    def do_save_load_vars(self, _):
        opc = self.get_byte()
        if opc == 1:
            name = "saveVars"
        else:
            name = "loadVars"
        args = []
        opc = self.get_byte()
        while opc:
            opcm = opc & 0x1f
            if opcm == 0x01:
                args.append(self.produce_instr("VarRange", opc, self.v, self.v))
            elif opcm == 0x02:
                args.append(self.produce_instr("StringRange", opc,
                                               self.vob, self.vob))
            elif opcm == 0x03:
                args.append(self.produce_instr("Open", opc, self.asc))
            elif opcm == 0x04:
                args.append(Instr("Append", []))
            elif opcm == 0x1f:
                args.append(Instr("Close", []))
            opc = self.get_byte()
        return Instr(name, args)

    def do_old_room_effect(self, opcode):
        d = self.get_byte()
        if (d & 0x1f) == 3:
            return Instr("oldRoomEffect-set",
                         [self.get_var_or_word(opcode, pmasks[0])])
        return Instr("oldRoomEffect-fadein",
                     [self.get_var_or_word(opcode, pmasks[0])])

    def do_begin_override(self, _):
        d = self.get_byte()
        if d:
            return Instr("beginOverride", [])
        return Instr("endOverride", [])

    def do_draw_box(self, opcode):
        args = self.get_complex(opcode, [self.vow, self.vow])
        opcode = self.get_byte()
        args += self.get_complex(opcode, [self.vow, self.vow, self.vob])
        return Instr("drawBox", args)

    def do_delay(self, _):
        d = self.get_byte()
        d |= self.get_byte() << 8
        d |= self.get_byte() << 16
        return Instr("delay", [d])

    def do_string_ops(self, _):
        opc = self.get_byte()
        opcm = opc & 0x1f
        if opcm == 0x01:
            instr = self.produce_instr("PutCodeInString", opc, self.vob, self.asc)
        elif opcm == 0x02:
            instr = self.produce_instr("CopyString", opc, self.vob, self.vob)
        elif opcm == 0x03:
            instr = self.produce_instr("SetStringChar", opc,
                                       self.vob, self.vob, self.vob)
        elif opcm == 0x04:
            instr = AssgnInstr('=',
                               self.get_var(),
                               self.produce_instr("GetStringChar",
                                                  opc,
                                                  self.vob,
                                                  self.vob))
        elif opcm == 0x05:
            instr = self.produce_instr("CreateString", opc, self.vob, self.vob)
        else:
            raise UnknownSubOpError, \
                "SCUMM345.do_string_ops: unknown string func %d" % opcm
        return instr

    def do_sentence(self, opcode):
        name = "doSentence"
        verb = self.get_var_or_byte(opcode, pmasks[0])
        if verb == 0xfe:
            return Instr(name, ["STOP"])
        return Instr(name, [verb,
                            self.get_var_or_word(opcode, pmasks[1]),
                            self.get_var_or_word(opcode, pmasks[2])])

    def do_draw_object_v5(self, opcode):
        args = self.get_complex(opcode, [self.vow])
        opc = self.get_byte()
        opcm = opc & 0x1f
        if opcm == 1:
            args.append(self.produce_instr("setXY", opc, self.vow, self.vow))
        elif opcm == 2:
            args.append(self.produce_instr("setImage", opc, self.vow))
        return Instr("drawObject", args)

    def do_actor_ops(self, opcode):
        convert_table = \
            (1, 0, 0, 2, 3, 4, 5, 6, 7, 8, 9,
             10, 11, 12, 13, 14, 15, 16, 17, 20)

        args = [self.get_var_or_byte(opcode, pmasks[0])]
        instrs = []

        while 1:
            opc = self.get_byte()
            if opc == 0xff:
                break
            if self.version < 5:
                opc = (opc & 0xe0) | convert_table[(opc & 0x1f) - 1]
            opcm = opc & 0x1f
            if opcm == 0x00:
                instrs.append(self.produce_instr("Unknown", opc, self.vob))
            elif opcm == 0x01:
                instrs.append(self.produce_instr("Costume", opc, self.vob))
            elif opcm == 0x02:
                instrs.append(self.produce_instr("WalkSpeed",
                                                 opc,
                                                 self.vob,
                                                 self.vob))
            elif opcm == 0x03:
                instrs.append(self.produce_instr("Sound", opc, self.vob))
            elif opcm == 0x04:
                instrs.append(self.produce_instr("WalkAnimNr", opc, self.vob))
            elif opcm == 0x05:
                instrs.append(self.produce_instr("TalkAnimNr",
                                                 opc,
                                                 self.vob,
                                                 self.vob))
            elif opcm == 0x06:
                instrs.append(self.produce_instr("StandAnimNr", opc, self.vob))
            elif opcm == 0x07:
                instrs.append(self.produce_instr("Nothing",
                                                 opc,
                                                 self.vob,
                                                 self.vob,
                                                 self.vob))
            elif opcm == 0x08:
                instrs.append(Instr("Init", []))
            elif opcm == 0x09:
                instrs.append(self.produce_instr("Elevation", opc, self.vow))
            elif opcm == 0x0a:
                instrs.append(Instr("DefaultAnims", []))
            elif opcm == 0x0b:
                instrs.append(self.produce_instr("Palette",
                                                 opc,
                                                 self.vob,
                                                 self.vob))
            elif opcm == 0x0c:
                instrs.append(self.produce_instr("TalkColor", opc, self.vob))
            elif opcm == 0x0d:
                instrs.append(self.produce_instr("Name", opc, self.asc))
            elif opcm == 0x0e:
                instrs.append(self.produce_instr("InitAnimNr", opc, self.vob))
#             elif opcm == 0x0f:
#                 instrs.append(self.produce_instr("PaletteList", opc, self.lst))
            elif opcm == 0x10:
                instrs.append(self.produce_instr("Width", opc, self.vob))
            elif opcm == 0x11:
                if self.version == 5:
                    instrs.append(self.produce_instr("Scale",
                                                     opc,
                                                     self.vob,
                                                     self.vob))
                else:
                    instrs.append(self.produce_instr("Scale", opc, self.vob))
            elif opcm == 0x12:
                instrs.append(Instr("NeverZClip", []))
            elif opcm == 0x13:
                instrs.append(self.produce_instr("SetZClip", opc, self.vob))
            elif opcm == 0x14:
                instrs.append(Instr("IgnoreBoxes", []))
            elif opcm == 0x15:
                instrs.append(Instr("FollowBoxes", []))
            elif opcm == 0x16:
                instrs.append(self.produce_instr("AnimeSpeed", opc, self.vob))
            elif opcm == 0x16:
                instrs.append(self.produce_instr("ShadowMode", opc, self.vob))
            else:
                raise UnknownSubOpError, \
                    "SCUMM345.do_actor_ops: %d" % opcm
        return Instr("ActorOps", args + instrs)

    def do_expression(self, _):
        dst = self.get_var()
        expr_stack = []
        while 1:
            i = self.get_byte()
            if i == 0xff:
                break;
            im = i & 0x1f
            if not 0x1 <= im <= 0x6:
                raise InvalidExpressionCodeError, \
                    "SCUMM345.handle_expression: %d is not a valid expression code" % im
            elif im == 0x1:
                expr_stack.append(str(self.get_var_or_word(i, pmasks[0])))
            elif im == 0x6:
                expr_stack.append("<%s>" % self.decode_next())
            else:
                exop = { 0x2 : '+', 0x3 : '-', 0x4 : '*', 0x5 : '/'}[im]
                b = expr_stack.pop()
                a = expr_stack.pop()
                expr_stack.append("(%s %s %s)" % (a, exop, b))
        return AssgnInstr('=', dst, expr_stack.pop())

    def do_resource(self, _):
        opc = self.get_byte()
        if self.version != 5:  # FIXME - should only be done for Zak256
            subop = opc & 0x3f
        else:
            subop = opc & 0x1f

        if subop == 0x1:
            instr = self.produce_instr("loadScript", opc, self.vob)
        elif subop == 0x2:
            instr = self.produce_instr("loadSound", opc, self.vob)
        elif subop == 0x3:
            instr = self.produce_instr("loadCostume", opc, self.vob)
        elif subop == 0x4:
            instr = self.produce_instr("loadRoom", opc, self.vob)
        elif subop == 0x5:
            instr = self.produce_instr("nukeScript", opc, self.vob)
        elif subop == 0x6:
            instr = self.produce_instr("nukeSound", opc, self.vob)
        elif subop == 0x7:
            instr = self.produce_instr("nukeCostume", opc, self.vob)
        elif subop == 0x8:
            instr = self.produce_instr("nukeRoom", opc, self.vob)
        elif subop == 0x9:
            instr = self.produce_instr("lockScript", opc, self.vob)
        elif subop == 0xa:
            instr = self.produce_instr("lockSound", opc, self.vob)
        elif subop == 0xb:
            instr = self.produce_instr("lockCostume", opc, self.vob)
        elif subop == 0xc:
            instr = self.produce_instr("lockRoom", opc, self.vob)
        elif subop == 0xd:
            instr = self.produce_instr("unlockScript", opc, self.vob)
        elif subop == 0xe:
            instr = self.produce_instr("unlockSound", opc, self.vob)
        elif subop == 0xf:
            instr = self.produce_instr("unlockCostume", opc, self.vob)
        elif subop == 0x10:
            instr = self.produce_instr("unlockRoom", opc, self.vob)
        elif subop == 0x11:
            instr = Instr("clearHeap", [])
        elif subop == 0x12:
            instr = self.produce_instr("loadCharset", opc, self.vob)
        elif subop == 0x13:
            instr = self.produce_instr("nukeCharset", opc, self.vob)
        elif subop == 0x14:
            instr = self.produce_instr("loadFlObject", opc,
                                       self.vob, self.vow)
        elif subop == 0x22+1:
            instr = self.produce_instr("resUnk1", opc,
                                       self.vob, self.vob)
        elif subop == 0x23+1:
            instr = self.produce_instr("resUnk2", opc,
                                       self.vob, self.vob, self.b)
        elif subop == 0x24+1:
            instr = self.produce_instr("resUnk3", opc,
                                       self.vob, self.vob)
        else:
            raise UnknownSubOpError, \
                "SCUMM345.do_resource: %d is not a known subop" % subop
        return instr

    def do_room_ops(self, _):
        opc = self.get_byte()
        opcm = opc & 0x1f
        if opcm == 0x01:
            instr = self.produce_instr("RoomScroll", opc,
                                       self.vow, self.vow)
        elif opcm == 0x02:
            instr = Instr("RoomColor", [])
        elif opcm == 0x03:
            instr = self.produce_instr("SetScreen", opc,
                                       self.vow, self.vow)
        elif opcm == 0x04:
            args = self.get_complex(opc, [self.vow, self.vow, self.vow])
            instr = Instr("SetPalColor",
                          args + self.get_complex(self.get_byte(), [self.vob]))
        elif opcm == 0x05:
            instr = Instr("ShakeOn", [])
        elif opcm == 0x06:
            instr = Instr("ShakeOff", [])
        elif opcm == 0x07:
            instr = Instr("Unused", [])
        elif opcm == 0x08:
            instr = self.produce_instr("RoomIntensity", opc,
                                       self.vob, self.vob, self.vob)
        elif opcm == 0x09:
            instr = self.produce_instr("saveLoad?", opc,
                                       self.vob, self.vob)
        elif opcm == 0x0a:
            instr = self.produce_instr("screenEffect?", opc,
                                       self.vow)
        elif opcm == 0x0b:
            args = self.get_complex(opc,
                                    [self.vow, self.vow, self.vow])
            instr = Instr("setRGBRoomIntensity",
                          args + self.get_complex(self.get_byte(),
                                                  [self.vob, self.vob]))
        elif opcm == 0x0c:
            args = self.get_complex(opc,
                                    [self.vow, self.vow, self.vow])
            instr = Instr("setRoomShadow",
                          args + self.get_complex(self.get_byte(),
                                                  [self.vob, self.vob]))
        elif opcm == 0x0d:
            instr = self.produce_instr("saveString", opc,
                                       self.vob, self.asc)
        elif opcm == 0x0e:
            instr = self.produce_instr("loadString", opc,
                                       self.vob, self.asc)
        elif opcm == 0x0f:
            args = self.get_complex(opc, [self.vob])
            args += self.get_complex(self.get_byte(), [self.vob, self.vob])
            instr = Instr("palManipulate",
                          args + self.get_complex(self.get_byte(), [self.vob]))
        elif opcm == 0x10:
            instr = self.produce_instr("colorCycleDelay", opc,
                                       self.vob, self.vob)
        else:
            raise UnknownSubOpError, \
                "SCUMM345.do_room_ops: unknown subop: %d" % opc
        return instr

    def do_cursor_command(self, _):
        opc = self.get_byte()
        opcm = opc & 0x1f
        if opcm == 0x01:
            instr = Instr("CursorShow", [])
        elif opcm == 0x02:
            instr = Instr("CursorHide", [])
        elif opcm == 0x03:
            instr = Instr("UserputOn", [])
        elif opcm == 0x04:
            instr = Instr("UserputOff", [])
        elif opcm == 0x05:
            instr = Instr("CursorSoftOn", [])
        elif opcm == 0x06:
            instr = Instr("CursorSoftOff", [])
        elif opcm == 0x07:
            instr = Instr("UserputSoftOn", [])
        elif opcm == 0x08:
            instr = Instr("UserputSoftOff", [])
        elif opcm == 0x0a:
            instr = self.produce_instr("SetCursorImg", opc, self.vob, self.vob)
        elif opcm == 0x0b:
            instr = self.produce_instr("SetCursorHotspot", opc,
                                       self.vob, self.vob, self.vob)
        elif opcm == 0x0c:
            instr = self.produce_instr("InitCursor", opc, self.vob)
        elif opcm == 0x0d:
            instr = self.produce_instr("InitCharset", opc, self.vob)
        elif opcm == 0x0e:
            if self.version == 3:
                instr = self.produce_instr("LoadCharset", opc,
                                           self.vob, self.vob)
            else:
                instr = self.produce_instr("CursorCommand", opc, self.lst)
        else:
            raise UnknownSubOpError, \
                "SCUMM345.do_cursor_command: unknown subop %d" % opcm
        return instr

    def do_verbops(self, opcode):
        args = [self.get_var_or_byte(opcode, pmasks[0])]
        while 1:
            opc = self.get_byte()
            if opc == 0xff:
                break
            opcm = opc & 0x1f
            if opcm == 0x1:
                args.append(self.produce_instr("Image", opc, self.vow))
            elif opcm == 0x2:
                args.append(self.produce_instr("Text", opc, self.asc))
            elif opcm == 0x3:
                args.append(self.produce_instr("Color", opc, self.vob))
            elif opcm == 0x4:
                args.append(self.produce_instr("HiColor", opc, self.vob))
            elif opcm == 0x5:
                args.append(self.produce_instr("SetXY", opc,
                                               self.vow, self.vow))
            elif opcm == 0x6:
                args.append(Instr("On", []))
            elif opcm == 0x7:
                args.append(Instr("Off", []))
            elif opcm == 0x8:
                args.append(Instr("Delete", []))
            elif opcm == 0x9:
                args.append(Instr("New", []))
            elif opcm == 0x10:
                args.append(self.produce_instr("DimColor", opc, self.vob))
            elif opcm == 0x11:
                args.append(Instr("Dim", []))
            elif opcm == 0x12:
                args.append(self.produce_instr("Key", opc, self.vob))
            elif opcm == 0x13:
                args.append(Instr("Center", []))
            elif opcm == 0x14:
                args.append(self.produce_instr("SetToString", opc, self.vow))
            elif opcm == 0x16:
                args.append(self.produce_instr("SetToObject", opc,
                                               self.vow, self.vob))
            elif opcm == 0x17:
                args.append(self.produce_instr("BackColor", opc, self.vob))
            else:
                raise UnknownSubOpError, \
                    "SCUMM345.do_verbops: unknown subop %d" % opcm
        return Instr("VerbOps", args)

    def do_print_ego(self, opcode):
        if opcode == 0xd8:
            opname = "printEgo"
            args = []
        else:
            opname = "print"
            args = [self.get_var_or_byte(opcode, pmasks[0])]
        while 1:
            opc = self.get_byte()
            if opc == 0xff:
                break
            opcm = opc & 0x1f
            if opcm == 0x0:
                args.append(self.produce_instr("Pos", opc, self.vow, self.vow))
            elif opcm == 0x1:
                args.append(self.produce_instr("Color", opc, self.vob))
            elif opcm == 0x2:
                args.append(self.produce_instr("Clipped", opc, self.vow))
            elif opcm == 0x3:
                args.append(self.produce_instr("RestoreBG", opc, self.vow, self.vow))
            elif opcm == 0x4:
                args.append(Instr("Center", []))
            elif opcm == 0x6:
                if self.unblocked:
                    args.append(self.produce_instr("Height", opc, self.vow))
                else:
                    args.append(Instr("Left", []))
            elif opcm == 0x7:
                args.append(Instr("Overhead", []))
            elif opcm == 0x8:
                args.append(self.produce_instr("PlayCDTrack",
                                               opc,
                                               self.vow, self.vow))
            elif opcm == 0xf:
                args.append(self.produce_instr("Text", opc, self.asc))
                break
            else:
                raise UnknownSubOpError, \
                    "SCUMM345.do_print_ego: unknown subop %d" % opcm
        return Instr(opname, args)

    def do_matrix_ops(self, _):
        opc = self.get_byte()
        opcm = opc & 0x1f
        if opcm == 0x1:
            instr = self.produce_instr("setBoxFlags", opc, self.vob, self.vob)
        elif opcm == 0x2:
            instr = self.produce_instr("setBoxScale", opc, self.vob, self.vob)
        elif opcm == 0x3:
            instr = self.produce_instr("setBoxSlot", opc, self.vob, self.vob)
        elif opcm == 0x4:
            instr = Instr("createBoxMatrix", [])
        else:
            raise UnknownSubOpError, \
                "SCUMM345.do_matrix_ops: unknown subop %d" % opcm
        return instr
