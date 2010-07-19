/* ScummVM Tools
 * Copyright (C) 2010 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *
 */

#include "codegen.h"

void Scumm::v6::CodeGenerator::processInst(const Instruction inst) {
	// TODO

	// This is just to keep some order in this code and have related opcodes near each other.
	// It's not strictly necessary, because we can just look directly at the opcode, but this should be easier to read.
	switch (inst._type) {
	case kLoad:
		switch (inst._opcode) {
		case 0x00: // pushByte
			_stack.push(new IntEntry(inst._params[0].getUnsigned(), true));
			break;
		case 0x01: // pushWord
			_stack.push(new IntEntry(inst._params[0].getSigned(), false));
			break;
		case 0x02: // pushByteVar
		case 0x03: // pushWordVar
			_stack.push(new VarEntry(decodeVarName(inst._params[0].getUnsigned())));
			break;
		case 0x06: // byteArrayRead
			// TODO
			break;
		case 0x07: // wordArrayRead
			// TODO
			break;
		case 0x0A: // byteArrayIndexedRead
			// TODO
			break;
		case 0x0B: // wordArrayIndexedRead
			// TODO
			break;
		}
		break;
	case kStore:
		switch (inst._opcode) {
			case 0x42:
			case 0x43:
			{
				EntryPtr p = new VarEntry(decodeVarName(inst._params[0].getUnsigned()));
				writeAssignment(p, _stack.top());
				_stack.pop();
				break;
			}
			case 0x46:
				break;
			case 0x47:
				break;
			case 0x4A:
				break;
			case 0x4B:
				break;
		}
		break;
	default:
		{
			std::stringstream s;
			s << boost::format("Unknown opcode %X at address %08X") % inst._opcode % inst._address;
			_curGroup->_code.push_back(indentString(s.str()));
			break;
		}
	}
}

const char *var_names[] = {
	/* 0 */
	NULL,
	"VAR_EGO",
	"VAR_CAMERA_POS_X",
	"VAR_HAVE_MSG",
	/* 4 */
	"VAR_ROOM",
	"VAR_OVERRIDE",
	"VAR_MACHINE_SPEED",
	NULL,
	/* 8 */
	"VAR_NUM_ACTOR",
	"VAR_V6_SOUNDMODE",
	"VAR_CURRENTDRIVE",
	"VAR_TMR_1",
	/* 12 */
	"VAR_TMR_2",
	"VAR_TMR_3",
	NULL,
	NULL,
	/* 16 */
	NULL,
	"VAR_CAMERA_MIN_X",
	"VAR_CAMERA_MAX_X",
	"VAR_TIMER_NEXT",
	/* 20 */
	"VAR_VIRT_MOUSE_X",
	"VAR_VIRT_MOUSE_Y",
	"VAR_ROOM_RESOURCE",
	"VAR_LAST_SOUND",
	/* 24 */
	"VAR_CUTSCENEEXIT_KEY",
	"VAR_TALK_ACTOR",
	"VAR_CAMERA_FAST_X",
	"VAR_SCROLL_SCRIPT",
	/* 28 */
	"VAR_ENTRY_SCRIPT",
	"VAR_ENTRY_SCRIPT2",
	"VAR_EXIT_SCRIPT",
	"VAR_EXIT_SCRIPT2",
	/* 32 */
	"VAR_VERB_SCRIPT",
	"VAR_SENTENCE_SCRIPT",
	"VAR_INVENTORY_SCRIPT",
	"VAR_CUTSCENE_START_SCRIPT",
	/* 36 */
	"VAR_CUTSCENE_END_SCRIPT",
	"VAR_CHARINC",
	"VAR_WALKTO_OBJ",
	"VAR_DEBUGMODE",
	/* 40 */
	"VAR_HEAPSPACE",
	"VAR_ROOM_WIDTH",
	"VAR_RESTART_KEY",
	"VAR_PAUSE_KEY",
	/* 44 */
	"VAR_MOUSE_X",
	"VAR_MOUSE_Y",
	"VAR_TIMER",
	"VAR_TMR_4",
	/* 48 */
	NULL,
	"VAR_VIDEOMODE",
	"VAR_MAINMENU_KEY",
	"VAR_FIXEDDISK",
	/* 52 */
	"VAR_CURSORSTATE",
	"VAR_USERPUT",
	"VAR_ROOM_HEIGHT",
	NULL,
	/* 56 */
	"VAR_SOUNDRESULT",
	"VAR_TALKSTOP_KEY",
	NULL,
	"VAR_FADE_DELAY",
	/* 60 */
	"VAR_NOSUBTITLES",
	"VAR_SAVELOAD_SCRIPT",
	"VAR_SAVELOAD_SCRIPT2",
	NULL,
	/* 64 */
	"VAR_SOUNDPARAM",
	"VAR_SOUNDPARAM2",
	"VAR_SOUNDPARAM3",
	"VAR_INPUTMODE",
	/* 68 */
	"VAR_MEMORY_PERFORMANCE",
	"VAR_VIDEO_PERFORMANCE",
	"VAR_ROOM_FLAG",
	"VAR_GAME_LOADED",
	/* 72 */
	"VAR_NEW_ROOM",
	NULL,
	"VAR_LEFTBTN_DOWN",
	"VAR_RIGHTBTN_DOWN",
	/* 76 */
	"VAR_V6_EMSSPACE",
	NULL,
	NULL,
	NULL,
	/* 80 */
	NULL,
	NULL,
	NULL,
	NULL,
	/* 84 */
	NULL,
	NULL,
	NULL,
	NULL,
	/* 88 */
	NULL,
	NULL,
	"VAR_GAME_DISK_MSG",
	"VAR_OPEN_FAILED_MSG",
	/* 92 */
	"VAR_READ_ERROR_MSG",
	"VAR_PAUSE_MSG",
	"VAR_RESTART_MSG",
	"VAR_QUIT_MSG",
	/* 96 */
	"VAR_SAVE_BTN",
	"VAR_LOAD_BTN",
	"VAR_PLAY_BTN",
	"VAR_CANCEL_BTN",
	/* 100 */
	"VAR_QUIT_BTN",
	"VAR_OK_BTN",
	"VAR_SAVE_DISK_MSG",
	"VAR_ENTER_NAME_MSG",
	/* 104 */
	"VAR_NOT_SAVED_MSG",
	"VAR_NOT_LOADED_MSG",
	"VAR_SAVE_MSG",
	"VAR_LOAD_MSG",
	/* 108 */
	"VAR_SAVE_MENU_TITLE",
	"VAR_LOAD_MENU_TITLE",
	"VAR_GUI_COLORS",
	"VAR_DEBUG_PASSWORD",
	/* 112 */
	NULL,
	NULL,
	NULL,
	NULL,
	/* 116 */
	NULL,
	"VAR_MAIN_MENU_TITLE",
	"VAR_RANDOM_NR",
	"VAR_TIMEDATE_YEAR",
	/* 120 */
	NULL,
	"VAR_GAME_VERSION",
	NULL,
	"VAR_CHARSET_MASK",
	/* 124 */
	NULL,
	"VAR_TIMEDATE_HOUR",
	"VAR_TIMEDATE_MINUTE",
	NULL,
	/* 128 */
	"VAR_TIMEDATE_DAY",
	"VAR_TIMEDATE_MONTH",
	NULL,
	NULL,
};

std::string Scumm::v6::CodeGenerator::decodeVarName(uint16 varID) {
	std::stringstream s;
	if (!(varID & 0xF000)) {
		uint16 var = varID & 0xFFF;
		if (var < (sizeof(var_names) / sizeof(var_names[0])) && var_names[var] != NULL)
			return var_names[var];
		else
			s << boost::format("var%d") % (varID & 0xFFF);
	} else if (varID & 0x8000) {
		s << boost::format("bitvar%d") % (varID & 0x7FFF);
	} else if (varID & 0x4000) {
		s << boost::format("localvar%d") % (varID & 0xFFF);
	} else {
		s << boost::format("?var?%d") % varID;
	}
	return s.str();
}
