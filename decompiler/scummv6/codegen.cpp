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

EntryPtr Scumm::v6::CodeGenerator::createListEntry() {
	EntryList list;
	EntryPtr countEntry = _stack.pop();
	std::stringstream s;
	s << countEntry;
	int count = atoi(s.str().c_str());
	for (int i = 0; i < count; i++) {
		list.push_front(_stack.pop());
	}
	return new ListEntry(list);
}

void Scumm::v6::CodeGenerator::processInst(const Instruction inst) {
	// TODO

	// This is just to keep some order in this code and have related
	// opcodes near each other. It's not strictly necessary, because we
	// can just look directly at the opcode, but this should be easier
	// to read.
	switch (inst._type) {
	case kLoad:
		switch (inst._opcode) {
		case 0x00: // pushByte
			_stack.push(new IntEntry(inst._params[0].getUnsigned(), false));
			break;
		case 0x01: // pushWord
			_stack.push(new IntEntry(inst._params[0].getSigned(), true));
			break;
		case 0x02: // pushByteVar
		case 0x03: // pushWordVar
			_stack.push(new VarEntry(decodeVarName(inst._params[0].getUnsigned())));
			break;
		case 0x06: // byteArrayRead
		case 0x07: // wordArrayRead
			{
				EntryList idxs;
				idxs.push_front(_stack.pop());
				_stack.push(new ArrayEntry(decodeArrayName(inst._params[0].getUnsigned()), idxs));
				break;
			}
		case 0x0A: // byteArrayIndexedRead
		case 0x0B: // wordArrayIndexedRead
			{
				EntryList idxs;
				idxs.push_front(_stack.pop());
				idxs.push_front(_stack.pop());
				_stack.push(new ArrayEntry(decodeArrayName(inst._params[0].getUnsigned()), idxs));
				break;
			}
		}
		break;
	case kStore:
		switch (inst._opcode) {
			case 0x42: // writeByteVar
			case 0x43: // writeWordVar
				{
					EntryPtr p = new VarEntry(decodeVarName(inst._params[0].getUnsigned()));
					writeAssignment(p, _stack.pop());
				}
				break;
			case 0x46: // byteArrayWrite
			case 0x47: // wordArrayWrite
				{
					EntryPtr value = _stack.pop();
					EntryList idxs;
					idxs.push_back(_stack.pop());
					EntryPtr p = new ArrayEntry(decodeArrayName(inst._params[0].getUnsigned()), idxs);
					writeAssignment(p, value);
				}
				break;
			case 0x4A: // byteArrayIndexedWrite
			case 0x4B: // wordArrayIndexedWrite
				{
					EntryPtr value = _stack.pop();
					EntryList idxs;
					idxs.push_front(_stack.pop());
					idxs.push_front(_stack.pop());
					EntryPtr p = new ArrayEntry(decodeArrayName(inst._params[0].getUnsigned()), idxs);
					writeAssignment(p, value);
				}
				break;
			default:
				{
					std::stringstream s;
					s << boost::format("Unknown opcode %X at address %08X") % inst._opcode % inst._address;
					addOutputLine(s.str());
				}
				break;
		}
		break;
	case kStack:
		// Only two opcodes in SCUMMv6, 0x1A and 0xA7: both are single item pop
		_stack.pop();
		//addOutputLine("// pop();");
		break;
	case kCondJumpRel:
		switch (_curGroup->_type) {
		case kIfCond:
		case kWhileCond:
			if (inst._opcode == 0x5C) // jumpTrue
				_stack.push(new UnaryOpEntry(_stack.pop(), "!"));
			break;
		case kDoWhileCond:
			if (inst._opcode == 0x5D) // jumpFalse
				_stack.push(new UnaryOpEntry(_stack.pop(), "!"));
			break;
		default:
			{
				std::stringstream s;
				s << boost::format("Couldn't handle conditional jump at address %08X") % inst._address;
				addOutputLine(s.str());
			}
			break;
		}
		break;
	case kUnaryOp:
		switch (inst._opcode) {
		case 0x4E: // byteVarInc
		case 0x4F: // wordVarInc
		case 0x56: // byteVarDec
		case 0x57: // wordVarDec
			{
				std::stringstream s;
				EntryPtr p = new UnaryOpEntry(new VarEntry(decodeVarName(inst._params[0].getUnsigned())), inst._codeGenData.substr(1));
				s << p << ";";
				addOutputLine(s.str());
			}
			break;
		case 0x52: // byteArrayInc
		case 0x53: // wordArrayInc
		case 0x5A: // byteArrayDec
		case 0x5B: // wordArrayDec
			{
				std::stringstream s;
				EntryList idxs;
				idxs.push_front(_stack.pop());
				EntryPtr p = new UnaryOpEntry(new ArrayEntry(decodeVarName(inst._params[0].getUnsigned()), idxs), inst._codeGenData.substr(1));
				s << p << ";";
				addOutputLine(s.str());
			}
			break;
		default:
			{
				std::stringstream s;
				s << boost::format("Unknown opcode %X at address %08X") % inst._opcode % inst._address;
				addOutputLine(s.str());
			}
			break;
		}
		break;
	case kSpecial:
		switch (inst._opcode) {
		case 0xA4CD: // arrayOp_assignString
			{
				EntryPtr value = new StringEntry(inst._params[1].getString());
				EntryList idxs;
				idxs.push_front(_stack.pop());
				EntryPtr p = new ArrayEntry(decodeArrayName(inst._params[0].getUnsigned()), idxs);
				writeAssignment(p, value);
			}
			break;
		case 0xA4D0: // arrayOp_assignIntList
			{
				EntryList idxs;
				idxs.push_front(_stack.pop());
				EntryPtr value = createListEntry();
				EntryPtr p = new ArrayEntry(decodeArrayName(inst._params[0].getUnsigned()), idxs);
				writeAssignment(p, value);
			}

			break;
		case 0xA4D4: // arrayOp_assign2DimList
			{
				EntryList idxs;
				idxs.push_front(_stack.pop());
				EntryPtr value = createListEntry();
				idxs.push_front(_stack.pop());
				EntryPtr p = new ArrayEntry(decodeArrayName(inst._params[0].getUnsigned()), idxs);
				writeAssignment(p, value);
			}

			break;
		default:
			{
				std::stringstream s;
				s << boost::format("Unknown opcode %X at address %08X") % inst._opcode % inst._address;
				addOutputLine(s.str());
			}
			break;
		}
		break;
	default:
		{
			std::stringstream s;
			s << boost::format("Unknown opcode %X at address %08X") % inst._opcode % inst._address;
			addOutputLine(s.str());
		}
		break;
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

const char *Scumm::v6::CodeGenerator::getVarName(uint16 varID) {
	if (varID >= sizeof(var_names) / sizeof(var_names[0]))
		return NULL;
	return var_names[varID];
}

std::string Scumm::v6::CodeGenerator::decodeVarName(uint16 varID) {
	std::stringstream s;
	if (!(varID & 0xF000)) {
		uint16 var = varID & 0xFFF;
		const char* varName = getVarName(var);
		if (varName	 != NULL)
			return varName;
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

std::string Scumm::v6::CodeGenerator::decodeArrayName(uint16 arrID) {
	std::stringstream s;
	const char *varName;
	if (!(arrID & 0xF000) && (varName = getVarName(arrID & 0xFFF)) != NULL)
		return varName;
	s << boost::format("array%d") % arrID;
	return s.str();
}

void Scumm::v6::CodeGenerator::processSpecialMetadata(const Instruction inst, char c) {
	switch (c) {
		// All of these meanings are taken from descumm.
		case 'l':
			addArg(createListEntry());
			break;
		// No SCUMMv6 opcodes using these types have more than one parameter, so it's safe to assume it's the first parameter we want.
		case 'w':
		case 'j':
		case 'i':
			switch (inst._params[0]._type) {
			case kSByte:
			case kShort:
				addArg(new IntEntry(inst._params[0].getSigned(), false));
				break;
			case kByte:
			case kUShort:
				addArg(new IntEntry(inst._params[0].getUnsigned(), false));
				break;
			default:
				std::cerr << boost::format("Unexpected type for parameter 0 @ %08X while processing metadata character %c") % inst._address % c;
				break;
			}
			break;
		case 'v':
			addArg(new VarEntry(decodeVarName(inst._params[0].getUnsigned())));
			break;
		case 's':
			addArg(new StringEntry(inst._params[0].getString()));
			break;
		case 'z':
			addArg(_stack.pop());
			addArg(_stack.pop());
			break;
		default:
			::CodeGenerator::processSpecialMetadata(inst, c);
			break;
	}
}
