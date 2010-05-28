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

#include <iostream>

#include "disassembler.h"

std::vector<Instruction> ScummV6Disassembler::disassemble() {
	std::string blockName;
	for (int i = 0; i < 4; i++) {
		blockName += _f.readChar();
	}
	if (blockName == "SCRP") {
		std::cout << "Input is global script\n";
		_f.seek(8, SEEK_SET);
	} else if (blockName == "LSCR") {
		std::cout << "Input is local script\n";
		_f.seek(9, SEEK_SET);
	} else if (blockName == "ENCD") {
		std::cout << "Input is room entry script\n";
		_f.seek(8, SEEK_SET);
	} else if (blockName == "EXCD") {
		std::cout << "Input is room exit script\n";
		_f.seek(8, SEEK_SET);
	} else if (blockName == "VERB") {
		std::cout << "Input is object script\n";
		_f.seek(8, SEEK_SET);
		std::cout << "Offset table:\n";
		uint8 verb = _f.readByte();
		while (verb != 0) {
			printf("%02x - %04x", verb, _f.readUint16LE());
		}
	}
	
	START_OPCODES;
		OPCODE(0x00, "pushByte", kLoad, 1, "B");
		OPCODE(0x01, "pushWord", kLoad, 1, "w");
		OPCODE(0x02, "pushByteVar", kLoad, 1, "B");
		OPCODE(0x03, "pushWordVar", kLoad, 1, "w");
		OPCODE(0x06, "byteArrayRead", kLoad, 0, "B");
		OPCODE(0x07, "wordArrayRead", kLoad, 0, "w");
		OPCODE(0x0A, "byteArrayIndexedRead", kLoad, -1, "B");
		OPCODE(0x0B, "wordArrayIndexedRead", kLoad, -1, "w");
		OPCODE(0x0C, "dup", kLoad, 1, "");
		OPCODE(0x0D, "not", kBoolean, 0, "");
		OPCODE(0x0E, "eq", kComparison, -1, "");
		OPCODE(0x0F, "neq", kComparison, -1, "");
		OPCODE(0x10, "gt", kComparison, -1, "");
		OPCODE(0x11, "lt", kComparison, -1, "");
		OPCODE(0x12, "le", kComparison, -1, "");
		OPCODE(0x13, "ge", kComparison, -1, "");
		OPCODE(0x14, "add", kArithmetic, -1, "");
		OPCODE(0x15, "sub", kArithmetic, -1, "");
		OPCODE(0x16, "mul", kArithmetic, -1, "");
		OPCODE(0x17, "div", kArithmetic, -1, "");
		OPCODE(0x18, "land", kBoolean, -1, "");
		OPCODE(0x19, "lor", kBoolean, -1, "");
		OPCODE(0x1A, "kill", kStack, -1, "");
		OPCODE(0x42, "writeByteVar", kStore, -1, "B");
		OPCODE(0x43, "writeWordVar", kStore, -1, "w");
		OPCODE(0x46, "byteArrayWrite", kStore, -2, "B");
		OPCODE(0x47, "wordArrayWrite", kStore, -2, "w");
		OPCODE(0x4A, "byteArrayIndexedWrite", kStore, -3, "B");
		OPCODE(0x4B, "wordArrayIndexedWrite", kStore, -3, "w");
		OPCODE(0x4E, "byteVarInc", kArithmetic, 0, "B");
		OPCODE(0x4F, "wordVarInc", kArithmetic, 0, "w");
		OPCODE(0x52, "byteArrayInc", kArithmetic, -1, "B");
		OPCODE(0x53, "wordArrayInc", kArithmetic, -1, "w");
		OPCODE(0x56, "byteVarDec", kArithmetic, 0, "B");
		OPCODE(0x57, "wordVarDec", kArithmetic, 0, "w");
		OPCODE(0x5A, "byteArrayDec", kArithmetic, -1, "B");
		OPCODE(0x5B, "wordArrayDec", kArithmetic, -1, "w");
		OPCODE(0x5C, "jumpTrue", kCondJump, -1, "w");
		OPCODE(0x5D, "jumpFalse", kCondJump, -1, "w");
		OPCODE(0x5E, "startScript", kSpecial, 0, ""); //Variable stack arguments
		OPCODE(0x5F, "startScriptQuick", kSpecial, 0, ""); //Variable stack arguments
		OPCODE(0x60, "startObject", kSpecial, 0, ""); //Variable stack arguments
		OPCODE(0x61, "drawObject", kSpecial, -2, "");
		OPCODE(0x62, "drawObjectAt", kSpecial, -3, "");
		OPCODE(0x63, "drawBlastObject", kSpecial, 0, "");
		OPCODE(0x64, "setBlastObjectWindow", kSpecial, -4, "");
		OPCODE(0x65, "stopObjectCodeA", kSpecial, 0, "");
		OPCODE(0x66, "stopObjectCodeB", kSpecial, 0, "");
		OPCODE(0x67, "endCutscene", kSpecial, 0, "");
		OPCODE(0x68, "beginCutscene", kSpecial, 0, ""); //Variable stack arguments
		OPCODE(0x69, "stopMusic", kSpecial, 0, "");
		OPCODE(0x6A, "freezeUnfreeze", kSpecial, -1, "");
		START_SUBOPCODE(0x6B);
			OPCODE(0x90, "cursorCmd_CursorOn", kSpecial, 0, "");
			OPCODE(0x91, "cursorCmd_CursorOff", kSpecial, 0, "");
			OPCODE(0x92, "cursorCmd_UserputOn", kSpecial, 0, "");
			OPCODE(0x93, "cursorCmd_UserputOff", kSpecial, 0, "");
			OPCODE(0x94, "cursorCmd_SoftOn", kSpecial, 0, "");
			OPCODE(0x95, "cursorCmd_SoftOff", kSpecial, 0, "");
			OPCODE(0x96, "cursorCmd_UserputSoftOn", kSpecial, 0, "");
			OPCODE(0x97, "cursorCmd_UserputSoftOff", kSpecial, 0, "");			
			OPCODE(0x99, "cursorCmd_Image", kSpecial, -2, "");
			OPCODE(0x9A, "cursorCmd_Hotspot", kSpecial, -2, "");
			OPCODE(0x9C, "cursorCmd_CharsetSet", kSpecial, -1, "");
			OPCODE(0x9D, "cursorCmd_CharsetColor", kSpecial, 0, ""); //Variable stack arguments
			OPCODE(0xD6, "cursorCmd_Transparent", kSpecial, -1, "");
		END_SUBOPCODE;
		OPCODE(0x6C, "breakHere", kSpecial, 0, "");
	END_OPCODES;

	return _insts;
}
