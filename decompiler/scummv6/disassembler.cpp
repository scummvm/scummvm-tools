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
#include <sstream>

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
		START_SUBOPCODE(0x6B); //cursorCommand
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
		OPCODE(0x6D, "ifClassOfIs", kSpecial, 0, ""); //Variable stack arguments
		OPCODE(0x6E, "setClass", kSpecial, 0, ""); //Variable stack arguments
		OPCODE(0x6F, "getState", kSpecial, 0, "");
		OPCODE(0x70, "setState", kSpecial, -2, "");
		OPCODE(0x71, "setOwner", kSpecial, -2, "");
		OPCODE(0x72, "getOwner", kSpecial, 0, "");
		OPCODE(0x73, "jump", kJump, 0, "w");
		OPCODE(0x74, "startSound", kSpecial, -1, "");
		OPCODE(0x75, "stopSound", kSpecial, -1, "");
		OPCODE(0x76, "startMusic", kSpecial, -1, "");
		OPCODE(0x77, "stopObjectScript", kSpecial, -1, "");
		OPCODE(0x78, "panCameraTo", kSpecial, -1, "");
		OPCODE(0x79, "actorFollowCamera", kSpecial, -1, "");
		OPCODE(0x7A, "setCameraAt", kSpecial, -1, "");
		OPCODE(0x7B, "loadRoom", kSpecial, -1, "");
		OPCODE(0x7C, "stopScript", kSpecial, -1, "");
		OPCODE(0x7D, "walkActorToObj", kSpecial, -3, "");
		OPCODE(0x7E, "walkActorTo", kSpecial, -3, "");
		OPCODE(0x7F, "putActorAtXY", kSpecial, -4, "");
		OPCODE(0x80, "putActorAtObject", kSpecial, -2, "");
		OPCODE(0x81, "faceActor", kSpecial, -2, "");
		OPCODE(0x82, "animateActor", kSpecial, -2, "");
		OPCODE(0x83, "doSentence", kSpecial, -4, "");
		OPCODE(0x84, "pickupObject", kSpecial, -1, "");
		OPCODE(0x85, "loadRoomWithEgo", kSpecial, -3, "");
		OPCODE(0x87, "getRandomNumber", kSpecial, 0, "");
		OPCODE(0x88, "getRandomNumberRange", kSpecial, -1, "");
		OPCODE(0x8A, "getActorMoving", kSpecial, 0, "");
		OPCODE(0x8B, "isScriptRunning", kSpecial, 0, "");
		OPCODE(0x8C, "getActorRoom", kSpecial, 0, "");
		OPCODE(0x8D, "getObjectX", kSpecial, 0, "");
		OPCODE(0x8E, "getObjectY", kSpecial, 0, "");
		OPCODE(0x8F, "getObjectDir", kSpecial, 0, "");
		OPCODE(0x90, "getActorWalkBox", kSpecial, 0, "");
		OPCODE(0x91, "getActorCostume", kSpecial, 0, "");
		OPCODE(0x92, "findInventory", kSpecial, -1, "");
		OPCODE(0x93, "getInventoryCount", kSpecial, 0, "");
		OPCODE(0x94, "getVerbFromXY", kSpecial, -1, "");
		OPCODE(0x95, "beginOverride", kSpecial, 0, "");
		OPCODE(0x96, "endOverride", kSpecial, 0, "");
		OPCODE(0x97, "setObjectName", kSpecial, -1, "c");
		OPCODE(0x98, "isSoundRunning", kSpecial, 0, "");
		OPCODE(0x99, "setBoxFlags", kSpecial, 0, ""); //Variable stack arguments
		OPCODE(0x9A, "createBoxMatrix", kSpecial, 0, "");
		START_SUBOPCODE(0x9B); //resourceRoutines
			OPCODE(0x64, "resRoutine_loadScript", kSpecial, -1, "");
			OPCODE(0x65, "resRoutine_loadSound", kSpecial, -1, "");
			OPCODE(0x66, "resRoutine_loadCostume", kSpecial, -1, "");
			OPCODE(0x67, "resRoutine_loadRoom", kSpecial, -1, "");
			OPCODE(0x68, "resRoutine_nukeScript", kSpecial, -1, "");
			OPCODE(0x69, "resRoutine_nukeSound", kSpecial, -1, "");
			OPCODE(0x6A, "resRoutine_nukeCostume", kSpecial, -1, "");
			OPCODE(0x6B, "resRoutine_nukeRoom", kSpecial, -1, "");
			OPCODE(0x6C, "resRoutine_lockScript", kSpecial, -1, "");
			OPCODE(0x6D, "resRoutine_lockSound", kSpecial, -1, "");
			OPCODE(0x6E, "resRoutine_lockCostume", kSpecial, -1, "");
			OPCODE(0x6F, "resRoutine_lockRoom", kSpecial, -1, "");
			OPCODE(0x70, "resRoutine_unlockScript", kSpecial, -1, "");
			OPCODE(0x71, "resRoutine_unlockSound", kSpecial, -1, "");
			OPCODE(0x72, "resRoutine_unlockCostume", kSpecial, -1, "");
			OPCODE(0x73, "resRoutine_unlockRoom", kSpecial, -1, "");
			OPCODE(0x75, "resRoutine_loadCharset", kSpecial, -1, "");
			OPCODE(0x76, "resRoutine_nukeCharset", kSpecial, -1, "");
			OPCODE(0x77, "resRoutine_loadFlObject", kSpecial, -1, "");
		END_SUBOPCODE;
		START_SUBOPCODE(0x9C); //roomOps
			OPCODE(0xAC, "roomOp_roomScroll", kSpecial, -2, "");
			OPCODE(0xAE, "roomOp_setScreen", kSpecial, -2, "");
			OPCODE(0xAF, "roomOp_setPalColor", kSpecial, -4, "");
			OPCODE(0xB0, "roomOp_shakeOn", kSpecial, 0, "");
			OPCODE(0xB1, "roomOp_shakeOff", kSpecial, 0, "");
			OPCODE(0xB3, "roomOp_darkenPalette", kSpecial, -3, "");
			OPCODE(0xB4, "roomOp_saveLoadRoom", kSpecial, -2, "");
			OPCODE(0xB5, "roomOp_screenEffect", kSpecial, -1, "");
			OPCODE(0xB6, "roomOp_darkenPaletteRGB", kSpecial, -5, "");
			OPCODE(0xB7, "roomOp_setupShadowPalette", kSpecial, -5, "");
			OPCODE(0xBA, "roomOp_palManipulate", kSpecial, -4, "");
			OPCODE(0xBB, "roomOp_colorCycleDelay", kSpecial, -2, "");
			OPCODE(0xD5, "roomOp_setPalette", kSpecial, -1, "");
			OPCODE(0xDC, "roomOp_copyPalColor", kSpecial, -2, "");
		END_SUBOPCODE;
		START_SUBOPCODE(0x9D); //actorOps
			OPCODE(0x4C, "actorOp_setCostume", kSpecial, -1, "");
			OPCODE(0x4D, "actorOp_setWalkSpeed", kSpecial, -2, "");
			OPCODE(0x4E, "actorOp_setSound", kSpecial, 0, ""); //Variable stack arguments
			OPCODE(0x4F, "actorOp_setWalkFrame", kSpecial, -1, "");
			OPCODE(0x50, "actorOp_setTalkFrame", kSpecial, -2, "");
			OPCODE(0x51, "actorOp_setStandFrame", kSpecial, -1, "");
			OPCODE(0x52, "actorOp_82?", kSpecial, -3, "");
			OPCODE(0x53, "actorOp_init", kSpecial, 0, "");
			OPCODE(0x54, "actorOp_setElevation", kSpecial, -1, "");
			OPCODE(0x55, "actorOp_setDefAnim", kSpecial, 0, "");
			OPCODE(0x56, "actorOp_setPalette", kSpecial, -2, "");
			OPCODE(0x57, "actorOp_setTalkColor", kSpecial, -1, "");
			OPCODE(0x58, "actorOp_setName", kSpecial, 0, "c");
			OPCODE(0x59, "actorOp_setInitFrame", kSpecial, -1, "");
			OPCODE(0x5B, "actorOp_setWidth", kSpecial, -1, "");
			OPCODE(0x5C, "actorOp_setScale", kSpecial, -1, "");
			OPCODE(0x5D, "actorOp_setNeverZClip", kSpecial, 0, "");
			OPCODE(0x5E, "actorOp_setAlwaysZClip", kSpecial, -1, "");
			OPCODE(0x5F, "actorOp_setIgnoreBoxes", kSpecial, 0, "");
			OPCODE(0x60, "actorOp_setFollowBoxes", kSpecial, 0, "");
			OPCODE(0x61, "actorOp_setAnimSpeed", kSpecial, -1, "");
			OPCODE(0x62, "actorOp_setShadowMode", kSpecial, -1, "");
			OPCODE(0x63, "actorOp_setTalkPos", kSpecial, -2, "");
			OPCODE(0xC5, "actorOp_setCurActor", kSpecial, -1, "");
			OPCODE(0xC6, "actorOp_setAnimVar", kSpecial, -2, "");
			OPCODE(0xD7, "actorOp_setIgnoreTurnsOn", kSpecial, 0, "");
			OPCODE(0xD8, "actorOp_setIgnoreTurnsOff", kSpecial, 0, "");
			OPCODE(0xD9, "actorOp_initLittle", kSpecial, 0, "");
			OPCODE(0xE1, "actorOp_setAlwaysZClip?", kSpecial, -1, "");
			OPCODE(0xE3, "actorOp_setLayer", kSpecial, -1, "");
			OPCODE(0xE4, "actorOp_setWalkScript", kSpecial, -1, "");
			OPCODE(0xE5, "actorOp_setStanding", kSpecial, 0, "");
			OPCODE(0xE6, "actorOp_setDirection", kSpecial, -1, "");
			OPCODE(0xE7, "actorOp_turnToDirection", kSpecial, -1, "");
			OPCODE(0xE9, "actorOp_freeze", kSpecial, 0, "");
			OPCODE(0xEA, "actorOp_unfreeze", kSpecial, 0, "");
			OPCODE(0xEB, "actorOp_setTalkScript", kSpecial, -1, "");
		END_SUBOPCODE;
		START_SUBOPCODE(0x9E); //verbOps
			OPCODE(0x7C, "verbOp_loadImg", kSpecial, -1, "");
			OPCODE(0x7D, "verbOp_loadString", kSpecial, 0, "c");
			OPCODE(0x7E, "verbOp_setColor", kSpecial, -1, "");
			OPCODE(0x7F, "verbOp_setHiColor", kSpecial, -1, "");
			OPCODE(0x80, "verbOp_setXY", kSpecial, -2, "");
			OPCODE(0x81, "verbOp_setOn", kSpecial, 0, "");
			OPCODE(0x82, "verbOp_setOff", kSpecial, 0, "");
			OPCODE(0x83, "verbOp_kill", kSpecial, 0, "");
			OPCODE(0x84, "verbOp_init", kSpecial, 0, "");
			OPCODE(0x85, "verbOp_setDimColor", kSpecial, -1, "");
			OPCODE(0x86, "verbOp_setDimmed", kSpecial, 0, "");
			OPCODE(0x87, "verbOp_setKey", kSpecial, -1, "");
			OPCODE(0x88, "verbOp_setCenter", kSpecial, 0, "");
			OPCODE(0x89, "verbOp_setToString", kSpecial, -1, "");
			OPCODE(0x8B, "verbOp_setToObject", kSpecial, -2, "");
			OPCODE(0x8C, "verbOp_setBkColor", kSpecial, -1, "");
			OPCODE(0xC4, "verbOp_setCurVerb", kSpecial, -1, "");
			OPCODE(0xFF, "verbOp_redraw", kSpecial, 0, "");
		END_SUBOPCODE;
		OPCODE(0x9F, "getActorFromXY", kSpecial, -1, "");
		OPCODE(0xA0, "findObject", kSpecial, -1, "");
		OPCODE(0xA1, "pseudoRoom", kSpecial, 0, ""); //Variable stack arguments
		OPCODE(0xA2, "getActorElevation", kSpecial, 0, "");
		OPCODE(0xA3, "getVerbEntrypoint", kSpecial, -1, "");
		START_SUBOPCODE(0xA4); //arrayOps
		END_SUBOPCODE;
	END_OPCODES;

	return _insts;
}

void ScummV6Disassembler::readParameter(Parameter *p, char type) {
	switch (type)	{
	case 'c': //Character string
		{
		byte cmd;
		bool inStr = false;
		std::stringstream s;
		while ((cmd = _f.readByte()) != 0) {
			if (cmd == 0xFF || cmd == 0xFE) {
				if (inStr) {
					s << '"';
					inStr = false;
				}
				cmd = _f.readByte();
				switch (cmd) {
				case 1:
					s << ":newline:";
					_address += 2;
					break;
				case 2:
					s << ":keeptext:";
					_address += 2;
					break;
				case 3:
					s << ":wait:";
					_address += 2;
					break;
				case 4:		// addIntToStack
				case 5:		// addVerbToStack
				case 6:		// addNameToStack
				case 7:		// addStringToStack
					{
					uint16 var = _f.readUint16LE();
					//TODO: Clean output similar to descumm
					s << ":addToStack=" << var << ":";
					_address += 4;
					}
					break;
				case 9:
					s << ":startanim=" << _f.readUint16LE() << ":";
					_address += 4;
					break;
				case 10:
					s << ":sound:";
					_f.seek(14, SEEK_CUR);
					_address += 16;
					break;
				case 12:
					s << ":setcolor=" << _f.readUint16LE() << ":";
					_address += 4;
					break;
				case 13:
					s << ":unk2=" << _f.readUint16LE() << ":";
					_address += 4;
					break;
				case 14:
					s << ":setfont=" << _f.readUint16LE() << ":";
					_address += 4;
					break;
				default:
					s << ":unk" << cmd << "=" << _f.readUint16LE() << ":";
					_address += 4;
					break;
				}
			} else {
				if (!inStr) {
					s << '"';
					inStr = true;
				}
				s << cmd;
				_address++;
			}
		}
		p->_type = kString;
		p->_value = s.str();
		}
		break;
	default: //Defer handling to parent implementation
		SimpleDisassembler::readParameter(p, type);
		break;
	}
}
