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

#include <boost/format.hpp>

#include "disassembler.h"

Scumm::v6::Scummv6Disassembler::Scummv6Disassembler(InstVec &insts) : SimpleDisassembler(insts) {
}

void Scumm::v6::Scummv6Disassembler::doDisassemble() throw(std::exception) {
	std::string blockName;
	for (int i = 0; i < 4; i++) {
		blockName += _f.readChar();
	}
	if (blockName == "SCRP") {
		//std::clog << "Input is global script\n";
		_f.seek(8, SEEK_SET);
	} else if (blockName == "LSCR") {
		//std::clog << "Input is local script\n";
		_f.seek(9, SEEK_SET);
	} else if (blockName == "ENCD") {
		//std::clog << "Input is room entry script\n";
		_f.seek(8, SEEK_SET);
	} else if (blockName == "EXCD") {
		//std::clog << "Input is room exit script\n";
		_f.seek(8, SEEK_SET);
	} else if (blockName == "VERB") {
		//std::clog << "Input is object script\n";
		_f.seek(8, SEEK_SET);
		//std::clog << "Offset table:\n";
		uint32 verb = _f.readByte();
		while (verb != 0) {
			/*std::clog << boost::format("%02x - %04x\n") % verb %*/ _f.readUint16LE();
			verb = _f.readByte();
		}
	} else {
		std::stringstream s;
		s << "Unknown block name " << blockName;
		throw std::runtime_error(s.str());
	}

	START_OPCODES;
		OPCODE(0x00, "pushByte", Scummv6LoadInstruction, 1, "B");
		OPCODE(0x01, "pushWord", Scummv6LoadInstruction, 1, "s");
		OPCODE(0x02, "pushByteVar", Scummv6LoadInstruction, 1, "B");
		OPCODE(0x03, "pushWordVar", Scummv6LoadInstruction, 1, "w");
		OPCODE(0x06, "byteArrayRead", Scummv6LoadInstruction, 0, "B");
		OPCODE(0x07, "wordArrayRead", Scummv6LoadInstruction, 0, "w");
		OPCODE(0x0A, "byteArrayIndexedRead", Scummv6LoadInstruction, -1, "B");
		OPCODE(0x0B, "wordArrayIndexedRead", Scummv6LoadInstruction, -1, "w");
		OPCODE(0x0C, "dup", DupStackInstruction, 1, "");
		OPCODE(0x0D, "not", BoolNegateStackInstruction, 0, "");
		OPCODE_MD(0x0E, "eq", BinaryOpStackInstruction, -1, "", "==");
		OPCODE_MD(0x0F, "neq", BinaryOpStackInstruction, -1, "", "!=");
		OPCODE_MD(0x10, "gt", BinaryOpStackInstruction, -1, "", ">");
		OPCODE_MD(0x11, "lt", BinaryOpStackInstruction, -1, "", "<");
		OPCODE_MD(0x12, "le", BinaryOpStackInstruction, -1, "", "<=");
		OPCODE_MD(0x13, "ge", BinaryOpStackInstruction, -1, "", ">=");
		OPCODE_MD(0x14, "add", BinaryOpStackInstruction, -1, "", "+");
		OPCODE_MD(0x15, "sub", BinaryOpStackInstruction, -1, "", "-");
		OPCODE_MD(0x16, "mul", BinaryOpStackInstruction, -1, "", "*");
		OPCODE_MD(0x17, "div", BinaryOpStackInstruction, -1, "", "/");
		OPCODE_MD(0x18, "land", BinaryOpStackInstruction, -1, "", "&&");
		OPCODE_MD(0x19, "lor", BinaryOpStackInstruction, -1, "", "||");
		OPCODE(0x1A, "pop", Scummv6StackInstruction, -1, "");
		OPCODE(0x42, "writeByteVar", Scummv6StoreInstruction, -1, "B");
		OPCODE(0x43, "writeWordVar", Scummv6StoreInstruction, -1, "w");
		OPCODE(0x46, "byteArrayWrite", Scummv6StoreInstruction, -2, "B");
		OPCODE(0x47, "wordArrayWrite", Scummv6StoreInstruction, -2, "w");
		OPCODE(0x4A, "byteArrayIndexedWrite", Scummv6StoreInstruction, -3, "B");
		OPCODE(0x4B, "wordArrayIndexedWrite", Scummv6StoreInstruction, -3, "w");
		OPCODE_MD(0x4E, "byteVarInc", Scummv6IncDecInstruction, 0, "B", "++");
		OPCODE_MD(0x4F, "wordVarInc", Scummv6IncDecInstruction, 0, "w", "++");
		OPCODE_MD(0x52, "byteArrayInc", Scummv6IncDecInstruction, -1, "B", "++");
		OPCODE_MD(0x53, "wordArrayInc", Scummv6IncDecInstruction, -1, "w", "++");
		OPCODE_MD(0x56, "byteVarDec", Scummv6IncDecInstruction, 0, "B", "--");
		OPCODE_MD(0x57, "wordVarDec", Scummv6IncDecInstruction, 0, "w", "--");
		OPCODE_MD(0x5A, "byteArrayDec", Scummv6IncDecInstruction, -1, "B", "--");
		OPCODE_MD(0x5B, "wordArrayDec", Scummv6IncDecInstruction, -1, "w", "--");
		OPCODE(0x5C, "jumpTrue", Scummv6CondJumpInstruction, -1, "a");
		OPCODE(0x5D, "jumpFalse", Scummv6CondJumpInstruction, -1, "a");
		OPCODE_MD(0x5E, "startScript", KernelCallStackInstruction, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0x5F, "startScriptQuick", KernelCallStackInstruction, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0x60, "startObject", KernelCallStackInstruction, 0x1030, "", "lppp"); // Variable stack arguments
		OPCODE_MD(0x61, "drawObject", KernelCallStackInstruction, -2, "", "pp");
		OPCODE_MD(0x62, "drawObjectAt", KernelCallStackInstruction, -3, "", "ppp");
		OPCODE_MD(0x63, "drawBlastObject", KernelCallStackInstruction, -5, "", "ppppp");
		OPCODE_MD(0x64, "setBlastObjectWindow", KernelCallStackInstruction, -4, "", "pppp");
		OPCODE(0x65, "stopObjectCodeA", KernelCallStackInstruction, 0, "");
		OPCODE(0x66, "stopObjectCodeB", KernelCallStackInstruction, 0, "");
		OPCODE(0x67, "endCutscene", KernelCallStackInstruction, 0, "");
		OPCODE_MD(0x68, "beginCutscene", KernelCallStackInstruction, 0x1000, "", "l"); // Variable stack arguments
		OPCODE(0x69, "stopMusic", KernelCallStackInstruction, 0, "");
		OPCODE_MD(0x6A, "freezeUnfreeze", KernelCallStackInstruction, -1, "", "p");
		START_SUBOPCODE_WITH_PREFIX(0x6B, "cursorCommand");
			OPCODE(0x90, "cursorOn", KernelCallStackInstruction, 0, "");
			OPCODE(0x91, "cursorOff", KernelCallStackInstruction, 0, "");
			OPCODE(0x92, "userputOn", KernelCallStackInstruction, 0, "");
			OPCODE(0x93, "userputOff", KernelCallStackInstruction, 0, "");
			OPCODE(0x94, "softCursorOn", KernelCallStackInstruction, 0, "");
			OPCODE(0x95, "softCursorOff", KernelCallStackInstruction, 0, "");
			OPCODE(0x96, "softUserputOn", KernelCallStackInstruction, 0, "");
			OPCODE(0x97, "softUserputOff", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x99, "setCursorImg", KernelCallStackInstruction, -2, "", "z");
			OPCODE_MD(0x9A, "setCursorHotspot", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x9C, "initCharset", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x9D, "charsetColors", KernelCallStackInstruction, 0x1000, "", "l"); // Variable stack arguments
			OPCODE_MD(0xD6, "setCursorTransparency", KernelCallStackInstruction, -1, "", "p");
		END_SUBOPCODE;
		OPCODE(0x6C, "breakHere", KernelCallStackInstruction, 0, "");
		OPCODE_MD(0x6D, "ifClassOfIs", KernelCallStackInstruction, 0x1011, "", "rlp"); // Variable stack arguments
		OPCODE_MD(0x6E, "setClass", KernelCallStackInstruction, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0x6F, "getState", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x70, "setState", KernelCallStackInstruction, -2, "", "pp");
		OPCODE_MD(0x71, "setOwner", KernelCallStackInstruction, -2, "", "pp");
		OPCODE_MD(0x72, "getOwner", KernelCallStackInstruction, 0, "", "rp");
		OPCODE(0x73, "jump", Scummv6JumpInstruction, 0, "a");
		OPCODE_MD(0x74, "startSound", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0x75, "stopSound", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0x76, "startMusic", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0x77, "stopObjectScript", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0x78, "panCameraTo", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0x79, "actorFollowCamera", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0x7A, "setCameraAt", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0x7B, "loadRoom", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0x7C, "stopScript", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0x7D, "walkActorToObj", KernelCallStackInstruction, -3, "", "ppp");
		OPCODE_MD(0x7E, "walkActorTo", KernelCallStackInstruction, -3, "", "ppp");
		OPCODE_MD(0x7F, "putActorAtXY", KernelCallStackInstruction, -4, "", "pppp");
		OPCODE_MD(0x80, "putActorAtObject", KernelCallStackInstruction, -3, "", "zp");
		OPCODE_MD(0x81, "faceActor", KernelCallStackInstruction, -2, "", "pp");
		OPCODE_MD(0x82, "animateActor", KernelCallStackInstruction, -2, "", "pp");
		OPCODE_MD(0x83, "doSentence", KernelCallStackInstruction, -4, "", "pppp");
		OPCODE_MD(0x84, "pickupObject", KernelCallStackInstruction, -2, "", "z");
		OPCODE_MD(0x85, "loadRoomWithEgo", KernelCallStackInstruction, -4, "", "ppz");
		OPCODE_MD(0x87, "getRandomNumber", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x88, "getRandomNumberRange", KernelCallStackInstruction, -1, "", "rpp");
		OPCODE_MD(0x8A, "getActorMoving", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x8B, "isScriptRunning", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x8C, "getActorRoom", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x8D, "getObjectX", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x8E, "getObjectY", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x8F, "getObjectDir", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x90, "getActorWalkBox", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x91, "getActorCostume", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x92, "findInventory", KernelCallStackInstruction, -1, "", "rpp");
		OPCODE_MD(0x93, "getInventoryCount", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x94, "getVerbFromXY", KernelCallStackInstruction, -1, "", "rpp");
		OPCODE_BASE(0x95)
			OPCODE_BODY("beginOverride", KernelCallStackInstruction, 0, "", "");
			// FIXME/TODO: beginOverride skips the following jump - that jump is instead to a "finally" handler
			// To simulate this, we simply skip the jump instruction, so the sequential order appears the same.
			// Semantically, it would probably be more correct to model this as a conditional jump,
			// but since precious little time remains of GSoC and that might not play very nice with the rest of the code,
			// this is the simplest way to handle the issue right now. Eventually, it should probably be fixed more properly.
			_f.seek(3, SEEK_CUR);
			_address += 3;
			OPCODE_END;
		OPCODE(0x96, "endOverride", KernelCallStackInstruction, 0, "");
		OPCODE_MD(0x97, "setObjectName", KernelCallStackInstruction, -1, "c", "ps");
		OPCODE_MD(0x98, "isSoundRunning", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0x99, "setBoxFlags", KernelCallStackInstruction, 0x1100, "", "pl"); // Variable stack arguments
		OPCODE(0x9A, "createBoxMatrix", KernelCallStackInstruction, 0, "");
		START_SUBOPCODE_WITH_PREFIX(0x9B, "resourceRoutines");
			OPCODE_MD(0x64, "loadScript", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x65, "loadSound", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x66, "loadCostume", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x67, "loadRoom", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x68, "nukeScript", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x69, "nukeSound", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x6A, "nukeCostume", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x6B, "nukeRoom", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x6C, "lockScript", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x6D, "lockSound", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x6E, "lockCostume", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x6F, "lockRoom", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x70, "unlockScript", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x71, "unlockSound", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x72, "unlockCostume", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x73, "unlockRoom", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x75, "loadCharset", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x76, "nukeCharset", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x77, "loadFlObject", KernelCallStackInstruction, -2, "", "pp");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9C, "roomOps");
			OPCODE_MD(0xAC, "roomScroll", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0xAE, "setScreen", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0xAF, "setPalColor", KernelCallStackInstruction, -4, "", "pppp");
			OPCODE(0xB0, "shakeOn", KernelCallStackInstruction, 0, "");
			OPCODE(0xB1, "shakeOff", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0xB3, "darkenPalette", KernelCallStackInstruction, -3, "", "ppp");
			OPCODE_MD(0xB4, "saveLoadRoom", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0xB5, "screenEffect", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0xB6, "darkenPaletteRGB", KernelCallStackInstruction, -5, "", "ppppp");
			OPCODE_MD(0xB7, "setupShadowPalette", KernelCallStackInstruction, -5, "", "ppppp");
			OPCODE_MD(0xBA, "palManipulate", KernelCallStackInstruction, -4, "", "pppp");
			OPCODE_MD(0xBB, "colorCycleDelay", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0xD5, "setPalette", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0xDC, "copyPalColor", KernelCallStackInstruction, -2, "", "pp");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9D, "actorOps");
			OPCODE_MD(0x4C, "setCostume", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x4D, "setWalkSpeed", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x4E, "setSound", KernelCallStackInstruction, 0x1000, "", "l"); // Variable stack arguments
			OPCODE_MD(0x4F, "setWalkFrame", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x50, "setTalkFrame", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x51, "setStandFrame", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x52, "82?", KernelCallStackInstruction, -3, "", "ppp");
			OPCODE(0x53, "init", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x54, "setElevation", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x55, "setDefAnim", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x56, "setPalette", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x57, "setTalkColor", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x58, "setName", KernelCallStackInstruction, 0, "c", "s");
			OPCODE_MD(0x59, "setInitFrame", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x5B, "setWidth", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x5C, "setScale", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x5D, "setNeverZClip", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x5E, "setAlwaysZClip", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x5F, "setIgnoreBoxes", KernelCallStackInstruction, 0, "");
			OPCODE(0x60, "setFollowBoxes", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x61, "setAnimSpeed", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x62, "setShadowMode", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x63, "setTalkPos", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0xC5, "setCurActor", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0xC6, "setAnimVar", KernelCallStackInstruction, -2, "", "pp");
			OPCODE(0xD7, "setIgnoreTurnsOn", KernelCallStackInstruction, 0, "");
			OPCODE(0xD8, "setIgnoreTurnsOff", KernelCallStackInstruction, 0, "");
			OPCODE(0xD9, "initLittle", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0xE1, "setAlwaysZClip?", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0xE3, "setLayer", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0xE4, "setWalkScript", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0xE5, "setStanding", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0xE6, "setDirection", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0xE7, "turnToDirection", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0xE9, "freeze", KernelCallStackInstruction, 0, "");
			OPCODE(0xEA, "unfreeze", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0xEB, "setTalkScript", KernelCallStackInstruction, -1, "", "p");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9E, "verbOps");
			OPCODE_MD(0x7C, "loadImg", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x7D, "loadString", KernelCallStackInstruction, 0, "c", "s");
			OPCODE_MD(0x7E, "setColor", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x7F, "setHiColor", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x80, "setXY", KernelCallStackInstruction, -2, "", "pp");
			OPCODE(0x81, "setOn", KernelCallStackInstruction, 0, "");
			OPCODE(0x82, "setOff", KernelCallStackInstruction, 0, "");
			OPCODE(0x83, "kill", KernelCallStackInstruction, 0, "");
			OPCODE(0x84, "init", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x85, "setDimColor", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x86, "setDimmed", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x87, "setKey", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x88, "setCenter", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x89, "setToString", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x8B, "setToObject", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x8C, "setBkColor", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0xC4, "setCurVerb", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0xFF, "redraw", KernelCallStackInstruction, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0x9F, "getActorFromXY", KernelCallStackInstruction, -1, "", "rpp");
		OPCODE_MD(0xA0, "findObject", KernelCallStackInstruction, -1, "", "rpp");
		OPCODE_MD(0xA1, "pseudoRoom", KernelCallStackInstruction, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0xA2, "getActorElevation", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0xA3, "getVerbEntrypoint", KernelCallStackInstruction, -1, "", "rpp");
		START_SUBOPCODE_WITH_PREFIX(0xA4, "arrayOps");
			OPCODE_MD(0xCD, "assignString", Scummv6ArrayOpInstruction, -1, "wc", "");
			OPCODE_MD(0xD0, "assignIntList", Scummv6ArrayOpInstruction, 0x1100, "w", ""); // Variable stack arguments
			OPCODE_MD(0xD4, "assign2DimList", Scummv6ArrayOpInstruction, 0x1100, "w", ""); // Variable stack arguments
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xA5, "saveRestoreVerbs");
			OPCODE_MD(0x8D, "saveVerbs", KernelCallStackInstruction, -3, "", "ppp");
			OPCODE_MD(0x8E, "restoreVerbs", KernelCallStackInstruction, -3, "", "ppp");
			OPCODE_MD(0x8F, "deleteVerbs", KernelCallStackInstruction, -3, "", "ppp");
		END_SUBOPCODE;
		OPCODE_MD(0xA6, "drawBox", KernelCallStackInstruction, -5, "", "ppppp");
		OPCODE(0xA7, "pop", Scummv6StackInstruction, -1, "");
		OPCODE_MD(0xA8, "getActorWidth", KernelCallStackInstruction, 0, "", "rp");
		START_SUBOPCODE(0xA9); // wait
			OPCODE_MD(0xA8, "waitForActor", KernelCallStackInstruction, -1, "s", "pj");
			OPCODE(0xA9, "waitForMessage", KernelCallStackInstruction, 0, "");
			OPCODE(0xAA, "waitForCamera", KernelCallStackInstruction, 0, "");
			OPCODE(0xAB, "waitForSentence", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0xE2, "waitUntilActorDrawn", KernelCallStackInstruction, -1, "s", "pj");
			OPCODE_MD(0xE8, "waitUntilActorTurned", KernelCallStackInstruction, -1, "s", "pj");
		END_SUBOPCODE;
		OPCODE_MD(0xAA, "getActorScaleX", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0xAB, "getActorAnimCounter", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0xAC, "soundKludge", KernelCallStackInstruction, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xAD, "isAnyOf", KernelCallStackInstruction, 0x1011, "", "rlp"); // Variable stack arguments
		START_SUBOPCODE_WITH_PREFIX(0xAE, "systemOps");
			OPCODE(0x9E, "restartGame", KernelCallStackInstruction, 0, "");
			OPCODE(0x9F, "pauseGame", KernelCallStackInstruction, 0, "");
			OPCODE(0xA0, "shutDown", KernelCallStackInstruction, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0xAF, "isActorInBox", KernelCallStackInstruction, -1, "", "rpp");
		OPCODE_MD(0xB0, "delay", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0xB1, "delaySeconds", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0xB2, "delayMinutes", KernelCallStackInstruction, -1, "", "p");
		OPCODE(0xB3, "stopSentence", KernelCallStackInstruction, 0, "");
		START_SUBOPCODE_WITH_PREFIX(0xB4, "printLine");
			OPCODE_MD(0x41, "XY", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x42, "color", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x43, "right", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x45, "center", KernelCallStackInstruction, 0, "");
			OPCODE(0x47, "left", KernelCallStackInstruction, 0, "");
			OPCODE(0x48, "overhead", KernelCallStackInstruction, 0, "");
			OPCODE(0x4A, "mumble", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x4B, "msg", KernelCallStackInstruction, 0, "c", "s");
			OPCODE(0xFE, "begin", KernelCallStackInstruction, 0, "");
			OPCODE(0xFF, "end", KernelCallStackInstruction, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB5, "printText");
			OPCODE_MD(0x41, "XY", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x42, "color", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x43, "right", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x45, "center", KernelCallStackInstruction, 0, "");
			OPCODE(0x47, "left", KernelCallStackInstruction, 0, "");
			OPCODE(0x48, "overhead", KernelCallStackInstruction, 0, "");
			OPCODE(0x4A, "mumble", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x4B, "msg", KernelCallStackInstruction, 0, "c", "s");
			OPCODE(0xFE, "begin", KernelCallStackInstruction, 0, "");
			OPCODE(0xFF, "end", KernelCallStackInstruction, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB6, "printDebug");
			OPCODE_MD(0x41, "XY", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x42, "color", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x43, "right", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x45, "center", KernelCallStackInstruction, 0, "");
			OPCODE(0x47, "left", KernelCallStackInstruction, 0, "");
			OPCODE(0x48, "overhead", KernelCallStackInstruction, 0, "");
			OPCODE(0x4A, "mumble", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x4B, "msg", KernelCallStackInstruction, 0, "c", "s");
			OPCODE(0xFE, "begin", KernelCallStackInstruction, 0, "");
			OPCODE(0xFF, "end", KernelCallStackInstruction, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB7, "printSystem");
			OPCODE_MD(0x41, "XY", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x42, "color", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x43, "right", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x45, "center", KernelCallStackInstruction, 0, "");
			OPCODE(0x47, "left", KernelCallStackInstruction, 0, "");
			OPCODE(0x48, "overhead", KernelCallStackInstruction, 0, "");
			OPCODE(0x4A, "mumble", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x4B, "msg", KernelCallStackInstruction, 0, "c", "s");
			OPCODE(0xFE, "begin", KernelCallStackInstruction, 0, "");
			OPCODE(0xFF, "end", KernelCallStackInstruction, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB8, "printActor");
			OPCODE_MD(0x41, "XY", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x42, "color", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x43, "right", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x45, "center", KernelCallStackInstruction, 0, "");
			OPCODE(0x47, "left", KernelCallStackInstruction, 0, "");
			OPCODE(0x48, "overhead", KernelCallStackInstruction, 0, "");
			OPCODE(0x4A, "mumble", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x4B, "msg", KernelCallStackInstruction, 0, "c", "s");
			OPCODE_MD(0xFE, "begin", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0xFF, "end", KernelCallStackInstruction, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB9, "printEgo");
			OPCODE_MD(0x41, "XY", KernelCallStackInstruction, -2, "", "pp");
			OPCODE_MD(0x42, "color", KernelCallStackInstruction, -1, "", "p");
			OPCODE_MD(0x43, "right", KernelCallStackInstruction, -1, "", "p");
			OPCODE(0x45, "center", KernelCallStackInstruction, 0, "");
			OPCODE(0x47, "left", KernelCallStackInstruction, 0, "");
			OPCODE(0x48, "overhead", KernelCallStackInstruction, 0, "");
			OPCODE(0x4A, "mumble", KernelCallStackInstruction, 0, "");
			OPCODE_MD(0x4B, "msg", KernelCallStackInstruction, 0, "c", "s");
			OPCODE(0xFE, "begin", KernelCallStackInstruction, 0, "");
			OPCODE(0xFF, "end", KernelCallStackInstruction, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0xBA, "talkActor", KernelCallStackInstruction, -1, "c", "ps");
		OPCODE_MD(0xBB, "talkEgo", KernelCallStackInstruction, 0, "c", "s");
		START_SUBOPCODE(0xBC); // dimArray
			OPCODE_MD(0xC7, "dimArrayInt", KernelCallStackInstruction, -1, "w", "pv");
			OPCODE_MD(0xC8, "dimArrayBit", KernelCallStackInstruction, -1, "w", "pv");
			OPCODE_MD(0xC9, "dimArrayNibble", KernelCallStackInstruction, -1, "w", "pv");
			OPCODE_MD(0xCA, "dimArrayByte", KernelCallStackInstruction, -1, "w", "pv");
			OPCODE_MD(0xCB, "dimArrayString", KernelCallStackInstruction, -1, "w", "pv");
			OPCODE_MD(0xCC, "dimArray_nukeArray", KernelCallStackInstruction, 0, "w", "v");
		END_SUBOPCODE;
		OPCODE_MD(0xBE, "startObjectQuick", KernelCallStackInstruction, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0xBF, "startScriptQuick2", KernelCallStackInstruction, 0x1010, "", "lp"); // Variable stack arguments
		START_SUBOPCODE(0xC0); // dim2DimArray
			OPCODE_MD(0xC7, "dim2DimArrayInt", KernelCallStackInstruction, -2, "w", "ppv");
			OPCODE_MD(0xC8, "dim2DimArrayBit", KernelCallStackInstruction, -2, "w", "ppv");
			OPCODE_MD(0xC9, "dim2DimArrayNibble", KernelCallStackInstruction, -2, "w", "ppv");
			OPCODE_MD(0xCA, "dim2DimArrayByte", KernelCallStackInstruction, -2, "w", "ppv");
			OPCODE_MD(0xCB, "dim2DimArrayString", KernelCallStackInstruction, -2, "w", "ppv");
		END_SUBOPCODE;
		OPCODE_MD(0xC4, "abs", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0xC5, "getDistObjObj", KernelCallStackInstruction, -1, "", "rpp");
		OPCODE_MD(0xC6, "getDistObjPt", KernelCallStackInstruction, -2, "", "rppp");
		OPCODE_MD(0xC7, "getDistPtPt", KernelCallStackInstruction, -3, "", "rpppp");
		OPCODE_MD(0xC8, "kernelGetFunctions", KernelCallStackInstruction, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xC9, "kernelSetFunctions", KernelCallStackInstruction, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xCA, "delayFrames", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0xCB, "pickOneOf", KernelCallStackInstruction, 0x1011, "", "rlp"); // Variable stack arguments
		OPCODE_MD(0xCC, "pickOneOfDefault", KernelCallStackInstruction, 0x111, "", "rplp"); // Variable stack arguments
		OPCODE_MD(0xCD, "stampObject", KernelCallStackInstruction, -4, "", "pppp");
		OPCODE(0xD0, "getDateTime", KernelCallStackInstruction, 0, "");
		OPCODE(0xD1, "stopTalking", KernelCallStackInstruction, 0, "");
		OPCODE_MD(0xD2, "getAnimateVariable", KernelCallStackInstruction, -1, "", "rpp");
		OPCODE_MD(0xD4, "shuffle", KernelCallStackInstruction, -2, "w", "vpp");
		OPCODE_MD(0xD5, "jumpToScript", KernelCallStackInstruction, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0xD6, "band", BinaryOpStackInstruction, -1, "", "&");
		OPCODE_MD(0xD7, "bor", BinaryOpStackInstruction, -1, "", "|");
		OPCODE_MD(0xD8, "isRoomScriptRunning", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0xDD, "findAllObjects", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0xE1, "getPixel", KernelCallStackInstruction, -1, "", "rpp");
		OPCODE_MD(0xE3, "pickVarRandom", KernelCallStackInstruction, 0x1001, "w", "rlw"); // Variable stack arguments
		OPCODE_MD(0xE4, "setBoxSet", KernelCallStackInstruction, -1, "", "p");
		OPCODE_MD(0xEC, "getActorLayer", KernelCallStackInstruction, 0, "", "rp");
		OPCODE_MD(0xED, "getObjectNewDir", KernelCallStackInstruction, 0, "", "rp");
	END_OPCODES;

	InstIterator it;
	for (it = _insts.begin(); it != _insts.end(); ++it)
		if ((*it)->_stackChange >= 0x1000)
			fixStackEffect(it, ((*it)->_stackChange >> 8) & 0xF, ((*it)->_stackChange >> 4) & 0xF, (*it)->_stackChange & 0xF);
}

void Scumm::v6::Scummv6Disassembler::fixStackEffect(InstIterator &it, int popBefore, int popAfter, int pushTotal) {
	(*it)->_stackChange = -popBefore - popAfter + pushTotal;
	InstIterator it2 = it;
	for (--it2; popBefore != 0; --it2)
		if ((*it2)->isLoad())
			--popBefore;
	(*it)->_stackChange -= (*it2)->_params[0]->getSigned() + 1;
}

ValuePtr Scumm::v6::Scummv6Disassembler::readParameter(InstPtr inst, char type) {
	ValuePtr retval = NULL;
	switch (type) {
	case 'a':
		retval = new RelAddressValue(inst->_address + 3, _f.readSint16LE());
		_address += 2;
		break;
	case 'c': { // Character string
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
				case 4:     // addIntToStack
				case 5:     // addVerbToStack
				case 6:     // addNameToStack
				case 7: {   // addStringToStack
					uint16 var = _f.readUint16LE();
					// TODO: Clean output similar to descumm
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
		_address++;
		if (inStr)
			s << '"';
		retval = new Scummv6StringValue(s.str());
		}
		break;
	default: // Defer handling to parent implementation
		retval = SimpleDisassembler::readParameter(inst, type);
		break;
	}
	return retval;
}
