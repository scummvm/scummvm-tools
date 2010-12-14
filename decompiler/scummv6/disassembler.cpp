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
	_instFactory.addEntry<Scummv6CondJumpInstruction>(kCondJumpInst);
	_instFactory.addEntry<Scummv6JumpInstruction>(kJumpInst);
	_instFactory.addEntry<Scummv6LoadInstruction>(kLoadInst);
	_instFactory.addEntry<Scummv6StackInstruction>(kStackInst);
	_instFactory.addEntry<Scummv6StoreInstruction>(kStoreInst);
	_instFactory.addEntry<Scummv6ArrayOpInstruction>(kArrayOpInst);
	_instFactory.addEntry<Scummv6IncDecInstruction>(kIncDecInst);
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
		OPCODE(0x00, "pushByte", kLoadInst, 1, "B");
		OPCODE(0x01, "pushWord", kLoadInst, 1, "s");
		OPCODE(0x02, "pushByteVar", kLoadInst, 1, "B");
		OPCODE(0x03, "pushWordVar", kLoadInst, 1, "w");
		OPCODE(0x06, "byteArrayRead", kLoadInst, 0, "B");
		OPCODE(0x07, "wordArrayRead", kLoadInst, 0, "w");
		OPCODE(0x0A, "byteArrayIndexedRead", kLoadInst, -1, "B");
		OPCODE(0x0B, "wordArrayIndexedRead", kLoadInst, -1, "w");
		OPCODE(0x0C, "dup", kDupInst, 1, "");
		OPCODE(0x0D, "not", kBoolNegateInst, 0, "");
		OPCODE_MD(0x0E, "eq", kBinaryOpInst, -1, "", "==");
		OPCODE_MD(0x0F, "neq", kBinaryOpInst, -1, "", "!=");
		OPCODE_MD(0x10, "gt", kBinaryOpInst, -1, "", ">");
		OPCODE_MD(0x11, "lt", kBinaryOpInst, -1, "", "<");
		OPCODE_MD(0x12, "le", kBinaryOpInst, -1, "", "<=");
		OPCODE_MD(0x13, "ge", kBinaryOpInst, -1, "", ">=");
		OPCODE_MD(0x14, "add", kBinaryOpInst, -1, "", "+");
		OPCODE_MD(0x15, "sub", kBinaryOpInst, -1, "", "-");
		OPCODE_MD(0x16, "mul", kBinaryOpInst, -1, "", "*");
		OPCODE_MD(0x17, "div", kBinaryOpInst, -1, "", "/");
		OPCODE_MD(0x18, "land", kBinaryOpInst, -1, "", "&&");
		OPCODE_MD(0x19, "lor", kBinaryOpInst, -1, "", "||");
		OPCODE(0x1A, "pop", kStackInst, -1, "");
		OPCODE(0x42, "writeByteVar", kStoreInst, -1, "B");
		OPCODE(0x43, "writeWordVar", kStoreInst, -1, "w");
		OPCODE(0x46, "byteArrayWrite", kStoreInst, -2, "B");
		OPCODE(0x47, "wordArrayWrite", kStoreInst, -2, "w");
		OPCODE(0x4A, "byteArrayIndexedWrite", kStoreInst, -3, "B");
		OPCODE(0x4B, "wordArrayIndexedWrite", kStoreInst, -3, "w");
		OPCODE_MD(0x4E, "byteVarInc", kIncDecInst, 0, "B", "++");
		OPCODE_MD(0x4F, "wordVarInc", kIncDecInst, 0, "w", "++");
		OPCODE_MD(0x52, "byteArrayInc", kIncDecInst, -1, "B", "++");
		OPCODE_MD(0x53, "wordArrayInc", kIncDecInst, -1, "w", "++");
		OPCODE_MD(0x56, "byteVarDec", kIncDecInst, 0, "B", "--");
		OPCODE_MD(0x57, "wordVarDec", kIncDecInst, 0, "w", "--");
		OPCODE_MD(0x5A, "byteArrayDec", kIncDecInst, -1, "B", "--");
		OPCODE_MD(0x5B, "wordArrayDec", kIncDecInst, -1, "w", "--");
		OPCODE(0x5C, "jumpTrue", kCondJumpInst, -1, "a");
		OPCODE(0x5D, "jumpFalse", kCondJumpInst, -1, "a");
		OPCODE_MD(0x5E, "startScript", kKernelCallInst, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0x5F, "startScriptQuick", kKernelCallInst, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0x60, "startObject", kKernelCallInst, 0x1030, "", "lppp"); // Variable stack arguments
		OPCODE_MD(0x61, "drawObject", kKernelCallInst, -2, "", "pp");
		OPCODE_MD(0x62, "drawObjectAt", kKernelCallInst, -3, "", "ppp");
		OPCODE_MD(0x63, "drawBlastObject", kKernelCallInst, -5, "", "ppppp");
		OPCODE_MD(0x64, "setBlastObjectWindow", kKernelCallInst, -4, "", "pppp");
		OPCODE(0x65, "stopObjectCodeA", kKernelCallInst, 0, "");
		OPCODE(0x66, "stopObjectCodeB", kKernelCallInst, 0, "");
		OPCODE(0x67, "endCutscene", kKernelCallInst, 0, "");
		OPCODE_MD(0x68, "beginCutscene", kKernelCallInst, 0x1000, "", "l"); // Variable stack arguments
		OPCODE(0x69, "stopMusic", kKernelCallInst, 0, "");
		OPCODE_MD(0x6A, "freezeUnfreeze", kKernelCallInst, -1, "", "p");
		START_SUBOPCODE_WITH_PREFIX(0x6B, "cursorCommand");
			OPCODE(0x90, "cursorOn", kKernelCallInst, 0, "");
			OPCODE(0x91, "cursorOff", kKernelCallInst, 0, "");
			OPCODE(0x92, "userputOn", kKernelCallInst, 0, "");
			OPCODE(0x93, "userputOff", kKernelCallInst, 0, "");
			OPCODE(0x94, "softCursorOn", kKernelCallInst, 0, "");
			OPCODE(0x95, "softCursorOff", kKernelCallInst, 0, "");
			OPCODE(0x96, "softUserputOn", kKernelCallInst, 0, "");
			OPCODE(0x97, "softUserputOff", kKernelCallInst, 0, "");
			OPCODE_MD(0x99, "setCursorImg", kKernelCallInst, -2, "", "z");
			OPCODE_MD(0x9A, "setCursorHotspot", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x9C, "initCharset", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x9D, "charsetColors", kKernelCallInst, 0x1000, "", "l"); // Variable stack arguments
			OPCODE_MD(0xD6, "setCursorTransparency", kKernelCallInst, -1, "", "p");
		END_SUBOPCODE;
		OPCODE(0x6C, "breakHere", kKernelCallInst, 0, "");
		OPCODE_MD(0x6D, "ifClassOfIs", kKernelCallInst, 0x1011, "", "rlp"); // Variable stack arguments
		OPCODE_MD(0x6E, "setClass", kKernelCallInst, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0x6F, "getState", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x70, "setState", kKernelCallInst, -2, "", "pp");
		OPCODE_MD(0x71, "setOwner", kKernelCallInst, -2, "", "pp");
		OPCODE_MD(0x72, "getOwner", kKernelCallInst, 0, "", "rp");
		OPCODE(0x73, "jump", kJumpInst, 0, "a");
		OPCODE_MD(0x74, "startSound", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0x75, "stopSound", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0x76, "startMusic", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0x77, "stopObjectScript", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0x78, "panCameraTo", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0x79, "actorFollowCamera", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0x7A, "setCameraAt", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0x7B, "loadRoom", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0x7C, "stopScript", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0x7D, "walkActorToObj", kKernelCallInst, -3, "", "ppp");
		OPCODE_MD(0x7E, "walkActorTo", kKernelCallInst, -3, "", "ppp");
		OPCODE_MD(0x7F, "putActorAtXY", kKernelCallInst, -4, "", "pppp");
		OPCODE_MD(0x80, "putActorAtObject", kKernelCallInst, -3, "", "zp");
		OPCODE_MD(0x81, "faceActor", kKernelCallInst, -2, "", "pp");
		OPCODE_MD(0x82, "animateActor", kKernelCallInst, -2, "", "pp");
		OPCODE_MD(0x83, "doSentence", kKernelCallInst, -4, "", "pppp");
		OPCODE_MD(0x84, "pickupObject", kKernelCallInst, -2, "", "z");
		OPCODE_MD(0x85, "loadRoomWithEgo", kKernelCallInst, -4, "", "ppz");
		OPCODE_MD(0x87, "getRandomNumber", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x88, "getRandomNumberRange", kKernelCallInst, -1, "", "rpp");
		OPCODE_MD(0x8A, "getActorMoving", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x8B, "isScriptRunning", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x8C, "getActorRoom", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x8D, "getObjectX", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x8E, "getObjectY", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x8F, "getObjectDir", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x90, "getActorWalkBox", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x91, "getActorCostume", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x92, "findInventory", kKernelCallInst, -1, "", "rpp");
		OPCODE_MD(0x93, "getInventoryCount", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x94, "getVerbFromXY", kKernelCallInst, -1, "", "rpp");
		OPCODE_BASE(0x95)
			OPCODE_BODY("beginOverride", kKernelCallInst, 0, "", "");
			// FIXME/TODO: beginOverride skips the following jump - that jump is instead to a "finally" handler
			// To simulate this, we simply skip the jump instruction, so the sequential order appears the same.
			// Semantically, it would probably be more correct to model this as a conditional jump,
			// but since precious little time remains of GSoC and that might not play very nice with the rest of the code,
			// this is the simplest way to handle the issue right now. Eventually, it should probably be fixed more properly.
			_f.seek(3, SEEK_CUR);
			_address += 3;
			OPCODE_END;
		OPCODE(0x96, "endOverride", kKernelCallInst, 0, "");
		OPCODE_MD(0x97, "setObjectName", kKernelCallInst, -1, "c", "ps");
		OPCODE_MD(0x98, "isSoundRunning", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0x99, "setBoxFlags", kKernelCallInst, 0x1100, "", "pl"); // Variable stack arguments
		OPCODE(0x9A, "createBoxMatrix", kKernelCallInst, 0, "");
		START_SUBOPCODE_WITH_PREFIX(0x9B, "resourceRoutines");
			OPCODE_MD(0x64, "loadScript", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x65, "loadSound", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x66, "loadCostume", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x67, "loadRoom", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x68, "nukeScript", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x69, "nukeSound", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x6A, "nukeCostume", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x6B, "nukeRoom", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x6C, "lockScript", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x6D, "lockSound", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x6E, "lockCostume", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x6F, "lockRoom", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x70, "unlockScript", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x71, "unlockSound", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x72, "unlockCostume", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x73, "unlockRoom", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x75, "loadCharset", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x76, "nukeCharset", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x77, "loadFlObject", kKernelCallInst, -2, "", "pp");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9C, "roomOps");
			OPCODE_MD(0xAC, "roomScroll", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0xAE, "setScreen", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0xAF, "setPalColor", kKernelCallInst, -4, "", "pppp");
			OPCODE(0xB0, "shakeOn", kKernelCallInst, 0, "");
			OPCODE(0xB1, "shakeOff", kKernelCallInst, 0, "");
			OPCODE_MD(0xB3, "darkenPalette", kKernelCallInst, -3, "", "ppp");
			OPCODE_MD(0xB4, "saveLoadRoom", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0xB5, "screenEffect", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0xB6, "darkenPaletteRGB", kKernelCallInst, -5, "", "ppppp");
			OPCODE_MD(0xB7, "setupShadowPalette", kKernelCallInst, -5, "", "ppppp");
			OPCODE_MD(0xBA, "palManipulate", kKernelCallInst, -4, "", "pppp");
			OPCODE_MD(0xBB, "colorCycleDelay", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0xD5, "setPalette", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0xDC, "copyPalColor", kKernelCallInst, -2, "", "pp");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9D, "actorOps");
			OPCODE_MD(0x4C, "setCostume", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x4D, "setWalkSpeed", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x4E, "setSound", kKernelCallInst, 0x1000, "", "l"); // Variable stack arguments
			OPCODE_MD(0x4F, "setWalkFrame", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x50, "setTalkFrame", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x51, "setStandFrame", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x52, "82?", kKernelCallInst, -3, "", "ppp");
			OPCODE(0x53, "init", kKernelCallInst, 0, "");
			OPCODE_MD(0x54, "setElevation", kKernelCallInst, -1, "", "p");
			OPCODE(0x55, "setDefAnim", kKernelCallInst, 0, "");
			OPCODE_MD(0x56, "setPalette", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x57, "setTalkColor", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x58, "setName", kKernelCallInst, 0, "c", "s");
			OPCODE_MD(0x59, "setInitFrame", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x5B, "setWidth", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x5C, "setScale", kKernelCallInst, -1, "", "p");
			OPCODE(0x5D, "setNeverZClip", kKernelCallInst, 0, "");
			OPCODE_MD(0x5E, "setAlwaysZClip", kKernelCallInst, -1, "", "p");
			OPCODE(0x5F, "setIgnoreBoxes", kKernelCallInst, 0, "");
			OPCODE(0x60, "setFollowBoxes", kKernelCallInst, 0, "");
			OPCODE_MD(0x61, "setAnimSpeed", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x62, "setShadowMode", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x63, "setTalkPos", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0xC5, "setCurActor", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0xC6, "setAnimVar", kKernelCallInst, -2, "", "pp");
			OPCODE(0xD7, "setIgnoreTurnsOn", kKernelCallInst, 0, "");
			OPCODE(0xD8, "setIgnoreTurnsOff", kKernelCallInst, 0, "");
			OPCODE(0xD9, "initLittle", kKernelCallInst, 0, "");
			OPCODE_MD(0xE1, "setAlwaysZClip?", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0xE3, "setLayer", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0xE4, "setWalkScript", kKernelCallInst, -1, "", "p");
			OPCODE(0xE5, "setStanding", kKernelCallInst, 0, "");
			OPCODE_MD(0xE6, "setDirection", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0xE7, "turnToDirection", kKernelCallInst, -1, "", "p");
			OPCODE(0xE9, "freeze", kKernelCallInst, 0, "");
			OPCODE(0xEA, "unfreeze", kKernelCallInst, 0, "");
			OPCODE_MD(0xEB, "setTalkScript", kKernelCallInst, -1, "", "p");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9E, "verbOps");
			OPCODE_MD(0x7C, "loadImg", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x7D, "loadString", kKernelCallInst, 0, "c", "s");
			OPCODE_MD(0x7E, "setColor", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x7F, "setHiColor", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x80, "setXY", kKernelCallInst, -2, "", "pp");
			OPCODE(0x81, "setOn", kKernelCallInst, 0, "");
			OPCODE(0x82, "setOff", kKernelCallInst, 0, "");
			OPCODE(0x83, "kill", kKernelCallInst, 0, "");
			OPCODE(0x84, "init", kKernelCallInst, 0, "");
			OPCODE_MD(0x85, "setDimColor", kKernelCallInst, -1, "", "p");
			OPCODE(0x86, "setDimmed", kKernelCallInst, 0, "");
			OPCODE_MD(0x87, "setKey", kKernelCallInst, -1, "", "p");
			OPCODE(0x88, "setCenter", kKernelCallInst, 0, "");
			OPCODE_MD(0x89, "setToString", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x8B, "setToObject", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x8C, "setBkColor", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0xC4, "setCurVerb", kKernelCallInst, -1, "", "p");
			OPCODE(0xFF, "redraw", kKernelCallInst, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0x9F, "getActorFromXY", kKernelCallInst, -1, "", "rpp");
		OPCODE_MD(0xA0, "findObject", kKernelCallInst, -1, "", "rpp");
		OPCODE_MD(0xA1, "pseudoRoom", kKernelCallInst, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0xA2, "getActorElevation", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0xA3, "getVerbEntrypoint", kKernelCallInst, -1, "", "rpp");
		START_SUBOPCODE_WITH_PREFIX(0xA4, "arrayOps");
			OPCODE_MD(0xCD, "assignString", kArrayOpInst, -1, "wc", "");
			OPCODE_MD(0xD0, "assignIntList", kArrayOpInst, 0x1100, "w", ""); // Variable stack arguments
			OPCODE_MD(0xD4, "assign2DimList", kArrayOpInst, 0x1100, "w", ""); // Variable stack arguments
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xA5, "saveRestoreVerbs");
			OPCODE_MD(0x8D, "saveVerbs", kKernelCallInst, -3, "", "ppp");
			OPCODE_MD(0x8E, "restoreVerbs", kKernelCallInst, -3, "", "ppp");
			OPCODE_MD(0x8F, "deleteVerbs", kKernelCallInst, -3, "", "ppp");
		END_SUBOPCODE;
		OPCODE_MD(0xA6, "drawBox", kKernelCallInst, -5, "", "ppppp");
		OPCODE(0xA7, "pop", kStackInst, -1, "");
		OPCODE_MD(0xA8, "getActorWidth", kKernelCallInst, 0, "", "rp");
		START_SUBOPCODE(0xA9); // wait
			OPCODE_MD(0xA8, "waitForActor", kKernelCallInst, -1, "s", "pj");
			OPCODE(0xA9, "waitForMessage", kKernelCallInst, 0, "");
			OPCODE(0xAA, "waitForCamera", kKernelCallInst, 0, "");
			OPCODE(0xAB, "waitForSentence", kKernelCallInst, 0, "");
			OPCODE_MD(0xE2, "waitUntilActorDrawn", kKernelCallInst, -1, "s", "pj");
			OPCODE_MD(0xE8, "waitUntilActorTurned", kKernelCallInst, -1, "s", "pj");
		END_SUBOPCODE;
		OPCODE_MD(0xAA, "getActorScaleX", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0xAB, "getActorAnimCounter", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0xAC, "soundKludge", kKernelCallInst, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xAD, "isAnyOf", kKernelCallInst, 0x1011, "", "rlp"); // Variable stack arguments
		START_SUBOPCODE_WITH_PREFIX(0xAE, "systemOps");
			OPCODE(0x9E, "restartGame", kKernelCallInst, 0, "");
			OPCODE(0x9F, "pauseGame", kKernelCallInst, 0, "");
			OPCODE(0xA0, "shutDown", kKernelCallInst, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0xAF, "isActorInBox", kKernelCallInst, -1, "", "rpp");
		OPCODE_MD(0xB0, "delay", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0xB1, "delaySeconds", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0xB2, "delayMinutes", kKernelCallInst, -1, "", "p");
		OPCODE(0xB3, "stopSentence", kKernelCallInst, 0, "");
		START_SUBOPCODE_WITH_PREFIX(0xB4, "printLine");
			OPCODE_MD(0x41, "XY", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x42, "color", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x43, "right", kKernelCallInst, -1, "", "p");
			OPCODE(0x45, "center", kKernelCallInst, 0, "");
			OPCODE(0x47, "left", kKernelCallInst, 0, "");
			OPCODE(0x48, "overhead", kKernelCallInst, 0, "");
			OPCODE(0x4A, "mumble", kKernelCallInst, 0, "");
			OPCODE_MD(0x4B, "msg", kKernelCallInst, 0, "c", "s");
			OPCODE(0xFE, "begin", kKernelCallInst, 0, "");
			OPCODE(0xFF, "end", kKernelCallInst, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB5, "printText");
			OPCODE_MD(0x41, "XY", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x42, "color", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x43, "right", kKernelCallInst, -1, "", "p");
			OPCODE(0x45, "center", kKernelCallInst, 0, "");
			OPCODE(0x47, "left", kKernelCallInst, 0, "");
			OPCODE(0x48, "overhead", kKernelCallInst, 0, "");
			OPCODE(0x4A, "mumble", kKernelCallInst, 0, "");
			OPCODE_MD(0x4B, "msg", kKernelCallInst, 0, "c", "s");
			OPCODE(0xFE, "begin", kKernelCallInst, 0, "");
			OPCODE(0xFF, "end", kKernelCallInst, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB6, "printDebug");
			OPCODE_MD(0x41, "XY", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x42, "color", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x43, "right", kKernelCallInst, -1, "", "p");
			OPCODE(0x45, "center", kKernelCallInst, 0, "");
			OPCODE(0x47, "left", kKernelCallInst, 0, "");
			OPCODE(0x48, "overhead", kKernelCallInst, 0, "");
			OPCODE(0x4A, "mumble", kKernelCallInst, 0, "");
			OPCODE_MD(0x4B, "msg", kKernelCallInst, 0, "c", "s");
			OPCODE(0xFE, "begin", kKernelCallInst, 0, "");
			OPCODE(0xFF, "end", kKernelCallInst, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB7, "printSystem");
			OPCODE_MD(0x41, "XY", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x42, "color", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x43, "right", kKernelCallInst, -1, "", "p");
			OPCODE(0x45, "center", kKernelCallInst, 0, "");
			OPCODE(0x47, "left", kKernelCallInst, 0, "");
			OPCODE(0x48, "overhead", kKernelCallInst, 0, "");
			OPCODE(0x4A, "mumble", kKernelCallInst, 0, "");
			OPCODE_MD(0x4B, "msg", kKernelCallInst, 0, "c", "s");
			OPCODE(0xFE, "begin", kKernelCallInst, 0, "");
			OPCODE(0xFF, "end", kKernelCallInst, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB8, "printActor");
			OPCODE_MD(0x41, "XY", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x42, "color", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x43, "right", kKernelCallInst, -1, "", "p");
			OPCODE(0x45, "center", kKernelCallInst, 0, "");
			OPCODE(0x47, "left", kKernelCallInst, 0, "");
			OPCODE(0x48, "overhead", kKernelCallInst, 0, "");
			OPCODE(0x4A, "mumble", kKernelCallInst, 0, "");
			OPCODE_MD(0x4B, "msg", kKernelCallInst, 0, "c", "s");
			OPCODE_MD(0xFE, "begin", kKernelCallInst, -1, "", "p");
			OPCODE(0xFF, "end", kKernelCallInst, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB9, "printEgo");
			OPCODE_MD(0x41, "XY", kKernelCallInst, -2, "", "pp");
			OPCODE_MD(0x42, "color", kKernelCallInst, -1, "", "p");
			OPCODE_MD(0x43, "right", kKernelCallInst, -1, "", "p");
			OPCODE(0x45, "center", kKernelCallInst, 0, "");
			OPCODE(0x47, "left", kKernelCallInst, 0, "");
			OPCODE(0x48, "overhead", kKernelCallInst, 0, "");
			OPCODE(0x4A, "mumble", kKernelCallInst, 0, "");
			OPCODE_MD(0x4B, "msg", kKernelCallInst, 0, "c", "s");
			OPCODE(0xFE, "begin", kKernelCallInst, 0, "");
			OPCODE(0xFF, "end", kKernelCallInst, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0xBA, "talkActor", kKernelCallInst, -1, "c", "ps");
		OPCODE_MD(0xBB, "talkEgo", kKernelCallInst, 0, "c", "s");
		START_SUBOPCODE(0xBC); // dimArray
			OPCODE_MD(0xC7, "dimArrayInt", kKernelCallInst, -1, "w", "pv");
			OPCODE_MD(0xC8, "dimArrayBit", kKernelCallInst, -1, "w", "pv");
			OPCODE_MD(0xC9, "dimArrayNibble", kKernelCallInst, -1, "w", "pv");
			OPCODE_MD(0xCA, "dimArrayByte", kKernelCallInst, -1, "w", "pv");
			OPCODE_MD(0xCB, "dimArrayString", kKernelCallInst, -1, "w", "pv");
			OPCODE_MD(0xCC, "dimArray_nukeArray", kKernelCallInst, 0, "w", "v");
		END_SUBOPCODE;
		OPCODE_MD(0xBE, "startObjectQuick", kKernelCallInst, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0xBF, "startScriptQuick2", kKernelCallInst, 0x1010, "", "lp"); // Variable stack arguments
		START_SUBOPCODE(0xC0); // dim2DimArray
			OPCODE_MD(0xC7, "dim2DimArrayInt", kKernelCallInst, -2, "w", "ppv");
			OPCODE_MD(0xC8, "dim2DimArrayBit", kKernelCallInst, -2, "w", "ppv");
			OPCODE_MD(0xC9, "dim2DimArrayNibble", kKernelCallInst, -2, "w", "ppv");
			OPCODE_MD(0xCA, "dim2DimArrayByte", kKernelCallInst, -2, "w", "ppv");
			OPCODE_MD(0xCB, "dim2DimArrayString", kKernelCallInst, -2, "w", "ppv");
		END_SUBOPCODE;
		OPCODE_MD(0xC4, "abs", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0xC5, "getDistObjObj", kKernelCallInst, -1, "", "rpp");
		OPCODE_MD(0xC6, "getDistObjPt", kKernelCallInst, -2, "", "rppp");
		OPCODE_MD(0xC7, "getDistPtPt", kKernelCallInst, -3, "", "rpppp");
		OPCODE_MD(0xC8, "kernelGetFunctions", kKernelCallInst, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xC9, "kernelSetFunctions", kKernelCallInst, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xCA, "delayFrames", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0xCB, "pickOneOf", kKernelCallInst, 0x1011, "", "rlp"); // Variable stack arguments
		OPCODE_MD(0xCC, "pickOneOfDefault", kKernelCallInst, 0x111, "", "rplp"); // Variable stack arguments
		OPCODE_MD(0xCD, "stampObject", kKernelCallInst, -4, "", "pppp");
		OPCODE(0xD0, "getDateTime", kKernelCallInst, 0, "");
		OPCODE(0xD1, "stopTalking", kKernelCallInst, 0, "");
		OPCODE_MD(0xD2, "getAnimateVariable", kKernelCallInst, -1, "", "rpp");
		OPCODE_MD(0xD4, "shuffle", kKernelCallInst, -2, "w", "vpp");
		OPCODE_MD(0xD5, "jumpToScript", kKernelCallInst, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0xD6, "band", kBinaryOpInst, -1, "", "&");
		OPCODE_MD(0xD7, "bor", kBinaryOpInst, -1, "", "|");
		OPCODE_MD(0xD8, "isRoomScriptRunning", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0xDD, "findAllObjects", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0xE1, "getPixel", kKernelCallInst, -1, "", "rpp");
		OPCODE_MD(0xE3, "pickVarRandom", kKernelCallInst, 0x1001, "w", "rlw"); // Variable stack arguments
		OPCODE_MD(0xE4, "setBoxSet", kKernelCallInst, -1, "", "p");
		OPCODE_MD(0xEC, "getActorLayer", kKernelCallInst, 0, "", "rp");
		OPCODE_MD(0xED, "getObjectNewDir", kKernelCallInst, 0, "", "rp");
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
