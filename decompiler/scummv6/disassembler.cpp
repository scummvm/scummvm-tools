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
		OPCODE(0x00, "pushByte", kLoadInstType, 1, "B");
		OPCODE(0x01, "pushWord", kLoadInstType, 1, "s");
		OPCODE(0x02, "pushByteVar", kLoadInstType, 1, "B");
		OPCODE(0x03, "pushWordVar", kLoadInstType, 1, "w");
		OPCODE(0x06, "byteArrayRead", kLoadInstType, 0, "B");
		OPCODE(0x07, "wordArrayRead", kLoadInstType, 0, "w");
		OPCODE(0x0A, "byteArrayIndexedRead", kLoadInstType, -1, "B");
		OPCODE(0x0B, "wordArrayIndexedRead", kLoadInstType, -1, "w");
		OPCODE(0x0C, "dup", kDupInstType, 1, "");
		OPCODE_MD(0x0D, "not", kUnaryOpPreInstType, 0, "", "!");
		OPCODE_MD(0x0E, "eq", kBinaryOpInstType, -1, "", "==");
		OPCODE_MD(0x0F, "neq", kBinaryOpInstType, -1, "", "!=");
		OPCODE_MD(0x10, "gt", kBinaryOpInstType, -1, "", ">");
		OPCODE_MD(0x11, "lt", kBinaryOpInstType, -1, "", "<");
		OPCODE_MD(0x12, "le", kBinaryOpInstType, -1, "", "<=");
		OPCODE_MD(0x13, "ge", kBinaryOpInstType, -1, "", ">=");
		OPCODE_MD(0x14, "add", kBinaryOpInstType, -1, "", "+");
		OPCODE_MD(0x15, "sub", kBinaryOpInstType, -1, "", "-");
		OPCODE_MD(0x16, "mul", kBinaryOpInstType, -1, "", "*");
		OPCODE_MD(0x17, "div", kBinaryOpInstType, -1, "", "/");
		OPCODE_MD(0x18, "land", kBinaryOpInstType, -1, "", "&&");
		OPCODE_MD(0x19, "lor", kBinaryOpInstType, -1, "", "||");
		OPCODE(0x1A, "pop", kStackInstType, -1, "");
		OPCODE(0x42, "writeByteVar", kStoreInstType, -1, "B");
		OPCODE(0x43, "writeWordVar", kStoreInstType, -1, "w");
		OPCODE(0x46, "byteArrayWrite", kStoreInstType, -2, "B");
		OPCODE(0x47, "wordArrayWrite", kStoreInstType, -2, "w");
		OPCODE(0x4A, "byteArrayIndexedWrite", kStoreInstType, -3, "B");
		OPCODE(0x4B, "wordArrayIndexedWrite", kStoreInstType, -3, "w");
		OPCODE_MD(0x4E, "byteVarInc", kUnaryOpPostInstType, 0, "B", "++");
		OPCODE_MD(0x4F, "wordVarInc", kUnaryOpPostInstType, 0, "w", "++");
		OPCODE_MD(0x52, "byteArrayInc", kUnaryOpPostInstType, -1, "B", "++");
		OPCODE_MD(0x53, "wordArrayInc", kUnaryOpPostInstType, -1, "w", "++");
		OPCODE_MD(0x56, "byteVarDec", kUnaryOpPostInstType, 0, "B", "--");
		OPCODE_MD(0x57, "wordVarDec", kUnaryOpPostInstType, 0, "w", "--");
		OPCODE_MD(0x5A, "byteArrayDec", kUnaryOpPostInstType, -1, "B", "--");
		OPCODE_MD(0x5B, "wordArrayDec", kUnaryOpPostInstType, -1, "w", "--");
		OPCODE(0x5C, "jumpTrue", kCondJumpRelInstType, -1, "s");
		OPCODE(0x5D, "jumpFalse", kCondJumpRelInstType, -1, "s");
		OPCODE_MD(0x5E, "startScript", kSpecialCallInstType, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0x5F, "startScriptQuick", kSpecialCallInstType, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0x60, "startObject", kSpecialCallInstType, 0x1030, "", "lppp"); // Variable stack arguments
		OPCODE_MD(0x61, "drawObject", kSpecialCallInstType, -2, "", "pp");
		OPCODE_MD(0x62, "drawObjectAt", kSpecialCallInstType, -3, "", "ppp");
		OPCODE_MD(0x63, "drawBlastObject", kSpecialCallInstType, -5, "", "ppppp");
		OPCODE_MD(0x64, "setBlastObjectWindow", kSpecialCallInstType, -4, "", "pppp");
		OPCODE(0x65, "stopObjectCodeA", kSpecialCallInstType, 0, "");
		OPCODE(0x66, "stopObjectCodeB", kSpecialCallInstType, 0, "");
		OPCODE(0x67, "endCutscene", kSpecialCallInstType, 0, "");
		OPCODE_MD(0x68, "beginCutscene", kSpecialCallInstType, 0x1000, "", "l"); // Variable stack arguments
		OPCODE(0x69, "stopMusic", kSpecialCallInstType, 0, "");
		OPCODE_MD(0x6A, "freezeUnfreeze", kSpecialCallInstType, -1, "", "p");
		START_SUBOPCODE_WITH_PREFIX(0x6B, "cursorCommand");
			OPCODE(0x90, "cursorOn", kSpecialCallInstType, 0, "");
			OPCODE(0x91, "cursorOff", kSpecialCallInstType, 0, "");
			OPCODE(0x92, "userputOn", kSpecialCallInstType, 0, "");
			OPCODE(0x93, "userputOff", kSpecialCallInstType, 0, "");
			OPCODE(0x94, "softCursorOn", kSpecialCallInstType, 0, "");
			OPCODE(0x95, "softCursorOff", kSpecialCallInstType, 0, "");
			OPCODE(0x96, "softUserputOn", kSpecialCallInstType, 0, "");
			OPCODE(0x97, "softUserputOff", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x99, "setCursorImg", kSpecialCallInstType, -2, "", "z");
			OPCODE_MD(0x9A, "setCursorHotspot", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x9C, "initCharset", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x9D, "charsetColors", kSpecialCallInstType, 0x1000, "", "l"); // Variable stack arguments
			OPCODE_MD(0xD6, "setCursorTransparency", kSpecialCallInstType, -1, "", "p");
		END_SUBOPCODE;
		OPCODE(0x6C, "breakHere", kSpecialCallInstType, 0, "");
		OPCODE_MD(0x6D, "ifClassOfIs", kSpecialCallInstType, 0x1011, "", "rlp"); // Variable stack arguments
		OPCODE_MD(0x6E, "setClass", kSpecialCallInstType, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0x6F, "getState", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x70, "setState", kSpecialCallInstType, -2, "", "pp");
		OPCODE_MD(0x71, "setOwner", kSpecialCallInstType, -2, "", "pp");
		OPCODE_MD(0x72, "getOwner", kSpecialCallInstType, 0, "", "rp");
		OPCODE(0x73, "jump", kJumpRelInstType, 0, "s");
		OPCODE_MD(0x74, "startSound", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0x75, "stopSound", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0x76, "startMusic", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0x77, "stopObjectScript", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0x78, "panCameraTo", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0x79, "actorFollowCamera", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0x7A, "setCameraAt", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0x7B, "loadRoom", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0x7C, "stopScript", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0x7D, "walkActorToObj", kSpecialCallInstType, -3, "", "ppp");
		OPCODE_MD(0x7E, "walkActorTo", kSpecialCallInstType, -3, "", "ppp");
		OPCODE_MD(0x7F, "putActorAtXY", kSpecialCallInstType, -4, "", "pppp");
		OPCODE_MD(0x80, "putActorAtObject", kSpecialCallInstType, -3, "", "zp");
		OPCODE_MD(0x81, "faceActor", kSpecialCallInstType, -2, "", "pp");
		OPCODE_MD(0x82, "animateActor", kSpecialCallInstType, -2, "", "pp");
		OPCODE_MD(0x83, "doSentence", kSpecialCallInstType, -4, "", "pppp");
		OPCODE_MD(0x84, "pickupObject", kSpecialCallInstType, -2, "", "z");
		OPCODE_MD(0x85, "loadRoomWithEgo", kSpecialCallInstType, -4, "", "ppz");
		OPCODE_MD(0x87, "getRandomNumber", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x88, "getRandomNumberRange", kSpecialCallInstType, -1, "", "rpp");
		OPCODE_MD(0x8A, "getActorMoving", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x8B, "isScriptRunning", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x8C, "getActorRoom", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x8D, "getObjectX", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x8E, "getObjectY", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x8F, "getObjectDir", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x90, "getActorWalkBox", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x91, "getActorCostume", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x92, "findInventory", kSpecialCallInstType, -1, "", "rpp");
		OPCODE_MD(0x93, "getInventoryCount", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x94, "getVerbFromXY", kSpecialCallInstType, -1, "", "rpp");
		OPCODE_BASE(0x95)
			OPCODE_BODY("beginOverride", kSpecialCallInstType, 0, "", "");
			// FIXME/TODO: beginOverride skips the following jump - that jump is instead to a "finally" handler
			// To simulate this, we simply skip the jump instruction, so the sequential order appears the same.
			// Semantically, it would probably be more correct to model this as a conditional jump,
			// but since precious little time remains of GSoC and that might not play very nice with the rest of the code,
			// this is the simplest way to handle the issue right now. Eventually, it should probably be fixed more properly.
			_f.seek(3, SEEK_CUR);
			_address += 3;
			OPCODE_END;
		OPCODE(0x96, "endOverride", kSpecialCallInstType, 0, "");
		OPCODE_MD(0x97, "setObjectName", kSpecialCallInstType, -1, "c", "ps");
		OPCODE_MD(0x98, "isSoundRunning", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0x99, "setBoxFlags", kSpecialCallInstType, 0x1100, "", "pl"); // Variable stack arguments
		OPCODE(0x9A, "createBoxMatrix", kSpecialCallInstType, 0, "");
		START_SUBOPCODE_WITH_PREFIX(0x9B, "resourceRoutines");
			OPCODE_MD(0x64, "loadScript", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x65, "loadSound", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x66, "loadCostume", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x67, "loadRoom", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x68, "nukeScript", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x69, "nukeSound", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x6A, "nukeCostume", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x6B, "nukeRoom", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x6C, "lockScript", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x6D, "lockSound", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x6E, "lockCostume", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x6F, "lockRoom", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x70, "unlockScript", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x71, "unlockSound", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x72, "unlockCostume", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x73, "unlockRoom", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x75, "loadCharset", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x76, "nukeCharset", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x77, "loadFlObject", kSpecialCallInstType, -2, "", "pp");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9C, "roomOps");
			OPCODE_MD(0xAC, "roomScroll", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0xAE, "setScreen", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0xAF, "setPalColor", kSpecialCallInstType, -4, "", "pppp");
			OPCODE(0xB0, "shakeOn", kSpecialCallInstType, 0, "");
			OPCODE(0xB1, "shakeOff", kSpecialCallInstType, 0, "");
			OPCODE_MD(0xB3, "darkenPalette", kSpecialCallInstType, -3, "", "ppp");
			OPCODE_MD(0xB4, "saveLoadRoom", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0xB5, "screenEffect", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0xB6, "darkenPaletteRGB", kSpecialCallInstType, -5, "", "ppppp");
			OPCODE_MD(0xB7, "setupShadowPalette", kSpecialCallInstType, -5, "", "ppppp");
			OPCODE_MD(0xBA, "palManipulate", kSpecialCallInstType, -4, "", "pppp");
			OPCODE_MD(0xBB, "colorCycleDelay", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0xD5, "setPalette", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0xDC, "copyPalColor", kSpecialCallInstType, -2, "", "pp");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9D, "actorOps");
			OPCODE_MD(0x4C, "setCostume", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x4D, "setWalkSpeed", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x4E, "setSound", kSpecialCallInstType, 0x1000, "", "l"); // Variable stack arguments
			OPCODE_MD(0x4F, "setWalkFrame", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x50, "setTalkFrame", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x51, "setStandFrame", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x52, "82?", kSpecialCallInstType, -3, "", "ppp");
			OPCODE(0x53, "init", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x54, "setElevation", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x55, "setDefAnim", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x56, "setPalette", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x57, "setTalkColor", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x58, "setName", kSpecialCallInstType, 0, "c", "s");
			OPCODE_MD(0x59, "setInitFrame", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x5B, "setWidth", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x5C, "setScale", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x5D, "setNeverZClip", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x5E, "setAlwaysZClip", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x5F, "setIgnoreBoxes", kSpecialCallInstType, 0, "");
			OPCODE(0x60, "setFollowBoxes", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x61, "setAnimSpeed", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x62, "setShadowMode", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x63, "setTalkPos", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0xC5, "setCurActor", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0xC6, "setAnimVar", kSpecialCallInstType, -2, "", "pp");
			OPCODE(0xD7, "setIgnoreTurnsOn", kSpecialCallInstType, 0, "");
			OPCODE(0xD8, "setIgnoreTurnsOff", kSpecialCallInstType, 0, "");
			OPCODE(0xD9, "initLittle", kSpecialCallInstType, 0, "");
			OPCODE_MD(0xE1, "setAlwaysZClip?", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0xE3, "setLayer", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0xE4, "setWalkScript", kSpecialCallInstType, -1, "", "p");
			OPCODE(0xE5, "setStanding", kSpecialCallInstType, 0, "");
			OPCODE_MD(0xE6, "setDirection", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0xE7, "turnToDirection", kSpecialCallInstType, -1, "", "p");
			OPCODE(0xE9, "freeze", kSpecialCallInstType, 0, "");
			OPCODE(0xEA, "unfreeze", kSpecialCallInstType, 0, "");
			OPCODE_MD(0xEB, "setTalkScript", kSpecialCallInstType, -1, "", "p");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9E, "verbOps");
			OPCODE_MD(0x7C, "loadImg", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x7D, "loadString", kSpecialCallInstType, 0, "c", "s");
			OPCODE_MD(0x7E, "setColor", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x7F, "setHiColor", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x80, "setXY", kSpecialCallInstType, -2, "", "pp");
			OPCODE(0x81, "setOn", kSpecialCallInstType, 0, "");
			OPCODE(0x82, "setOff", kSpecialCallInstType, 0, "");
			OPCODE(0x83, "kill", kSpecialCallInstType, 0, "");
			OPCODE(0x84, "init", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x85, "setDimColor", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x86, "setDimmed", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x87, "setKey", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x88, "setCenter", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x89, "setToString", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x8B, "setToObject", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x8C, "setBkColor", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0xC4, "setCurVerb", kSpecialCallInstType, -1, "", "p");
			OPCODE(0xFF, "redraw", kSpecialCallInstType, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0x9F, "getActorFromXY", kSpecialCallInstType, -1, "", "rpp");
		OPCODE_MD(0xA0, "findObject", kSpecialCallInstType, -1, "", "rpp");
		OPCODE_MD(0xA1, "pseudoRoom", kSpecialCallInstType, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0xA2, "getActorElevation", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0xA3, "getVerbEntrypoint", kSpecialCallInstType, -1, "", "rpp");
		START_SUBOPCODE_WITH_PREFIX(0xA4, "arrayOps");
			OPCODE_MD(0xCD, "assignString", kSpecialCallInstType, -1, "wc", "");
			OPCODE_MD(0xD0, "assignIntList", kSpecialCallInstType, 0x1100, "w", ""); // Variable stack arguments
			OPCODE_MD(0xD4, "assign2DimList", kSpecialCallInstType, 0x1100, "w", ""); // Variable stack arguments
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xA5, "saveRestoreVerbs");
			OPCODE_MD(0x8D, "saveVerbs", kSpecialCallInstType, -3, "", "ppp");
			OPCODE_MD(0x8E, "restoreVerbs", kSpecialCallInstType, -3, "", "ppp");
			OPCODE_MD(0x8F, "deleteVerbs", kSpecialCallInstType, -3, "", "ppp");
		END_SUBOPCODE;
		OPCODE_MD(0xA6, "drawBox", kSpecialCallInstType, -5, "", "ppppp");
		OPCODE(0xA7, "pop", kStackInstType, -1, "");
		OPCODE_MD(0xA8, "getActorWidth", kSpecialCallInstType, 0, "", "rp");
		START_SUBOPCODE(0xA9); // wait
			OPCODE_MD(0xA8, "waitForActor", kSpecialCallInstType, -1, "s", "pj");
			OPCODE(0xA9, "waitForMessage", kSpecialCallInstType, 0, "");
			OPCODE(0xAA, "waitForCamera", kSpecialCallInstType, 0, "");
			OPCODE(0xAB, "waitForSentence", kSpecialCallInstType, 0, "");
			OPCODE_MD(0xE2, "waitUntilActorDrawn", kSpecialCallInstType, -1, "s", "pj");
			OPCODE_MD(0xE8, "waitUntilActorTurned", kSpecialCallInstType, -1, "s", "pj");
		END_SUBOPCODE;
		OPCODE_MD(0xAA, "getActorScaleX", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0xAB, "getActorAnimCounter", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0xAC, "soundKludge", kSpecialCallInstType, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xAD, "isAnyOf", kSpecialCallInstType, 0x1011, "", "rlp"); // Variable stack arguments
		START_SUBOPCODE_WITH_PREFIX(0xAE, "systemOps");
			OPCODE(0x9E, "restartGame", kSpecialCallInstType, 0, "");
			OPCODE(0x9F, "pauseGame", kSpecialCallInstType, 0, "");
			OPCODE(0xA0, "shutDown", kSpecialCallInstType, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0xAF, "isActorInBox", kSpecialCallInstType, -1, "", "rpp");
		OPCODE_MD(0xB0, "delay", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0xB1, "delaySeconds", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0xB2, "delayMinutes", kSpecialCallInstType, -1, "", "p");
		OPCODE(0xB3, "stopSentence", kSpecialCallInstType, 0, "");
		START_SUBOPCODE_WITH_PREFIX(0xB4, "printLine");
			OPCODE_MD(0x41, "XY", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x45, "center", kSpecialCallInstType, 0, "");
			OPCODE(0x47, "left", kSpecialCallInstType, 0, "");
			OPCODE(0x48, "overhead", kSpecialCallInstType, 0, "");
			OPCODE(0x4A, "mumble", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecialCallInstType, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecialCallInstType, 0, "");
			OPCODE(0xFF, "end", kSpecialCallInstType, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB5, "printText");
			OPCODE_MD(0x41, "XY", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x45, "center", kSpecialCallInstType, 0, "");
			OPCODE(0x47, "left", kSpecialCallInstType, 0, "");
			OPCODE(0x48, "overhead", kSpecialCallInstType, 0, "");
			OPCODE(0x4A, "mumble", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecialCallInstType, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecialCallInstType, 0, "");
			OPCODE(0xFF, "end", kSpecialCallInstType, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB6, "printDebug");
			OPCODE_MD(0x41, "XY", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x45, "center", kSpecialCallInstType, 0, "");
			OPCODE(0x47, "left", kSpecialCallInstType, 0, "");
			OPCODE(0x48, "overhead", kSpecialCallInstType, 0, "");
			OPCODE(0x4A, "mumble", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecialCallInstType, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecialCallInstType, 0, "");
			OPCODE(0xFF, "end", kSpecialCallInstType, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB7, "printSystem");
			OPCODE_MD(0x41, "XY", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x45, "center", kSpecialCallInstType, 0, "");
			OPCODE(0x47, "left", kSpecialCallInstType, 0, "");
			OPCODE(0x48, "overhead", kSpecialCallInstType, 0, "");
			OPCODE(0x4A, "mumble", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecialCallInstType, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecialCallInstType, 0, "");
			OPCODE(0xFF, "end", kSpecialCallInstType, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB8, "printActor");
			OPCODE_MD(0x41, "XY", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x45, "center", kSpecialCallInstType, 0, "");
			OPCODE(0x47, "left", kSpecialCallInstType, 0, "");
			OPCODE(0x48, "overhead", kSpecialCallInstType, 0, "");
			OPCODE(0x4A, "mumble", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecialCallInstType, 0, "c", "s");
			OPCODE_MD(0xFE, "begin", kSpecialCallInstType, -1, "", "p");
			OPCODE(0xFF, "end", kSpecialCallInstType, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB9, "printEgo");
			OPCODE_MD(0x41, "XY", kSpecialCallInstType, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecialCallInstType, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecialCallInstType, -1, "", "p");
			OPCODE(0x45, "center", kSpecialCallInstType, 0, "");
			OPCODE(0x47, "left", kSpecialCallInstType, 0, "");
			OPCODE(0x48, "overhead", kSpecialCallInstType, 0, "");
			OPCODE(0x4A, "mumble", kSpecialCallInstType, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecialCallInstType, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecialCallInstType, 0, "");
			OPCODE(0xFF, "end", kSpecialCallInstType, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0xBA, "talkActor", kSpecialCallInstType, -1, "c", "ps");
		OPCODE_MD(0xBB, "talkEgo", kSpecialCallInstType, 0, "c", "s");
		START_SUBOPCODE(0xBC); // dimArray
			OPCODE_MD(0xC7, "dimArrayInt", kSpecialCallInstType, -1, "w", "pv");
			OPCODE_MD(0xC8, "dimArrayBit", kSpecialCallInstType, -1, "w", "pv");
			OPCODE_MD(0xC9, "dimArrayNibble", kSpecialCallInstType, -1, "w", "pv");
			OPCODE_MD(0xCA, "dimArrayByte", kSpecialCallInstType, -1, "w", "pv");
			OPCODE_MD(0xCB, "dimArrayString", kSpecialCallInstType, -1, "w", "pv");
			OPCODE_MD(0xCC, "dimArray_nukeArray", kSpecialCallInstType, 0, "w", "v");
		END_SUBOPCODE;
		OPCODE_MD(0xBE, "startObjectQuick", kSpecialCallInstType, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0xBF, "startScriptQuick2", kSpecialCallInstType, 0x1010, "", "lp"); // Variable stack arguments
		START_SUBOPCODE(0xC0); // dim2DimArray
			OPCODE_MD(0xC7, "dim2DimArrayInt", kSpecialCallInstType, -2, "w", "ppv");
			OPCODE_MD(0xC8, "dim2DimArrayBit", kSpecialCallInstType, -2, "w", "ppv");
			OPCODE_MD(0xC9, "dim2DimArrayNibble", kSpecialCallInstType, -2, "w", "ppv");
			OPCODE_MD(0xCA, "dim2DimArrayByte", kSpecialCallInstType, -2, "w", "ppv");
			OPCODE_MD(0xCB, "dim2DimArrayString", kSpecialCallInstType, -2, "w", "ppv");
		END_SUBOPCODE;
		OPCODE_MD(0xC4, "abs", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0xC5, "getDistObjObj", kSpecialCallInstType, -1, "", "rpp");
		OPCODE_MD(0xC6, "getDistObjPt", kSpecialCallInstType, -2, "", "rppp");
		OPCODE_MD(0xC7, "getDistPtPt", kSpecialCallInstType, -3, "", "rpppp");
		OPCODE_MD(0xC8, "kernelGetFunctions", kSpecialCallInstType, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xC9, "kernelSetFunctions", kSpecialCallInstType, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xCA, "delayFrames", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0xCB, "pickOneOf", kSpecialCallInstType, 0x1011, "", "rlp"); // Variable stack arguments
		OPCODE_MD(0xCC, "pickOneOfDefault", kSpecialCallInstType, 0x111, "", "rplp"); // Variable stack arguments
		OPCODE_MD(0xCD, "stampObject", kSpecialCallInstType, -4, "", "pppp");
		OPCODE(0xD0, "getDateTime", kSpecialCallInstType, 0, "");
		OPCODE(0xD1, "stopTalking", kSpecialCallInstType, 0, "");
		OPCODE_MD(0xD2, "getAnimateVariable", kSpecialCallInstType, -1, "", "rpp");
		OPCODE_MD(0xD4, "shuffle", kSpecialCallInstType, -2, "w", "vpp");
		OPCODE_MD(0xD5, "jumpToScript", kSpecialCallInstType, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0xD6, "band", kBinaryOpInstType, -1, "", "&");
		OPCODE_MD(0xD7, "bor", kBinaryOpInstType, -1, "", "|");
		OPCODE_MD(0xD8, "isRoomScriptRunning", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0xDD, "findAllObjects", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0xE1, "getPixel", kSpecialCallInstType, -1, "", "rpp");
		OPCODE_MD(0xE3, "pickVarRandom", kSpecialCallInstType, 0x1001, "w", "rlw"); // Variable stack arguments
		OPCODE_MD(0xE4, "setBoxSet", kSpecialCallInstType, -1, "", "p");
		OPCODE_MD(0xEC, "getActorLayer", kSpecialCallInstType, 0, "", "rp");
		OPCODE_MD(0xED, "getObjectNewDir", kSpecialCallInstType, 0, "", "rp");
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
		if ((*it2)->_type == kLoadInstType)
			--popBefore;
	(*it)->_stackChange -= (*it2)->_params[0].getSigned() + 1;
}

void Scumm::v6::Scummv6Disassembler::readParameter(Parameter *p, char type) {
	switch (type) {
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
		p->_type = kStringParamType;
		p->_value = s.str();
		}
		break;
	default: // Defer handling to parent implementation
		SimpleDisassembler::readParameter(p, type);
		break;
	}
}
