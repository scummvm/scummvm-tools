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

Scumm::v6::Scummv6Disassembler::Scummv6Disassembler(std::vector<Instruction> &insts) : SimpleDisassembler(insts) {
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
		OPCODE(0x00, "pushByte", kLoad, 1, "B");
		OPCODE(0x01, "pushWord", kLoad, 1, "s");
		OPCODE(0x02, "pushByteVar", kLoad, 1, "B");
		OPCODE(0x03, "pushWordVar", kLoad, 1, "w");
		OPCODE(0x06, "byteArrayRead", kLoad, 0, "B");
		OPCODE(0x07, "wordArrayRead", kLoad, 0, "w");
		OPCODE(0x0A, "byteArrayIndexedRead", kLoad, -1, "B");
		OPCODE(0x0B, "wordArrayIndexedRead", kLoad, -1, "w");
		OPCODE(0x0C, "dup", kDup, 1, "");
		OPCODE_MD(0x0D, "not", kUnaryOp, 0, "", "!");
		OPCODE_MD(0x0E, "eq", kBinaryOp, -1, "", "==");
		OPCODE_MD(0x0F, "neq", kBinaryOp, -1, "", "!=");
		OPCODE_MD(0x10, "gt", kBinaryOp, -1, "", ">");
		OPCODE_MD(0x11, "lt", kBinaryOp, -1, "", "<");
		OPCODE_MD(0x12, "le", kBinaryOp, -1, "", "<=");
		OPCODE_MD(0x13, "ge", kBinaryOp, -1, "", ">=");
		OPCODE_MD(0x14, "add", kBinaryOp, -1, "", "+");
		OPCODE_MD(0x15, "sub", kBinaryOp, -1, "", "-");
		OPCODE_MD(0x16, "mul", kBinaryOp, -1, "", "*");
		OPCODE_MD(0x17, "div", kBinaryOp, -1, "", "/");
		OPCODE_MD(0x18, "land", kBinaryOp, -1, "", "&&");
		OPCODE_MD(0x19, "lor", kBinaryOp, -1, "", "||");
		OPCODE(0x1A, "pop", kStack, -1, "");
		OPCODE(0x42, "writeByteVar", kStore, -1, "B");
		OPCODE(0x43, "writeWordVar", kStore, -1, "w");
		OPCODE(0x46, "byteArrayWrite", kStore, -2, "B");
		OPCODE(0x47, "wordArrayWrite", kStore, -2, "w");
		OPCODE(0x4A, "byteArrayIndexedWrite", kStore, -3, "B");
		OPCODE(0x4B, "wordArrayIndexedWrite", kStore, -3, "w");
		OPCODE_MD(0x4E, "byteVarInc", kUnaryOp, 0, "B", "\xC0++");
		OPCODE_MD(0x4F, "wordVarInc", kUnaryOp, 0, "w", "\xC0++");
		OPCODE_MD(0x52, "byteArrayInc", kUnaryOp, -1, "B", "\xC0++");
		OPCODE_MD(0x53, "wordArrayInc", kUnaryOp, -1, "w", "\xC0++");
		OPCODE_MD(0x56, "byteVarDec", kUnaryOp, 0, "B", "\xC0--");
		OPCODE_MD(0x57, "wordVarDec", kUnaryOp, 0, "w", "\xC0--");
		OPCODE_MD(0x5A, "byteArrayDec", kUnaryOp, -1, "B", "\xC0--");
		OPCODE_MD(0x5B, "wordArrayDec", kUnaryOp, -1, "w", "\xC0--");
		OPCODE(0x5C, "jumpTrue", kCondJumpRel, -1, "s");
		OPCODE(0x5D, "jumpFalse", kCondJumpRel, -1, "s");
		OPCODE_MD(0x5E, "startScript", kSpecial, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0x5F, "startScriptQuick", kSpecial, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0x60, "startObject", kSpecial, 0x1030, "", "lppp"); // Variable stack arguments
		OPCODE_MD(0x61, "drawObject", kSpecial, -2, "", "pp");
		OPCODE_MD(0x62, "drawObjectAt", kSpecial, -3, "", "ppp");
		OPCODE_MD(0x63, "drawBlastObject", kSpecial, -5, "", "ppppp");
		OPCODE_MD(0x64, "setBlastObjectWindow", kSpecial, -4, "", "pppp");
		OPCODE(0x65, "stopObjectCodeA", kSpecial, 0, "");
		OPCODE(0x66, "stopObjectCodeB", kSpecial, 0, "");
		OPCODE(0x67, "endCutscene", kSpecial, 0, "");
		OPCODE_MD(0x68, "beginCutscene", kSpecial, 0x1000, "", "l"); // Variable stack arguments
		OPCODE(0x69, "stopMusic", kSpecial, 0, "");
		OPCODE_MD(0x6A, "freezeUnfreeze", kSpecial, -1, "", "p");
		START_SUBOPCODE_WITH_PREFIX(0x6B, "cursorCommand");
			OPCODE(0x90, "cursorOn", kSpecial, 0, "");
			OPCODE(0x91, "cursorOff", kSpecial, 0, "");
			OPCODE(0x92, "userputOn", kSpecial, 0, "");
			OPCODE(0x93, "userputOff", kSpecial, 0, "");
			OPCODE(0x94, "softCursorOn", kSpecial, 0, "");
			OPCODE(0x95, "softCursorOff", kSpecial, 0, "");
			OPCODE(0x96, "softUserputOn", kSpecial, 0, "");
			OPCODE(0x97, "softUserputOff", kSpecial, 0, "");
			OPCODE_MD(0x99, "setCursorImg", kSpecial, -2, "", "z");
			OPCODE_MD(0x9A, "setCursorHotspot", kSpecial, -2, "", "pp");
			OPCODE_MD(0x9C, "initCharset", kSpecial, -1, "", "p");
			OPCODE_MD(0x9D, "charsetColors", kSpecial, 0x1000, "", "l"); // Variable stack arguments
			OPCODE_MD(0xD6, "setCursorTransparency", kSpecial, -1, "", "p");
		END_SUBOPCODE;
		OPCODE(0x6C, "breakHere", kSpecial, 0, "");
		OPCODE_MD(0x6D, "ifClassOfIs", kSpecial, 0x1011, "", "rlp"); // Variable stack arguments
		OPCODE_MD(0x6E, "setClass", kSpecial, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0x6F, "getState", kSpecial, 0, "", "rp");
		OPCODE_MD(0x70, "setState", kSpecial, -2, "", "pp");
		OPCODE_MD(0x71, "setOwner", kSpecial, -2, "", "pp");
		OPCODE_MD(0x72, "getOwner", kSpecial, 0, "", "rp");
		OPCODE(0x73, "jump", kJumpRel, 0, "s");
		OPCODE_MD(0x74, "startSound", kSpecial, -1, "", "p");
		OPCODE_MD(0x75, "stopSound", kSpecial, -1, "", "p");
		OPCODE_MD(0x76, "startMusic", kSpecial, -1, "", "p");
		OPCODE_MD(0x77, "stopObjectScript", kSpecial, -1, "", "p");
		OPCODE_MD(0x78, "panCameraTo", kSpecial, -1, "", "p");
		OPCODE_MD(0x79, "actorFollowCamera", kSpecial, -1, "", "p");
		OPCODE_MD(0x7A, "setCameraAt", kSpecial, -1, "", "p");
		OPCODE_MD(0x7B, "loadRoom", kSpecial, -1, "", "p");
		OPCODE_MD(0x7C, "stopScript", kSpecial, -1, "", "p");
		OPCODE_MD(0x7D, "walkActorToObj", kSpecial, -3, "", "ppp");
		OPCODE_MD(0x7E, "walkActorTo", kSpecial, -3, "", "ppp");
		OPCODE_MD(0x7F, "putActorAtXY", kSpecial, -4, "", "pppp");
		OPCODE_MD(0x80, "putActorAtObject", kSpecial, -3, "", "zp");
		OPCODE_MD(0x81, "faceActor", kSpecial, -2, "", "pp");
		OPCODE_MD(0x82, "animateActor", kSpecial, -2, "", "pp");
		OPCODE_MD(0x83, "doSentence", kSpecial, -4, "", "pppp");
		OPCODE_MD(0x84, "pickupObject", kSpecial, -2, "", "z");
		OPCODE_MD(0x85, "loadRoomWithEgo", kSpecial, -4, "", "ppz");
		OPCODE_MD(0x87, "getRandomNumber", kSpecial, 0, "", "rp");
		OPCODE_MD(0x88, "getRandomNumberRange", kSpecial, -1, "", "rpp");
		OPCODE_MD(0x8A, "getActorMoving", kSpecial, 0, "", "rp");
		OPCODE_MD(0x8B, "isScriptRunning", kSpecial, 0, "", "rp");
		OPCODE_MD(0x8C, "getActorRoom", kSpecial, 0, "", "rp");
		OPCODE_MD(0x8D, "getObjectX", kSpecial, 0, "", "rp");
		OPCODE_MD(0x8E, "getObjectY", kSpecial, 0, "", "rp");
		OPCODE_MD(0x8F, "getObjectDir", kSpecial, 0, "", "rp");
		OPCODE_MD(0x90, "getActorWalkBox", kSpecial, 0, "", "rp");
		OPCODE_MD(0x91, "getActorCostume", kSpecial, 0, "", "rp");
		OPCODE_MD(0x92, "findInventory", kSpecial, -1, "", "rpp");
		OPCODE_MD(0x93, "getInventoryCount", kSpecial, 0, "", "rp");
		OPCODE_MD(0x94, "getVerbFromXY", kSpecial, -1, "", "rpp");
		OPCODE_BASE(0x95)
			OPCODE_BODY("beginOverride", kSpecial, 0, "", "");
			// FIXME/TODO: beginOverride skips the following jump - that jump is instead to a "finally" handler
			// To simulate this, we simply skip the jump instruction, so the sequential order appears the same.
			// Semantically, it would probably be more correct to model this as a conditional jump,
			// but since precious little time remains of GSoC and that might not play very nice with the rest of the code,
			// this is the simplest way to handle the issue right now. Eventually, it should probably be fixed more properly.
			_f.seek(3, SEEK_CUR);
			_address += 3;
			OPCODE_END;
		OPCODE(0x96, "endOverride", kSpecial, 0, "");
		OPCODE_MD(0x97, "setObjectName", kSpecial, -1, "c", "ps");
		OPCODE_MD(0x98, "isSoundRunning", kSpecial, 0, "", "rp");
		OPCODE_MD(0x99, "setBoxFlags", kSpecial, 0x1100, "", "pl"); // Variable stack arguments
		OPCODE(0x9A, "createBoxMatrix", kSpecial, 0, "");
		START_SUBOPCODE_WITH_PREFIX(0x9B, "resourceRoutines");
			OPCODE_MD(0x64, "loadScript", kSpecial, -1, "", "p");
			OPCODE_MD(0x65, "loadSound", kSpecial, -1, "", "p");
			OPCODE_MD(0x66, "loadCostume", kSpecial, -1, "", "p");
			OPCODE_MD(0x67, "loadRoom", kSpecial, -1, "", "p");
			OPCODE_MD(0x68, "nukeScript", kSpecial, -1, "", "p");
			OPCODE_MD(0x69, "nukeSound", kSpecial, -1, "", "p");
			OPCODE_MD(0x6A, "nukeCostume", kSpecial, -1, "", "p");
			OPCODE_MD(0x6B, "nukeRoom", kSpecial, -1, "", "p");
			OPCODE_MD(0x6C, "lockScript", kSpecial, -1, "", "p");
			OPCODE_MD(0x6D, "lockSound", kSpecial, -1, "", "p");
			OPCODE_MD(0x6E, "lockCostume", kSpecial, -1, "", "p");
			OPCODE_MD(0x6F, "lockRoom", kSpecial, -1, "", "p");
			OPCODE_MD(0x70, "unlockScript", kSpecial, -1, "", "p");
			OPCODE_MD(0x71, "unlockSound", kSpecial, -1, "", "p");
			OPCODE_MD(0x72, "unlockCostume", kSpecial, -1, "", "p");
			OPCODE_MD(0x73, "unlockRoom", kSpecial, -1, "", "p");
			OPCODE_MD(0x75, "loadCharset", kSpecial, -1, "", "p");
			OPCODE_MD(0x76, "nukeCharset", kSpecial, -1, "", "p");
			OPCODE_MD(0x77, "loadFlObject", kSpecial, -2, "", "pp");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9C, "roomOps");
			OPCODE_MD(0xAC, "roomScroll", kSpecial, -2, "", "pp");
			OPCODE_MD(0xAE, "setScreen", kSpecial, -2, "", "pp");
			OPCODE_MD(0xAF, "setPalColor", kSpecial, -4, "", "pppp");
			OPCODE(0xB0, "shakeOn", kSpecial, 0, "");
			OPCODE(0xB1, "shakeOff", kSpecial, 0, "");
			OPCODE_MD(0xB3, "darkenPalette", kSpecial, -3, "", "ppp");
			OPCODE_MD(0xB4, "saveLoadRoom", kSpecial, -2, "", "pp");
			OPCODE_MD(0xB5, "screenEffect", kSpecial, -1, "", "p");
			OPCODE_MD(0xB6, "darkenPaletteRGB", kSpecial, -5, "", "ppppp");
			OPCODE_MD(0xB7, "setupShadowPalette", kSpecial, -5, "", "ppppp");
			OPCODE_MD(0xBA, "palManipulate", kSpecial, -4, "", "pppp");
			OPCODE_MD(0xBB, "colorCycleDelay", kSpecial, -2, "", "pp");
			OPCODE_MD(0xD5, "setPalette", kSpecial, -1, "", "p");
			OPCODE_MD(0xDC, "copyPalColor", kSpecial, -2, "", "pp");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9D, "actorOps");
			OPCODE_MD(0x4C, "setCostume", kSpecial, -1, "", "p");
			OPCODE_MD(0x4D, "setWalkSpeed", kSpecial, -2, "", "pp");
			OPCODE_MD(0x4E, "setSound", kSpecial, 0x1000, "", "l"); // Variable stack arguments
			OPCODE_MD(0x4F, "setWalkFrame", kSpecial, -1, "", "p");
			OPCODE_MD(0x50, "setTalkFrame", kSpecial, -2, "", "pp");
			OPCODE_MD(0x51, "setStandFrame", kSpecial, -1, "", "p");
			OPCODE_MD(0x52, "82?", kSpecial, -3, "", "ppp");
			OPCODE(0x53, "init", kSpecial, 0, "");
			OPCODE_MD(0x54, "setElevation", kSpecial, -1, "", "p");
			OPCODE(0x55, "setDefAnim", kSpecial, 0, "");
			OPCODE_MD(0x56, "setPalette", kSpecial, -2, "", "pp");
			OPCODE_MD(0x57, "setTalkColor", kSpecial, -1, "", "p");
			OPCODE_MD(0x58, "setName", kSpecial, 0, "c", "s");
			OPCODE_MD(0x59, "setInitFrame", kSpecial, -1, "", "p");
			OPCODE_MD(0x5B, "setWidth", kSpecial, -1, "", "p");
			OPCODE_MD(0x5C, "setScale", kSpecial, -1, "", "p");
			OPCODE(0x5D, "setNeverZClip", kSpecial, 0, "");
			OPCODE_MD(0x5E, "setAlwaysZClip", kSpecial, -1, "", "p");
			OPCODE(0x5F, "setIgnoreBoxes", kSpecial, 0, "");
			OPCODE(0x60, "setFollowBoxes", kSpecial, 0, "");
			OPCODE_MD(0x61, "setAnimSpeed", kSpecial, -1, "", "p");
			OPCODE_MD(0x62, "setShadowMode", kSpecial, -1, "", "p");
			OPCODE_MD(0x63, "setTalkPos", kSpecial, -2, "", "pp");
			OPCODE_MD(0xC5, "setCurActor", kSpecial, -1, "", "p");
			OPCODE_MD(0xC6, "setAnimVar", kSpecial, -2, "", "pp");
			OPCODE(0xD7, "setIgnoreTurnsOn", kSpecial, 0, "");
			OPCODE(0xD8, "setIgnoreTurnsOff", kSpecial, 0, "");
			OPCODE(0xD9, "initLittle", kSpecial, 0, "");
			OPCODE_MD(0xE1, "setAlwaysZClip?", kSpecial, -1, "", "p");
			OPCODE_MD(0xE3, "setLayer", kSpecial, -1, "", "p");
			OPCODE_MD(0xE4, "setWalkScript", kSpecial, -1, "", "p");
			OPCODE(0xE5, "setStanding", kSpecial, 0, "");
			OPCODE_MD(0xE6, "setDirection", kSpecial, -1, "", "p");
			OPCODE_MD(0xE7, "turnToDirection", kSpecial, -1, "", "p");
			OPCODE(0xE9, "freeze", kSpecial, 0, "");
			OPCODE(0xEA, "unfreeze", kSpecial, 0, "");
			OPCODE_MD(0xEB, "setTalkScript", kSpecial, -1, "", "p");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0x9E, "verbOps");
			OPCODE_MD(0x7C, "loadImg", kSpecial, -1, "", "p");
			OPCODE_MD(0x7D, "loadString", kSpecial, 0, "c", "s");
			OPCODE_MD(0x7E, "setColor", kSpecial, -1, "", "p");
			OPCODE_MD(0x7F, "setHiColor", kSpecial, -1, "", "p");
			OPCODE_MD(0x80, "setXY", kSpecial, -2, "", "pp");
			OPCODE(0x81, "setOn", kSpecial, 0, "");
			OPCODE(0x82, "setOff", kSpecial, 0, "");
			OPCODE(0x83, "kill", kSpecial, 0, "");
			OPCODE(0x84, "init", kSpecial, 0, "");
			OPCODE_MD(0x85, "setDimColor", kSpecial, -1, "", "p");
			OPCODE(0x86, "setDimmed", kSpecial, 0, "");
			OPCODE_MD(0x87, "setKey", kSpecial, -1, "", "p");
			OPCODE(0x88, "setCenter", kSpecial, 0, "");
			OPCODE_MD(0x89, "setToString", kSpecial, -1, "", "p");
			OPCODE_MD(0x8B, "setToObject", kSpecial, -2, "", "pp");
			OPCODE_MD(0x8C, "setBkColor", kSpecial, -1, "", "p");
			OPCODE_MD(0xC4, "setCurVerb", kSpecial, -1, "", "p");
			OPCODE(0xFF, "redraw", kSpecial, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0x9F, "getActorFromXY", kSpecial, -1, "", "rpp");
		OPCODE_MD(0xA0, "findObject", kSpecial, -1, "", "rpp");
		OPCODE_MD(0xA1, "pseudoRoom", kSpecial, 0x1010, "", "lp"); // Variable stack arguments
		OPCODE_MD(0xA2, "getActorElevation", kSpecial, 0, "", "rp");
		OPCODE_MD(0xA3, "getVerbEntrypoint", kSpecial, -1, "", "rpp");
		START_SUBOPCODE_WITH_PREFIX(0xA4, "arrayOps");
			OPCODE_MD(0xCD, "assignString", kSpecial, -1, "wc", "\xC0");
			OPCODE_MD(0xD0, "assignIntList", kSpecial, 0x1100, "w", "\xC0"); // Variable stack arguments
			OPCODE_MD(0xD4, "assign2DimList", kSpecial, 0x1100, "w", "\xC0"); // Variable stack arguments
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xA5, "saveRestoreVerbs");
			OPCODE_MD(0x8D, "saveVerbs", kSpecial, -3, "", "ppp");
			OPCODE_MD(0x8E, "restoreVerbs", kSpecial, -3, "", "ppp");
			OPCODE_MD(0x8F, "deleteVerbs", kSpecial, -3, "", "ppp");
		END_SUBOPCODE;
		OPCODE_MD(0xA6, "drawBox", kSpecial, -5, "", "ppppp");
		OPCODE(0xA7, "pop", kStack, -1, "");
		OPCODE_MD(0xA8, "getActorWidth", kSpecial, 0, "", "rp");
		START_SUBOPCODE(0xA9); // wait
			OPCODE_MD(0xA8, "waitForActor", kSpecial, -1, "s", "pj");
			OPCODE(0xA9, "waitForMessage", kSpecial, 0, "");
			OPCODE(0xAA, "waitForCamera", kSpecial, 0, "");
			OPCODE(0xAB, "waitForSentence", kSpecial, 0, "");
			OPCODE_MD(0xE2, "waitUntilActorDrawn", kSpecial, -1, "s", "pj");
			OPCODE_MD(0xE8, "waitUntilActorTurned", kSpecial, -1, "s", "pj");
		END_SUBOPCODE;
		OPCODE_MD(0xAA, "getActorScaleX", kSpecial, 0, "", "rp");
		OPCODE_MD(0xAB, "getActorAnimCounter", kSpecial, 0, "", "rp");
		OPCODE_MD(0xAC, "soundKludge", kSpecial, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xAD, "isAnyOf", kSpecial, 0x1011, "", "rlp"); // Variable stack arguments
		START_SUBOPCODE_WITH_PREFIX(0xAE, "systemOps");
			OPCODE(0x9E, "restartGame", kSpecial, 0, "");
			OPCODE(0x9F, "pauseGame", kSpecial, 0, "");
			OPCODE(0xA0, "shutDown", kSpecial, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0xAF, "isActorInBox", kSpecial, -1, "", "rpp");
		OPCODE_MD(0xB0, "delay", kSpecial, -1, "", "p");
		OPCODE_MD(0xB1, "delaySeconds", kSpecial, -1, "", "p");
		OPCODE_MD(0xB2, "delayMinutes", kSpecial, -1, "", "p");
		OPCODE(0xB3, "stopSentence", kSpecial, 0, "");
		START_SUBOPCODE_WITH_PREFIX(0xB4, "printLine");
			OPCODE_MD(0x41, "XY", kSpecial, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecial, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecial, -1, "", "p");
			OPCODE(0x45, "center", kSpecial, 0, "");
			OPCODE(0x47, "left", kSpecial, 0, "");
			OPCODE(0x48, "overhead", kSpecial, 0, "");
			OPCODE(0x4A, "mumble", kSpecial, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecial, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecial, 0, "");
			OPCODE(0xFF, "end", kSpecial, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB5, "printText");
			OPCODE_MD(0x41, "XY", kSpecial, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecial, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecial, -1, "", "p");
			OPCODE(0x45, "center", kSpecial, 0, "");
			OPCODE(0x47, "left", kSpecial, 0, "");
			OPCODE(0x48, "overhead", kSpecial, 0, "");
			OPCODE(0x4A, "mumble", kSpecial, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecial, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecial, 0, "");
			OPCODE(0xFF, "end", kSpecial, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB6, "printDebug");
			OPCODE_MD(0x41, "XY", kSpecial, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecial, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecial, -1, "", "p");
			OPCODE(0x45, "center", kSpecial, 0, "");
			OPCODE(0x47, "left", kSpecial, 0, "");
			OPCODE(0x48, "overhead", kSpecial, 0, "");
			OPCODE(0x4A, "mumble", kSpecial, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecial, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecial, 0, "");
			OPCODE(0xFF, "end", kSpecial, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB7, "printSystem");
			OPCODE_MD(0x41, "XY", kSpecial, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecial, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecial, -1, "", "p");
			OPCODE(0x45, "center", kSpecial, 0, "");
			OPCODE(0x47, "left", kSpecial, 0, "");
			OPCODE(0x48, "overhead", kSpecial, 0, "");
			OPCODE(0x4A, "mumble", kSpecial, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecial, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecial, 0, "");
			OPCODE(0xFF, "end", kSpecial, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB8, "printActor");
			OPCODE_MD(0x41, "XY", kSpecial, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecial, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecial, -1, "", "p");
			OPCODE(0x45, "center", kSpecial, 0, "");
			OPCODE(0x47, "left", kSpecial, 0, "");
			OPCODE(0x48, "overhead", kSpecial, 0, "");
			OPCODE(0x4A, "mumble", kSpecial, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecial, 0, "c", "s");
			OPCODE_MD(0xFE, "begin", kSpecial, -1, "", "p");
			OPCODE(0xFF, "end", kSpecial, 0, "");
		END_SUBOPCODE;
		START_SUBOPCODE_WITH_PREFIX(0xB9, "printEgo");
			OPCODE_MD(0x41, "XY", kSpecial, -2, "", "pp");
			OPCODE_MD(0x42, "color", kSpecial, -1, "", "p");
			OPCODE_MD(0x43, "right", kSpecial, -1, "", "p");
			OPCODE(0x45, "center", kSpecial, 0, "");
			OPCODE(0x47, "left", kSpecial, 0, "");
			OPCODE(0x48, "overhead", kSpecial, 0, "");
			OPCODE(0x4A, "mumble", kSpecial, 0, "");
			OPCODE_MD(0x4B, "msg", kSpecial, 0, "c", "s");
			OPCODE(0xFE, "begin", kSpecial, 0, "");
			OPCODE(0xFF, "end", kSpecial, 0, "");
		END_SUBOPCODE;
		OPCODE_MD(0xBA, "talkActor", kSpecial, -1, "c", "ps");
		OPCODE_MD(0xBB, "talkEgo", kSpecial, 0, "c", "s");
		START_SUBOPCODE(0xBC); // dimArray
			OPCODE_MD(0xC7, "dimArrayInt", kSpecial, -1, "w", "pv");
			OPCODE_MD(0xC8, "dimArrayBit", kSpecial, -1, "w", "pv");
			OPCODE_MD(0xC9, "dimArrayNibble", kSpecial, -1, "w", "pv");
			OPCODE_MD(0xCA, "dimArrayByte", kSpecial, -1, "w", "pv");
			OPCODE_MD(0xCB, "dimArrayString", kSpecial, -1, "w", "pv");
			OPCODE_MD(0xCC, "dimArray_nukeArray", kSpecial, 0, "w", "v");
		END_SUBOPCODE;
		OPCODE_MD(0xBE, "startObjectQuick", kSpecial, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0xBF, "startScriptQuick2", kSpecial, 0x1010, "", "lp"); // Variable stack arguments
		START_SUBOPCODE(0xC0); // dim2DimArray
			OPCODE_MD(0xC7, "dim2DimArrayInt", kSpecial, -2, "w", "ppv");
			OPCODE_MD(0xC8, "dim2DimArrayBit", kSpecial, -2, "w", "ppv");
			OPCODE_MD(0xC9, "dim2DimArrayNibble", kSpecial, -2, "w", "ppv");
			OPCODE_MD(0xCA, "dim2DimArrayByte", kSpecial, -2, "w", "ppv");
			OPCODE_MD(0xCB, "dim2DimArrayString", kSpecial, -2, "w", "ppv");
		END_SUBOPCODE;
		OPCODE_MD(0xC4, "abs", kSpecial, 0, "", "rp");
		OPCODE_MD(0xC5, "getDistObjObj", kSpecial, -1, "", "rpp");
		OPCODE_MD(0xC6, "getDistObjPt", kSpecial, -2, "", "rppp");
		OPCODE_MD(0xC7, "getDistPtPt", kSpecial, -3, "", "rpppp");
		OPCODE_MD(0xC8, "kernelGetFunctions", kSpecial, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xC9, "kernelSetFunctions", kSpecial, 0x1000, "", "l"); // Variable stack arguments
		OPCODE_MD(0xCA, "delayFrames", kSpecial, -1, "", "p");
		OPCODE_MD(0xCB, "pickOneOf", kSpecial, 0x1011, "", "rlp"); // Variable stack arguments
		OPCODE_MD(0xCC, "pickOneOfDefault", kSpecial, 0x111, "", "rplp"); // Variable stack arguments
		OPCODE_MD(0xCD, "stampObject", kSpecial, -4, "", "pppp");
		OPCODE(0xD0, "getDateTime", kSpecial, 0, "");
		OPCODE(0xD1, "stopTalking", kSpecial, 0, "");
		OPCODE_MD(0xD2, "getAnimateVariable", kSpecial, -1, "", "rpp");
		OPCODE_MD(0xD4, "shuffle", kSpecial, -2, "w", "vpp");
		OPCODE_MD(0xD5, "jumpToScript", kSpecial, 0x1020, "", "lpp"); // Variable stack arguments
		OPCODE_MD(0xD6, "band", kBinaryOp, -1, "", "&");
		OPCODE_MD(0xD7, "bor", kBinaryOp, -1, "", "|");
		OPCODE_MD(0xD8, "isRoomScriptRunning", kSpecial, 0, "", "rp");
		OPCODE_MD(0xDD, "findAllObjects", kSpecial, 0, "", "rp");
		OPCODE_MD(0xE1, "getPixel", kSpecial, -1, "", "rpp");
		OPCODE_MD(0xE3, "pickVarRandom", kSpecial, 0x1001, "w", "rlw"); // Variable stack arguments
		OPCODE_MD(0xE4, "setBoxSet", kSpecial, -1, "", "p");
		OPCODE_MD(0xEC, "getActorLayer", kSpecial, 0, "", "rp");
		OPCODE_MD(0xED, "getObjectNewDir", kSpecial, 0, "", "rp");
	END_OPCODES;

	InstIterator it;
	for (it = _insts.begin(); it != _insts.end(); ++it)
		if (it->_stackChange >= 0x1000)
			fixStackEffect(it, (it->_stackChange >> 8) & 0xF, (it->_stackChange >> 4) & 0xF, it->_stackChange & 0xF);
}

void Scumm::v6::Scummv6Disassembler::fixStackEffect(InstIterator &it, int popBefore, int popAfter, int pushTotal) {
	it->_stackChange = -popBefore - popAfter + pushTotal;
	InstIterator it2 = it;
	for (--it2; popBefore != 0; --it2)
		if (it2->_type == kLoad)
			--popBefore;
	it->_stackChange -= it2->_params[0].getSigned() + 1;
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
		p->_type = kString;
		p->_value = s.str();
		}
		break;
	default: // Defer handling to parent implementation
		SimpleDisassembler::readParameter(p, type);
		break;
	}
}
