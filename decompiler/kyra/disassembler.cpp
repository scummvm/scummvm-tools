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

#include "disassembler.h"
#include "engine.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <boost/format.hpp>

struct FunctionData {
  std::string _name;
	std::string _metadata;
};

static FunctionData kyra2FuncDesc[] = {
	// 0x00
	{ "setCharacterFacingRefresh", "" },
	{ "setCharacterPos", "" },
	{ "defineObject", "" },
	{ "refreshCharacter", "" },
	// 0x04
	{ "getCharacterX", "" },
	{ "getCharacterY", "" },
	{ "getCharacterFacing", "" },
	{ "getCharacterScene", "" },
	// 0x08
	{ "setSceneComment", "" },
	{ "unk09", "" },
	{ "unk0A", "" },
	{ "setCharacterAnimFrame", "" },
	// 0x0c
	{ "setCharacterFacingOverwrite", "" },
	{ "trySceneChange", "" },
	{ "moveCharacter", "" },
	{ "customCharacterChat", "" },
	// 0x10
	{ "soundFadeOut", "" },
	{ "showChapterMessage", "" },
	{ "restoreTalkTextMessageBkgd", "" },
	{ "unk13", "" },
	// 0x14
	{ "wsaClose", "" },
	{ "backUpScreen", "" },
	{ "restoreScreen", "" },
	{ "displayWsaFrame", "" },
	// 0x18
	{ "displayWsaSequentialFramesLooping", "" },
	{ "wsaOpen", "" },
	{ "displayWsaSequentialFrames", "" },
	{ "displayWsaSequence", "" },
	// 0x1c
	{ "addItemToInventory", "" },
	{ "drawShape", "" },
	{ "addItemToCurScene", "" },
	{ "limitMouseRange", "" },
	// 0x20
	{ "checkForItem", "" },
	{ "loadSoundFile", "" },
	{ "removeSlotFromInventory", "" },
	{ "defineItem", "" },
	// 0x24
	{ "removeItemFromInventory", "" },
	{ "countItemInInventory", "" },
	{ "countItemsInScene", "" },
	{ "queryGameFlag", "" },
	// 0x28
	{ "resetGameFlag", "" },
	{ "setGameFlag", "" },
	{ "setHandItem", "" },
	{ "removeHandItem", "" },
	// 0x2c
	{ "getMouseState", "" },
	{ "hideMouse", "" },
	{ "addSpecialExit", "" },
	{ "setMousePos", "" },
	// 0x30
	{ "showMouse", "" },
	{ "unk31", "" },
	{ "wipeDownMouseItem", "" },
	{ "getElapsedSecs", "" },
	// 0x34
	{ "getTimerDelay", "" },
	{ "playSoundEffect", "" },
	{ "delaySecs", "" },
	{ "delay", "" },
	// 0x38
	{ "dummy38", "" },
	{ "setTimerDelay", "" },
	{ "setScaleTableItem", "" },
	{ "setDrawLayerTableItem", "" },
	// 0x3c
	{ "setCharPalEntry", "" },
	{ "loadZShapes", "" },
	{ "drawSceneShape", "" },
	{ "drawSceneShapeOnPage", "" },
	// 0x40
	{ "disableAnimObject", "" },
	{ "enableAnimObject", "" },
	{ "dummy42", "" },
	{ "loadPalette384", "" },
	// 0x44
	{ "setPalette384", "" },
	{ "restoreBackBuffer", "" },
	{ "backUpInventoryGfx", "" },
	{ "disableSceneAnim", "" },
	// 0x48
	{ "enableSceneAnim", "" },
	{ "restoreInventoryGfx", "" },
	{ "setSceneAnimPos2", "" },
	{ "update", "" },
	// 0x4c
	{ "unk4c", "" },
	{ "fadeScenePal", "" },
	{ "dummy4E", "" },
	{ "dummy4F", "" },
	// 0x50
	{ "enterNewScene", "" },
	{ "switchScene", "" },
	{ "getShapeFlag1", "" },
	{ "setPathfinderFlag", "" },
	// 0x54
	{ "getSceneExitToFacing", "" },
	{ "setLayerFlag", "" },
	{ "setZanthiaPos", "" },
	{ "loadMusicTrack", "" },
	// 0x58
	{ "playWanderScoreViaMap", "" },
	{ "playSoundEffect", "" },
	{ "setSceneAnimPos", "" },
	{ "blockInWalkableRegion", "" },
	// 0x5c
	{ "blockOutWalkableRegion", "" },
	{ "unk5d", "" },
	{ "setCauldronState", "" },
	{ "showItemString", "" },
	// 0x60
	{ "getRand", "" },
	{ "isAnySoundPlaying", "" },
	{ "setDeathHandler", "" },
	{ "setDrawNoShapeFlag", "" },
	// 0x64
	{ "setRunFlag", "" },
	{ "showLetter", "" },
	{ "unk66", "" },
	{ "fillRect", "" },
	// 0x68
	{ "unk68", "" },
	{ "unk69", "" },
	{ "playFireflyScore", "" },
	{ "waitForConfirmationClick", "" },
	// 0x6c
	{ "encodeShape", "" },
	{ "defineRoomEntrance", "" },
	{ "runAnimationScript", "" },
	{ "setSpecialSceneScriptRunTime", "" },
	// 0x70
	{ "defineSceneAnim", "" },
	{ "updateSceneAnim", "" },
	{ "updateSceneAnim", "" },
	{ "addToSceneAnimPosAndUpdate", "" },
	// 0x74
	{ "useItemOnMainChar", "" },
	{ "startDialogue", "" },
	{ "randomSceneChat", "" },
	{ "setDlgIndex", "" },
	// 0x78
	{ "getDlgIndex", "" },
	{ "defineScene", "" },
	{ "addCauldronStateTableEntry", "" },
	{ "setCountDown", "" },
	// 0x7c
	{ "getCountDown", "" },
	{ "dummy7D", "" },
	{ "dummy7E", "" },
	{ "pressColorKey", "" },
	// 0x80
	{ "objectChat", "" },
	{ "changeChapter", "" },
	{ "getColorCodeFlag1", "" },
	{ "setColorCodeFlag1", "" },
	// 0x84
	{ "getColorCodeFlag2", "" },
	{ "setColorCodeFlag2", "" },
	{ "getColorCodeValue", "" },
	{ "setColorCodeValue", "" },
	// 0x88
	{ "countItemInstances", "" },
	{ "removeItemFromScene", "" },
	{ "initObject", "" },
	{ "npcChat", "" },
	// 0x8c
	{ "deinitObject", "" },
	{ "playTimSequence", "" },
	{ "makeBookOrCauldronAppear", "" },
	{ "setSpecialSceneScriptState", "" },
	// 0x90
	{ "clearSpecialSceneScriptState", "" },
	{ "querySpecialSceneScriptState", "" },
	{ "resetInputColorCode", "" },
	{ "setHiddenItemsEntry", "" },
	// 0x94
	{ "getHiddenItemsEntry", "" },
	{ "mushroomEffect", "" },
	{ "wsaClose", "" },
	{ "meanWhileScene", "" },
	// 0x98
	{ "customChat", "" },
	{ "customChatFinish", "" },
	{ "setupSceneAnimation", "" },
	{ "stopSceneAnimation", "" },
	// 0x9c
	{ "disableTimer", "" },
	{ "enableTimer", "" },
	{ "setTimerCountdown", "" },
	{ "processPaletteIndex", "" },
	// 0xa0
	{ "updateTwoSceneAnims", "" },
	{ "getRainbowRoomData", "" },
	{ "drawSceneShapeEx", "" },
	{ "midiSoundFadeout", "" },
	// 0xa4
	{ "getSfxDriver", "" },
	{ "getVocSupport", "" },
	{ "getMusicDriver", "" },
	{ "setVocHigh", "" },
	// 0xa8
	{ "getVocHigh", "" },
	{ "zanthiaChat", "" },
	{ "isVoiceEnabled", "" },
	{ "isVoicePlaying", "" },
	// 0xac
	{ "stopVoicePlaying", "" },
	{ "getGameLanguage", "" },
	{ "demoFinale", "" },
	{ "dummyAF", "" }
};

IFFChunk::IFFChunk() {
	_size = 0;
	_data = NULL;
}

Kyra::Disassembler::Disassembler(Engine *engine) : ::Disassembler(), _engine(engine) {
}

Kyra::Disassembler::~Disassembler() {
	if (_textChunk._data)
		delete[] _textChunk._data;
	if (_ordrChunk._data)
		delete[] _ordrChunk._data;
	if (_dataChunk._data)
		delete[] _dataChunk._data;
}

void Kyra::Disassembler::doDisassemble() throw(UnknownOpcodeException) {
	// Load data
	IFF_ID id;
	id = _f.readUint32BE();
	if (id != MKID_BE('FORM')) {
		std::cerr << boost::format("ERROR: Unexpected IFF magic number 0x%08X (expected 0x%08X)!\n") % id % MKID_BE('FORM');
		return;
	}
	_f.readUint32BE(); // Skip file length
	_formType = _f.readUint32BE();
	if (_formType != MKID_BE('EMC2')) {
		std::cerr << boost::format("ERROR: Unexpected file type 0x%08X (expected 0x%08X)!\n") % _formType % MKID_BE('EMC2');
		return;
	}

	// Read chunks into memory
	do {
		IFFChunk temp;
		temp._chunkType = _f.readUint32BE();
		temp._size = _f.readUint32BE();
		temp._data = new uint8[temp._size];
		_f.read_throwsOnError(temp._data, temp._size);
		switch (temp._chunkType) {
		case MKID_BE('TEXT'):
			_textChunk = temp;
			break;
		case MKID_BE('ORDR'):
			_ordrChunk = temp;
			break;
		case MKID_BE('DATA'):
			_dataChunk = temp;
			break;
		default:
			std::cerr << boost::format("ERROR: Unexpected chunk type 0x%08X!\n") % temp._chunkType;
			delete [] temp._data;
			return;
		}
		if (temp._size % 2 != 0) // Skip padding byte
			_f.readByte();
	} while (_f.pos() != (int)_f.size());

	// Extract strings from TEXT chunk
	uint16 minTextOffset = 0xFFFF;
	for (uint16 i = 0; i < _textChunk._size / 2; ++i) {
		if (minTextOffset > READ_BE_UINT16(&((uint16 *)_textChunk._data)[i])) {
			minTextOffset = READ_BE_UINT16(&((uint16 *)_textChunk._data)[i]);
		}
		if (minTextOffset <= i*2)
			break;
	}

	if (minTextOffset != 0xFFFF) {
		uint16 numStrings = minTextOffset / 2;
#define posString(x) (char*)&_textChunk._data[READ_BE_UINT16(&((uint16 *)_textChunk._data)[(x)])]
		for (uint16 i = 0; i < numStrings; ++i) {
			_engine->_textStrings.push_back(posString(i));
		}
#undef posString
	}

	uint16 minFuncAddr = 0xFFFF;
	// Extract function entry points from ORDR chunk
	std::vector<uint16> funcAddrs;
	for (size_t i = 0; i < _ordrChunk._size / 2; ++i) {
		uint16 addr = READ_BE_UINT16(&((uint16 *)_ordrChunk._data)[i]);

		if (addr == (uint16)-1)
			continue;

		addr <<= 1;

		if (minFuncAddr > addr) {
			minFuncAddr = addr;
		}

		funcAddrs.push_back(addr);
	}

	// Disassemble
	std::set<uint16> jumpTargets;
	// Map from addresses to instructions
	std::map<uint16, InstIterator> addrMap;
	uint16 numInsts = _dataChunk._size / 2;
	for (uint16 i = 0; i < numInsts; ++i) {
		uint16 address = i*2;
		uint16 code = READ_BE_UINT16(&((uint16 *)_dataChunk._data)[i]);
		int16 opcode = (code >> 8) & 0x1F;
		int16 parameter;

		if (code & 0x8000) {
			opcode = 0;
			parameter = code & 0x7FFF;
		} else if (code & 0x4000) {
			parameter = (int8)(code);
		} else if (code & 0x2000) {
			i++;
			parameter = READ_BE_UINT16(&((uint16 *)_dataChunk._data)[i]);
		} else {
			parameter = 0;
		}

#define ADD_INST addrMap[address] = _insts.insert(_insts.end(), Instruction());
#define LAST_INST (_insts[_insts.size()-1])
#define OPCODE_MD(name, category, stackChange, hasParam, codeGenData) \
		ADD_INST; \
		LAST_INST._opcode = opcode; \
		LAST_INST._address = address; \
		LAST_INST._stackChange = stackChange; \
		LAST_INST._name = name; \
		LAST_INST._type = category; \
		LAST_INST._codeGenData = codeGenData; \
		if (hasParam) { \
			Parameter p; \
			p._type = kShort; \
			p._value = parameter; \
			LAST_INST._params.push_back(p);\
		}
#define OPCODE(name, category, stackChange, hasParam) OPCODE_MD(name, category, stackChange, hasParam, "");

		// TOOD: Add metadata where applicable
		switch(opcode) {
		case 0:
			parameter *=2;
			if (parameter < minFuncAddr)
				jumpTargets.insert(_insts.size());
			OPCODE("jumpTo", kJump, 0, true);
			break;
		case 1:
			OPCODE("setRetValue", kStore, 0, true);
			break;
		case 2:
			if (parameter == 0) {
				OPCODE("pushRet", kLoad, 1, false);
			} else if (parameter == 1) {
				OPCODE("pushPos", kSpecial, 2, false); // Sets up function call
			} else {
				// Error: invalid parameter halts execution
			}
			break;
		case 3:
		case 4:
			OPCODE("push", kLoad, 1, true);
			break;
		case 5:
			OPCODE("pushVar", kLoad, 1, true);
			break;
		case 6:
			OPCODE("pushBPNeg", kLoad, 1, true);
			break;
		case 7:
			OPCODE("pushBPAdd", kLoad, 1, true);
			break;
		case 8:
			if (parameter == 0) {
				OPCODE("popRet", kStore, -1, false);
			} else if (parameter == 1) {
				OPCODE("popPos", kReturn, -2, false); // Returns from function call
			} else {
				// Error: invalid parameter halts execution
			}
			break;
		case 9:
			OPCODE("popVar", kStore, 1, true);
			break;
		case 10:
			OPCODE("popBPNeg", kStore, 1, true);
			break;
		case 11:
			OPCODE("popBPAdd", kStore, 1, true);
			break;
		case 12:
			OPCODE("addSP", kStack, -parameter, true);
			break;
		case 13:
			OPCODE("subSP", kStack, parameter, true);
			break;
		case 14:
			parameter = (uint8)parameter;
			if ((uint16)parameter >= sizeof(kyra2FuncDesc) / sizeof(kyra2FuncDesc[0]) || kyra2FuncDesc[parameter]._name.length() == 0) {
				// Error: unknown function
			}
			OPCODE_MD(kyra2FuncDesc[parameter]._name, kSpecial, 0, false, kyra2FuncDesc[parameter]._metadata)
			break;
		case 15:
			parameter *=2;
			if (parameter < minFuncAddr)
				jumpTargets.insert(_insts.size());
			OPCODE("ifNotJmp", kCondJump, -1, true);
			break;
		case 16:
			if (parameter == 0) {
				OPCODE_MD("boolCast", kUnaryOp, 0, false, "(bool)");
			} else if (parameter == 1) {
				OPCODE_MD("arithmeticNegate", kUnaryOp, 0, false,"-");
			} else if (parameter == 2) {
				OPCODE_MD("bitwiseNegate", kUnaryOp, 0, false, "~");
			} else {
				// Error: invalid parameter halts execution
			}
			break;
		case 17:
			switch (parameter) {
				case 0:
					OPCODE_MD("eval_band", kBinaryOp, -1, false, "&&");
					break;
				case 1:
					OPCODE_MD("eval_bor", kBinaryOp, -1, false, "||");
					break;
				case 2:
					OPCODE_MD("eval_eq", kComparison, -1, false, "==");
					break;
				case 3:
					OPCODE_MD("eval_neq", kComparison, -1, false, "!=");
					break;
				case 4:
					OPCODE_MD("eval_leq", kComparison, -1, false, "<=");
					break;
				case 5:
					OPCODE_MD("eval_lt", kComparison, -1, false, "<");
					break;
				case 6:
					OPCODE_MD("eval_geq", kComparison, -1, false, ">=");
					break;
				case 7:
					OPCODE_MD("eval_gt", kComparison, -1, false, ">");
					break;
				case 8:
					OPCODE_MD("eval_add", kBinaryOp, -1, false, "+");
					break;
				case 9:
					OPCODE_MD("eval_sub", kBinaryOp, -1, false, "-");
					break;
				case 10:
					OPCODE_MD("eval_mult", kBinaryOp, -1, false, "*");
					break;
				case 11:
					OPCODE_MD("eval_div", kBinaryOp, -1, false, "/");
					break;
				case 12:
					OPCODE_MD("eval_shr", kBinaryOp, -1, false, ">>");
					break;
				case 13:
					OPCODE_MD("eval_shl", kBinaryOp, -1, false, "<<");
					break;
				case 14:
					OPCODE_MD("eval_land", kBinaryOp, -1, false, "&");
					break;
				case 15:
					OPCODE_MD("eval_lor", kBinaryOp, -1, false, "|");
					break;
				case 16:
					OPCODE_MD("eval_mod", kBinaryOp, -1, false, "%");
					break;
				case 17:
					OPCODE_MD("eval_xor", kBinaryOp, -1, false, "^");
					break;
				default:
					// Error: Invalid parameter
					break;
			}
			break;
		case 18:
			OPCODE("setRetAndJmp", kSpecial, -2, false);
			break;
		default:
			throw UnknownOpcodeException(i*2, code);
		}
#undef OPCODE
#undef OPCODE_MD
#undef LAST_INST
#undef ADD_INST
	}

	// Function detection
	uint16 nextFunc = 0;
	// Process candidate entry points
	for (std::set<uint16>::iterator it = jumpTargets.begin(); it != jumpTargets.end(); ++it) {
		// If candidate was already placed inside a function, skip it
		if (_insts[*it]._address < nextFunc)
			continue;
		// Determine function end point
		bool lastWasPop = false;

		for (int i = *it ; _insts[i]._address < minFuncAddr; i++) {
			if (_insts[i]._address < *it)
				continue;
			// Kyra2 sometimes has an addSP instruction between the two popPos instrucitons, so we ignore those
			if (_insts[i]._name.compare("addSP") == 0)
					continue;

			if (lastWasPop && _insts[i]._name.compare("popPos") == 0) {
				_engine->_functions[_insts[*it]._address] = Function(addrMap[_insts[*it]._address], addrMap[_insts[i]._address]);
				nextFunc = _insts[i]._address;
				break;
			}

			lastWasPop = (_insts[i]._name.compare("popPos") == 0);
		}
	}

	for (FuncMap::iterator it = _engine->_functions.begin(); it != _engine->_functions.end(); ++it) {
		std::stringstream s;
		s << boost::format("sub0x%X") % it->second._startIt->_address;
		int maxArg = 0;
		for (InstIterator instIt = it->second._startIt; instIt != it->second._endIt; ++instIt) {
			if (instIt->_name.compare("pushBPAdd") == 0) {
				if (maxArg < instIt->_params[0].getSigned()) {
					maxArg = instIt->_params[0].getSigned();
				}
			}
		}
		it->second._args = maxArg;
		it->second._retVal = true;
	}

	std::sort(funcAddrs.begin(), funcAddrs.end());
	//Create ranges from entry points
	for (size_t i = 0; i < funcAddrs.size(); i++) {
		if (i == funcAddrs.size() - 1) // Last function
			_engine->_functions[funcAddrs[i]] = Function(addrMap[funcAddrs[i]], _insts.end());
		else
			_engine->_functions[funcAddrs[i]] = Function(addrMap[funcAddrs[i]], addrMap[funcAddrs[i+1]]);
	}
}
