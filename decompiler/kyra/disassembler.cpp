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

//#include <algorithm>
#include <boost/format.hpp>

struct FunctionData {
  std::string _name;
	std::string _metadata;
};

static FunctionData kyra2FuncDesc[] = {
	// 0x00
	{ "o2_setCharacterFacingRefresh", "" },
	{ "o2_setCharacterPos", "" },
	{ "o2_defineObject", "" },
	{ "o2_refreshCharacter", "" },
	// 0x04
	{ "o2_getCharacterX", "" },
	{ "o2_getCharacterY", "" },
	{ "o2_getCharacterFacing", "" },
	{ "o2_getCharacterScene", "" },
	// 0x08
	{ "o2_setSceneComment", "" },
	{ "unk09", "" },
	{ "unk0A", "" },
	{ "o2_setCharacterAnimFrame", "" },
	// 0x0c
	{ "o2_setCharacterFacingOverwrite", "" },
	{ "o2_trySceneChange", "" },
	{ "o2_moveCharacter", "" },
	{ "o2_customCharacterChat", "" },
	// 0x10
	{ "o2_soundFadeOut", "" },
	{ "o2_showChapterMessage", "" },
	{ "o2_restoreTalkTextMessageBkgd", "" },
	{ "unk13", "" },
	// 0x14
	{ "o2_wsaClose", "" },
	{ "o2_backUpScreen", "" },
	{ "o2_restoreScreen", "" },
	{ "o2_displayWsaFrame", "" },
	// 0x18
	{ "o2_displayWsaSequentialFramesLooping", "" },
	{ "o2_wsaOpen", "" },
	{ "o2_displayWsaSequentialFrames", "" },
	{ "o2_displayWsaSequence", "" },
	// 0x1c
	{ "o2_addItemToInventory", "" },
	{ "o2_drawShape", "" },
	{ "o2_addItemToCurScene", "" },
	{ "o2_limitMouseRange", "" },
	// 0x20
	{ "o2_checkForItem", "" },
	{ "o2_loadSoundFile", "" },
	{ "o2_removeSlotFromInventory", "" },
	{ "o2_defineItem", "" },
	// 0x24
	{ "o2_removeItemFromInventory", "" },
	{ "o2_countItemInInventory", "" },
	{ "o2_countItemsInScene", "" },
	{ "o1_queryGameFlag", "" },
	// 0x28
	{ "o1_resetGameFlag", "" },
	{ "o1_setGameFlag", "" },
	{ "o1_setHandItem", "" },
	{ "o1_removeHandItem", "" },
	// 0x2c
	{ "o1_getMouseState", "" },
	{ "o1_hideMouse", "" },
	{ "o2_addSpecialExit", "" },
	{ "o1_setMousePos", "" },
	// 0x30
	{ "o1_showMouse", "" },
	{ "unk31", "" },
	{ "o2_wipeDownMouseItem", "" },
	{ "o2_getElapsedSecs", "" },
	// 0x34
	{ "o2_getTimerDelay", "" },
	{ "o1_playSoundEffect", "" },
	{ "o2_delaySecs", "" },
	{ "o2_delay", "" },
	// 0x38
	{ "o2_dummy38", "" },
	{ "o2_setTimerDelay", "" },
	{ "o2_setScaleTableItem", "" },
	{ "o2_setDrawLayerTableItem", "" },
	// 0x3c
	{ "o2_setCharPalEntry", "" },
	{ "o2_loadZShapes", "" },
	{ "o2_drawSceneShape", "" },
	{ "o2_drawSceneShapeOnPage", "" },
	// 0x40
	{ "o2_disableAnimObject", "" },
	{ "o2_enableAnimObject", "" },
	{ "o2_dummy42", "" },
	{ "o2_loadPalette384", "" },
	// 0x44
	{ "o2_setPalette384", "" },
	{ "o2_restoreBackBuffer", "" },
	{ "o2_backUpInventoryGfx", "" },
	{ "o2_disableSceneAnim", "" },
	// 0x48
	{ "o2_enableSceneAnim", "" },
	{ "o2_restoreInventoryGfx", "" },
	{ "o2_setSceneAnimPos2", "" },
	{ "o2_update", "" },
	// 0x4c
	{ "unk4C", "" },
	{ "o2_fadeScenePal", "" },
	{ "o2_dummy4E", "" },
	{ "o2_dummy4F", "" },
	// 0x50
	{ "o2_enterNewScene", "" },
	{ "o2_switchScene", "" },
	{ "o2_getShapeFlag1", "" },
	{ "o2_setPathfinderFlag", "" },
	// 0x54
	{ "o2_getSceneExitToFacing", "" },
	{ "o2_setLayerFlag", "" },
	{ "o2_setZanthiaPos", "" },
	{ "o2_loadMusicTrack", "" },
	// 0x58
	{ "o1_playWanderScoreViaMap", "" },
	{ "o1_playSoundEffect", "" },
	{ "o2_setSceneAnimPos", "" },
	{ "o1_blockInWalkableRegion", "" },
	// 0x5c
	{ "o1_blockOutWalkableRegion", "" },
	{ "unk5D", "" },
	{ "o2_setCauldronState", "" },
	{ "o2_showItemString", "" },
	// 0x60
	{ "o1_getRand", "" },
	{ "o2_isAnySoundPlaying", "" },
	{ "o1_setDeathHandler", "" },
	{ "o2_setDrawNoShapeFlag", "" },
	// 0x64
	{ "o2_setRunFlag", "" },
	{ "o2_showLetter", "" },
	{ "unk66", "" },
	{ "o1_fillRect", "" },
	// 0x68
	{ "unk68", "" },
	{ "unk69", "" },
	{ "o2_playFireflyScore", "" },
	{ "o2_waitForConfirmationClick", "" },
	// 0x6c
	{ "o2_encodeShape", "" },
	{ "o2_defineRoomEntrance", "" },
	{ "o2_runAnimationScript", "" },
	{ "o2_setSpecialSceneScriptRunTime", "" },
	// 0x70
	{ "o2_defineSceneAnim", "" },
	{ "o2_updateSceneAnim", "" },
	{ "o2_updateSceneAnim", "" },
	{ "o2_addToSceneAnimPosAndUpdate", "" },
	// 0x74
	{ "o2_useItemOnMainChar", "" },
	{ "o2_startDialogue", "" },
	{ "o2_randomSceneChat", "" },
	{ "o2_setDlgIndex", "" },
	// 0x78
	{ "o2_getDlgIndex", "" },
	{ "o2_defineScene", "" },
	{ "o2_addCauldronStateTableEntry", "" },
	{ "o2_setCountDown", "" },
	// 0x7c
	{ "o2_getCountDown", "" },
	{ "o2_dummy7D", "" },
	{ "o2_dummy7E", "" },
	{ "o2_pressColorKey", "" },
	// 0x80
	{ "o2_objectChat", "" },
	{ "o2_changeChapter", "" },
	{ "o2_getColorCodeFlag1", "" },
	{ "o2_setColorCodeFlag1", "" },
	// 0x84
	{ "o2_getColorCodeFlag2", "" },
	{ "o2_setColorCodeFlag2", "" },
	{ "o2_getColorCodeValue", "" },
	{ "o2_setColorCodeValue", "" },
	// 0x88
	{ "o2_countItemInstances", "" },
	{ "o2_removeItemFromScene", "" },
	{ "o2_initObject", "" },
	{ "o2_npcChat", "" },
	// 0x8c
	{ "o2_deinitObject", "" },
	{ "o2_playTimSequence", "" },
	{ "o2_makeBookOrCauldronAppear", "" },
	{ "o2_setSpecialSceneScriptState", "" },
	// 0x90
	{ "o2_clearSpecialSceneScriptState", "" },
	{ "o2_querySpecialSceneScriptState", "" },
	{ "o2_resetInputColorCode", "" },
	{ "o2_setHiddenItemsEntry", "" },
	// 0x94
	{ "o2_getHiddenItemsEntry", "" },
	{ "o2_mushroomEffect", "" },
	{ "o2_wsaClose", "" },
	{ "o2_meanWhileScene", "" },
	// 0x98
	{ "o2_customChat", "" },
	{ "o2_customChatFinish", "" },
	{ "o2_setupSceneAnimation", "" },
	{ "o2_stopSceneAnimation", "" },
	// 0x9c
	{ "o2_disableTimer", "" },
	{ "o2_enableTimer", "" },
	{ "o2_setTimerCountdown", "" },
	{ "o2_processPaletteIndex", "" },
	// 0xa0
	{ "o2_updateTwoSceneAnims", "" },
	{ "o2_getRainbowRoomData", "" },
	{ "o2_drawSceneShapeEx", "" },
	{ "o2_midiSoundFadeout", "" },
	// 0xa4
	{ "o2_getSfxDriver", "" },
	{ "o2_getVocSupport", "" },
	{ "o2_getMusicDriver", "" },
	{ "o2_setVocHigh", "" },
	// 0xa8
	{ "o2_getVocHigh", "" },
	{ "o2_zanthiaChat", "" },
	{ "o2_isVoiceEnabled", "" },
	{ "o2_isVoicePlaying", "" },
	// 0xac
	{ "o2_stopVoicePlaying", "" },
	{ "o2_getGameLanguage", "" },
	{ "o2_demoFinale", "" },
	{ "o2_dummyAF", "" }
};

IFFChunk::IFFChunk() {
	_size = 0;
	_data = NULL;
}

Kyra::Disassembler::Disassembler(Engine *engine, std::vector<Instruction> &insts) : ::Disassembler(insts), _engine(engine) {
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

		addr++;
		addr <<= 1;

		if (minFuncAddr > addr) {
			minFuncAddr = addr;
		}

		funcAddrs.push_back(addr);
	}

	// Disassemble
	std::set<uint16> jumpTargets;
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

#define ADD_INST _insts.insert(_insts.end(), Instruction());
#define LAST_INST (_insts[_insts.size()-1])
#define OPCODE_MD(name, category, stackChange, hasParam, isSigned, codeGenData) \
		ADD_INST; \
		LAST_INST._opcode = opcode; \
		LAST_INST._address = address; \
		LAST_INST._stackChange = stackChange; \
		LAST_INST._name = name; \
		LAST_INST._type = category; \
		LAST_INST._codeGenData = codeGenData; \
		if (hasParam) { \
			Parameter p; \
			if (isSigned) { \
				p._type = kShort; \
				p._value = parameter; \
			} else { \
				p._type = kUShort; \
				p._value = (uint32)parameter; \
			} \
			LAST_INST._params.push_back(p);\
		}
#define OPCODE(name, category, stackChange, hasParam, isSigned) OPCODE_MD(name, category, stackChange, hasParam, isSigned, "");

		// TOOD: Add metadata where applicable
		switch(opcode) {
		case 0:
			parameter *= 2;
			if (parameter < minFuncAddr)
				jumpTargets.insert(_insts.size());
			OPCODE("jumpTo", kJump, 0, true, false);
			break;
		case 1:
			OPCODE("setRetValue", kStore, 0, true, true);
			break;
		case 2:
			if (parameter == 0) {
				OPCODE("pushRet", kLoad, 1, true, false);
			} else if (parameter == 1) {
				OPCODE_MD("pushPos", kSpecial, 0, true, false, "\xC0"); // Sets up function call
			} else {
				// Error: invalid parameter halts execution
			}
			break;
		case 3:
		case 4:
			OPCODE("push", kLoad, 1, true, true);
			break;
		case 5:
			OPCODE("pushVar", kLoad, 1, true, true);
			break;
		case 6:
			OPCODE("pushBPNeg", kLoad, 1, true, true);
			break;
		case 7:
			OPCODE("pushBPAdd", kLoad, 1, true, true);
			break;
		case 8:
			if (parameter == 0) {
				OPCODE("popRet", kStore, -1, true, false);
			} else if (parameter == 1) {
				OPCODE("popPos", kReturn, 0, true, false); // Returns from function call
			} else {
				// Error: invalid parameter halts execution
			}
			break;
		case 9:
			OPCODE("popVar", kStore, -1, true, true);
			break;
		case 10:
			OPCODE("popBPNeg", kStore, -1, true, true);
			break;
		case 11:
			OPCODE("popBPAdd", kStore, -1, true, true);
			break;
		case 12:
			OPCODE("addSP", kStack, -parameter, true, true);
			break;
		case 13:
			OPCODE("subSP", kStack, parameter, true, true);
			break;
		case 14:
			parameter = (uint8)parameter;
			if ((uint16)parameter >= sizeof(kyra2FuncDesc) / sizeof(kyra2FuncDesc[0]) || kyra2FuncDesc[parameter]._name.length() == 0) {
				// Error: unknown function
			}
			OPCODE_MD(kyra2FuncDesc[parameter]._name, kSpecial, 0, false, false, kyra2FuncDesc[parameter]._metadata)
			break;
		case 15:
			parameter *= 2;
			if (parameter < minFuncAddr)
				jumpTargets.insert(_insts.size());
			OPCODE("ifNotJmp", kCondJump, -1, true, false);
			break;
		case 16:
			if (parameter == 0) {
				OPCODE_MD("boolCast", kUnaryOp, 0, false, false, "(bool)");
			} else if (parameter == 1) {
				OPCODE_MD("arithmeticNegate", kUnaryOp, 0, false, false,"-");
			} else if (parameter == 2) {
				OPCODE_MD("bitwiseNegate", kUnaryOp, 0, false, false, "~");
			} else {
				// Error: invalid parameter halts execution
			}
			break;
		case 17:
			switch (parameter) {
				case 0:
					OPCODE_MD("eval_band", kBinaryOp, -1, false, false, "&&");
					break;
				case 1:
					OPCODE_MD("eval_bor", kBinaryOp, -1, false, false, "||");
					break;
				case 2:
					OPCODE_MD("eval_eq", kComparison, -1, false, false, "==");
					break;
				case 3:
					OPCODE_MD("eval_neq", kComparison, -1, false, false, "!=");
					break;
				case 4:
					OPCODE_MD("eval_leq", kComparison, -1, false, false, "<=");
					break;
				case 5:
					OPCODE_MD("eval_lt", kComparison, -1, false, false, "<");
					break;
				case 6:
					OPCODE_MD("eval_geq", kComparison, -1, false, false, ">=");
					break;
				case 7:
					OPCODE_MD("eval_gt", kComparison, -1, false, false, ">");
					break;
				case 8:
					OPCODE_MD("eval_add", kBinaryOp, -1, false, false, "+");
					break;
				case 9:
					OPCODE_MD("eval_sub", kBinaryOp, -1, false, false, "-");
					break;
				case 10:
					OPCODE_MD("eval_mult", kBinaryOp, -1, false, false, "*");
					break;
				case 11:
					OPCODE_MD("eval_div", kBinaryOp, -1, false, false, "/");
					break;
				case 12:
					OPCODE_MD("eval_shr", kBinaryOp, -1, false, false, ">>");
					break;
				case 13:
					OPCODE_MD("eval_shl", kBinaryOp, -1, false, false, "<<");
					break;
				case 14:
					OPCODE_MD("eval_land", kBinaryOp, -1, false, false, "&");
					break;
				case 15:
					OPCODE_MD("eval_lor", kBinaryOp, -1, false, false, "|");
					break;
				case 16:
					OPCODE_MD("eval_mod", kBinaryOp, -1, false, false, "%");
					break;
				case 17:
					OPCODE_MD("eval_xor", kBinaryOp, -1, false, false, "^");
					break;
				default:
					// Error: Invalid parameter
					break;
			}
			break;
		case 18:
			OPCODE("setRetAndJmp", kSpecial, -2, false, false);
			break;
		default:
			throw UnknownOpcodeException(i*2, code);
		}
#undef OPCODE
#undef OPCODE_MD
#undef LAST_INST
#undef ADD_INST
	}

	// Map from addresses to instructions
	std::map<uint16, InstIterator> addrMap;

	for (InstIterator it = _insts.begin(); it != _insts.end(); ++it)
		addrMap[it->_address] = it;

	std::sort(funcAddrs.begin(), funcAddrs.end());
	//Create ranges from entry points
	for (size_t i = 0; i < funcAddrs.size(); i++) {
		if (i == funcAddrs.size() - 1) // Last function
			_engine->_functions[funcAddrs[i]] = Function(addrMap[funcAddrs[i]], _insts.end());
		else
			_engine->_functions[funcAddrs[i]] = Function(addrMap[funcAddrs[i]], addrMap[funcAddrs[i+1]]);
	}

	// Correct jumps to functions so they're treated as calls
	bool lastWasPushPos = false;
	for (InstIterator it = _insts.begin(); it != _insts.end(); ++it) {
		if (it->_type == kJump || it->_type == kCondJump) {
			if (lastWasPushPos || _engine->_functions.find(it->_params[0].getUnsigned()) != _engine->_functions.end()) {
				it->_type = kCall;
			}
		}
		lastWasPushPos = (it->_name.compare("pushPos") == 0);
	}
}
