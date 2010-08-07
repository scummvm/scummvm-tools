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
	{ "o2_setCharacterFacingRefresh", "0ppp" },
	{ "o2_setCharacterPos", "0ppp" },
	{ "o2_defineObject", "0pspppp" },
	{ "o2_refreshCharacter", "0pppp" },
	// 0x04
	{ "o2_getCharacterX", "" },
	{ "o2_getCharacterY", "" },
	{ "o2_getCharacterFacing", "" },
	{ "o2_getCharacterScene", "" },
	// 0x08
	{ "o2_setSceneComment", "0s" },
	{ "unk09", "0p" },
	{ "unk0A", "0p" },
	{ "o2_setCharacterAnimFrame", "0ppp" },
	// 0x0c
	{ "o2_setCharacterFacingOverwrite", "0p" },
	{ "o2_trySceneChange", "0pppp" },
	{ "o2_moveCharacter", "0ppp" },
	{ "o2_customCharacterChat", "0spppp" },
	// 0x10
	{ "o2_soundFadeOut", "" },
	{ "o2_showChapterMessage", "0pp" },
	{ "o2_restoreTalkTextMessageBkgd", "" },
	{ "o2_printString", "0spppp" },
	// 0x14
	{ "o2_wsaClose", "0p" },
	{ "o2_backUpScreen", "0p" },
	{ "o2_restoreScreen", "0p" },
	{ "o2_displayWsaFrame", "0ppppppppp" },
	// 0x18
	{ "o2_displayWsaSequentialFramesLooping", "0pppppppp" },
	{ "o2_wsaOpen", "0sp" },
	{ "o2_displayWsaSequentialFrames", "0ppppppp" },
	{ "o2_displayWsaSequence", "0pppppp" },
	// 0x1c
	{ "o2_addItemToInventory", "0ppp" },
	{ "o2_drawShape", "0ppppp" },
	{ "o2_addItemToCurScene", "0ppp" },
	{ "o2_limitMouseRange", "0pppp" },
	// 0x20
	{ "o2_checkForItem", "0pp" },
	{ "o2_loadSoundFile", "0p" },
	{ "o2_removeSlotFromInventory", "0p" },
	{ "o2_defineItem", "0pppp" },
	// 0x24
	{ "o2_removeItemFromInventory", "0p" },
	{ "o2_countItemInInventory", "0pp" },
	{ "o2_countItemsInScene", "0p" },
	{ "o1_queryGameFlag", "0p" },
	// 0x28
	{ "o1_resetGameFlag", "0p" },
	{ "o1_setGameFlag", "0p" },
	{ "o1_setHandItem", "0p" },
	{ "o1_removeHandItem", "" },
	// 0x2c
	{ "o1_getMouseState", "" },
	{ "o1_hideMouse", "" },
	{ "o2_addSpecialExit", "0ppppp" },
	{ "o1_setMousePos", "0pp" },
	// 0x30
	{ "o1_showMouse", "" },
	{ "o2_drawBox", "0ppppp" },
	{ "o2_wipeDownMouseItem", "0ppp" },
	{ "o2_getElapsedSecs", "" },
	// 0x34
	{ "o2_getTimerDelay", "0p" },
	{ "o1_playSoundEffect", "0p" },
	{ "o2_delaySecs", "0p" },
	{ "o2_delay", "0pp" },
	// 0x38
	{ "o2_dummy38", "" },
	{ "o2_setTimerDelay", "0pp" },
	{ "o2_setScaleTableItem", "0pp" },
	{ "o2_setDrawLayerTableItem", "0pp" },
	// 0x3c
	{ "o2_setCharPalEntry", "0pp" },
	{ "o2_loadZShapes", "0p" },
	{ "o2_drawSceneShape", "0pppp" },
	{ "o2_drawSceneShapeOnPage", "0ppppp" },
	// 0x40
	{ "o2_disableAnimObject", "0p" },
	{ "o2_enableAnimObject", "0p" },
	{ "o2_dummy42", "" },
	{ "o2_loadPalette384", "0s" },
	// 0x44
	{ "o2_setPalette384", "" },
	{ "o2_restoreBackBuffer", "0p" },
	{ "o2_backUpInventoryGfx", "" },
	{ "o2_disableSceneAnim", "0p" },
	// 0x48
	{ "o2_enableSceneAnim", "0p" },
	{ "o2_restoreInventoryGfx", "" },
	{ "o2_setSceneAnimPos2", "0ppp" },
	{ "o2_update", "0p" },
	// 0x4c
	{ "unk4C_palFade?", "0pp" },
	{ "o2_fadeScenePal", "0pp" },
	{ "o2_dummy4E", "" },
	{ "o2_dummy4F", "" },
	// 0x50
	{ "o2_enterNewScene", "0ppppp" },
	{ "o2_switchScene", "0p" },
	{ "o2_getShapeFlag1", "0pp" },
	{ "o2_setPathfinderFlag", "0p" },
	// 0x54
	{ "o2_getSceneExitToFacing", "0pp" },
	{ "o2_setLayerFlag", "0p" },
	{ "o2_setZanthiaPos", "0pp" },
	{ "o2_loadMusicTrack", "0p" },
	// 0x58
	{ "o1_playWanderScoreViaMap", "0pp" },
	{ "o1_playSoundEffect", "0p" },
	{ "o2_setSceneAnimPos", "0ppp" },
	{ "o1_blockInWalkableRegion", "0pppp" },
	// 0x5c
	{ "o1_blockOutWalkableRegion", "0pppp" },
	{ "unk5D", "0ppppp" },
	{ "o2_setCauldronState", "0pp" },
	{ "o2_showItemString", "0pp" },
	// 0x60
	{ "o1_getRand", "0pp" },
	{ "o2_isAnySoundPlaying", "" },
	{ "o1_setDeathHandler", "0p" },
	{ "o2_setDrawNoShapeFlag", "0p" },
	// 0x64
	{ "o2_setRunFlag", "0p" },
	{ "o2_showLetter", "0p" },
	{ "o1_shakeScreen", "0pp" },
	{ "o1_fillRect", "0pppppp" },
	// 0x68
	{ "o2_getKey", "" },
	{ "unk69", "0pppp" },
	{ "o2_playFireflyScore", "" },
	{ "o2_waitForConfirmationClick", "0p" },
	// 0x6c
	{ "o2_encodeShape", "0ppppp" },
	{ "o2_defineRoomEntrance", "0ppp" },
	{ "o2_runAnimationScript", "0sppp" },
	{ "o2_setSpecialSceneScriptRunTime", "0pp" },
	// 0x70
	{ "o2_defineSceneAnim", "0pppppppppppps" },
	{ "o2_updateSceneAnim", "pp" },
	{ "o2_updateSceneAnim", "pp" },
	{ "o2_addToSceneAnimPosAndUpdate", "0ppp" },
	// 0x74
	{ "o2_useItemOnMainChar", "" },
	{ "o2_startDialogue", "0p" },
	{ "o2_randomSceneChat", "" },
	{ "o2_setDlgIndex", "0p" },
	// 0x78
	{ "o2_getDlgIndex", "" },
	{ "o2_defineScene", "0pspppppp" },
	{ "o2_addCauldronStateTableEntry", "0pp" },
	{ "o2_setCountDown", "0p" },
	// 0x7c
	{ "o2_getCountDown", "" },
	{ "o2_dummy7D", "" },
	{ "o2_dummy7E", "" },
	{ "o2_pressColorKey", "0p" },
	// 0x80
	{ "o2_objectChat", "0sp" },
	{ "o2_changeChapter", "0pp" },
	{ "o2_getColorCodeFlag1", "" },
	{ "o2_setColorCodeFlag1", "0p" },
	// 0x84
	{ "o2_getColorCodeFlag2", "" },
	{ "o2_setColorCodeFlag2", "0p" },
	{ "o2_getColorCodeValue", "0p" },
	{ "o2_setColorCodeValue", "0pp" },
	// 0x88
	{ "o2_countItemInstances", "0p" },
	{ "o2_removeItemFromScene", "0pp" },
	{ "o2_initObject", "0p" },
	{ "o2_npcChat", "0spp" }, // FIXME: Non-talkie metadata; talkie opcode is 0spp
	// 0x8c
	{ "o2_deinitObject", "0p" },
	{ "o2_playTimSequence", "0s" },
	{ "o2_makeBookOrCauldronAppear", "0p" },
	{ "o2_setSpecialSceneScriptState", "0p" },
	// 0x90
	{ "o2_clearSpecialSceneScriptState", "0p" },
	{ "o2_querySpecialSceneScriptState", "0p" },
	{ "o2_resetInputColorCode", "" },
	{ "o2_setHiddenItemsEntry", "0pp" },
	// 0x94
	{ "o2_getHiddenItemsEntry", "0p" },
	{ "o2_mushroomEffect", "" },
	{ "o2_wsaClose", "0p" },
	{ "o2_meanWhileScene", "0p" },
	// 0x98
	{ "o2_customChat", "0spp" },
	{ "o2_customChatFinish", "" },
	{ "o2_setupSceneAnimation", "0pppppppppppps" },
	{ "o2_stopSceneAnimation", "0pp" },
	// 0x9c
	{ "o2_disableTimer", "0p" },
	{ "o2_enableTimer", "0p" },
	{ "o2_setTimerCountdown", "0pp" },
	{ "o2_processPaletteIndex", "0pppppp" },
	// 0xa0
	{ "o2_updateTwoSceneAnims", "0pppp" },
	{ "o2_getRainbowRoomData", "0p" },
	{ "o2_drawSceneShapeEx", "0pppp" },
	{ "o2_midiSoundFadeout", "" },
	// 0xa4
	{ "o2_getSfxDriver", "" },
	{ "o2_getVocSupport", "" },
	{ "o2_getMusicDriver", "" },
	{ "o2_setVocHigh", "0p" },
	// 0xa8
	{ "o2_getVocHigh", "" },
	{ "o2_zanthiaChat", "0sp" },
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
