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
	{ "o2_setCharacterFacingRefresh", "\xC0r0ppp" },
	{ "o2_setCharacterPos", "\xC0r0ppp" },
	{ "o2_defineObject", "\xC0r0pspppp" },
	{ "o2_refreshCharacter", "\xC0r0pppp" },
	// 0x04
	{ "o2_getCharacterX", "\xC0r" },
	{ "o2_getCharacterY", "\xC0r" },
	{ "o2_getCharacterFacing", "\xC0r" },
	{ "o2_getCharacterScene", "\xC0r" },
	// 0x08
	{ "o2_setSceneComment", "\xC0r0s" },
	{ "unk09", "\xC0r0p" },
	{ "unk0A", "\xC0r0p" },
	{ "o2_setCharacterAnimFrame", "\xC0r0ppp" },
	// 0x0c
	{ "o2_setCharacterFacingOverwrite", "\xC0r0p" },
	{ "o2_trySceneChange", "\xC0r0pppp" },
	{ "o2_moveCharacter", "\xC0r0ppp" },
	{ "o2_customCharacterChat", "\xC0r0spppp" },
	// 0x10
	{ "o2_soundFadeOut", "\xC0r" },
	{ "o2_showChapterMessage", "\xC0r0pp" },
	{ "o2_restoreTalkTextMessageBkgd", "\xC0r" },
	{ "o2_printString", "\xC0r0spppp" },
	// 0x14
	{ "o2_wsaClose", "\xC0r0p" },
	{ "o2_backUpScreen", "\xC0r0p" },
	{ "o2_restoreScreen", "\xC0r0p" },
	{ "o2_displayWsaFrame", "\xC0r0ppppppppp" },
	// 0x18
	{ "o2_displayWsaSequentialFramesLooping", "\xC0r0pppppppp" },
	{ "o2_wsaOpen", "\xC0r0sp" },
	{ "o2_displayWsaSequentialFrames", "\xC0r0ppppppp" },
	{ "o2_displayWsaSequence", "\xC0r0pppppp" },
	// 0x1c
	{ "o2_addItemToInventory", "\xC0r0ppp" },
	{ "o2_drawShape", "\xC0r0ppppp" },
	{ "o2_addItemToCurScene", "\xC0r0ppp" },
	{ "o2_limitMouseRange", "\xC0r0pppp" },
	// 0x20
	{ "o2_checkForItem", "\xC0r0pp" },
	{ "o2_loadSoundFile", "\xC0r0p" },
	{ "o2_removeSlotFromInventory", "\xC0r0p" },
	{ "o2_defineItem", "\xC0r0pppp" },
	// 0x24
	{ "o2_removeItemFromInventory", "\xC0r0p" },
	{ "o2_countItemInInventory", "\xC0r0pp" },
	{ "o2_countItemsInScene", "\xC0r0p" },
	{ "o1_queryGameFlag", "\xC0r0p" },
	// 0x28
	{ "o1_resetGameFlag", "\xC0r0p" },
	{ "o1_setGameFlag", "\xC0r0p" },
	{ "o1_setHandItem", "\xC0r0p" },
	{ "o1_removeHandItem", "\xC0r" },
	// 0x2c
	{ "o1_getMouseState", "\xC0r" },
	{ "o1_hideMouse", "\xC0r" },
	{ "o2_addSpecialExit", "\xC0r0ppppp" },
	{ "o1_setMousePos", "\xC0r0pp" },
	// 0x30
	{ "o1_showMouse", "\xC0r" },
	{ "o2_drawBox", "\xC0r0ppppp" },
	{ "o2_wipeDownMouseItem", "\xC0r0ppp" },
	{ "o2_getElapsedSecs", "\xC0r" },
	// 0x34
	{ "o2_getTimerDelay", "\xC0r0p" },
	{ "o1_playSoundEffect", "\xC0r0p" },
	{ "o2_delaySecs", "\xC0r0p" },
	{ "o2_delay", "\xC0r0pp" },
	// 0x38
	{ "o2_dummy38", "\xC0r" },
	{ "o2_setTimerDelay", "\xC0r0pp" },
	{ "o2_setScaleTableItem", "\xC0r0pp" },
	{ "o2_setDrawLayerTableItem", "\xC0r0pp" },
	// 0x3c
	{ "o2_setCharPalEntry", "\xC0r0pp" },
	{ "o2_loadZShapes", "\xC0r0p" },
	{ "o2_drawSceneShape", "\xC0r0pppp" },
	{ "o2_drawSceneShapeOnPage", "\xC0r0ppppp" },
	// 0x40
	{ "o2_disableAnimObject", "\xC0r0p" },
	{ "o2_enableAnimObject", "\xC0r0p" },
	{ "o2_dummy42", "\xC0r" },
	{ "o2_loadPalette384", "\xC0r0s" },
	// 0x44
	{ "o2_setPalette384", "\xC0r" },
	{ "o2_restoreBackBuffer", "\xC0r0p" },
	{ "o2_backUpInventoryGfx", "\xC0r" },
	{ "o2_disableSceneAnim", "\xC0r0p" },
	// 0x48
	{ "o2_enableSceneAnim", "\xC0r0p" },
	{ "o2_restoreInventoryGfx", "\xC0r" },
	{ "o2_setSceneAnimPos2", "\xC0r0ppp" },
	{ "o2_update", "\xC0r0p" },
	// 0x4c
	{ "unk4C_palFade?", "\xC0r0pp" },
	{ "o2_fadeScenePal", "\xC0r0pp" },
	{ "o2_dummy4E", "\xC0r" },
	{ "o2_dummy4F", "\xC0r" },
	// 0x50
	{ "o2_enterNewScene", "\xC0r0ppppp" },
	{ "o2_switchScene", "\xC0r0p" },
	{ "o2_getShapeFlag1", "\xC0r0pp" },
	{ "o2_setPathfinderFlag", "\xC0r0p" },
	// 0x54
	{ "o2_getSceneExitToFacing", "\xC0r0pp" },
	{ "o2_setLayerFlag", "\xC0r0p" },
	{ "o2_setZanthiaPos", "\xC0r0pp" },
	{ "o2_loadMusicTrack", "\xC0r0p" },
	// 0x58
	{ "o1_playWanderScoreViaMap", "\xC0r0pp" },
	{ "o1_playSoundEffect", "\xC0r0p" },
	{ "o2_setSceneAnimPos", "\xC0r0ppp" },
	{ "o1_blockInWalkableRegion", "\xC0r0pppp" },
	// 0x5c
	{ "o1_blockOutWalkableRegion", "\xC0r0pppp" },
	{ "unk5D", "\xC0r0ppppp" },
	{ "o2_setCauldronState", "\xC0r0pp" },
	{ "o2_showItemString", "\xC0r0pp" },
	// 0x60
	{ "o1_getRand", "\xC0r0pp" },
	{ "o2_isAnySoundPlaying", "\xC0r" },
	{ "o1_setDeathHandler", "\xC0r0p" },
	{ "o2_setDrawNoShapeFlag", "\xC0r0p" },
	// 0x64
	{ "o2_setRunFlag", "\xC0r0p" },
	{ "o2_showLetter", "\xC0r0p" },
	{ "o1_shakeScreen", "\xC0r0pp" },
	{ "o1_fillRect", "\xC0r0pppppp" },
	// 0x68
	{ "o2_getKey", "\xC0r" },
	{ "unk69", "\xC0r0pppp" },
	{ "o2_playFireflyScore", "\xC0r" },
	{ "o2_waitForConfirmationClick", "\xC0r0p" },
	// 0x6c
	{ "o2_encodeShape", "\xC0r0ppppp" },
	{ "o2_defineRoomEntrance", "\xC0r0ppp" },
	{ "o2_runAnimationScript", "\xC0r0sppp" },
	{ "o2_setSpecialSceneScriptRunTime", "\xC0r0pp" },
	// 0x70
	{ "o2_defineSceneAnim", "\xC0r0pppppppppppps" },
	{ "o2_updateSceneAnim", "\xC0r0pp" },
	{ "o2_updateSceneAnim", "\xC0r0pp" },
	{ "o2_addToSceneAnimPosAndUpdate", "\xC0r0ppp" },
	// 0x74
	{ "o2_useItemOnMainChar", "\xC0r" },
	{ "o2_startDialogue", "\xC0r0p" },
	{ "o2_randomSceneChat", "\xC0r" },
	{ "o2_setDlgIndex", "\xC0r0p" },
	// 0x78
	{ "o2_getDlgIndex", "\xC0r" },
	{ "o2_defineScene", "\xC0r0pspppppp" },
	{ "o2_addCauldronStateTableEntry", "\xC0r0pp" },
	{ "o2_setCountDown", "\xC0r0p" },
	// 0x7c
	{ "o2_getCountDown", "\xC0r" },
	{ "o2_dummy7D", "\xC0r" },
	{ "o2_dummy7E", "\xC0r" },
	{ "o2_pressColorKey", "\xC0r0p" },
	// 0x80
	{ "o2_objectChat", "\xC0r0sp" },
	{ "o2_changeChapter", "\xC0r0pp" },
	{ "o2_getColorCodeFlag1", "\xC0r" },
	{ "o2_setColorCodeFlag1", "\xC0r0p" },
	// 0x84
	{ "o2_getColorCodeFlag2", "\xC0r" },
	{ "o2_setColorCodeFlag2", "\xC0r0p" },
	{ "o2_getColorCodeValue", "\xC0r0p" },
	{ "o2_setColorCodeValue", "\xC0r0pp" },
	// 0x88
	{ "o2_countItemInstances", "\xC0r0p" },
	{ "o2_removeItemFromScene", "\xC0r0pp" },
	{ "o2_initObject", "\xC0r0p" },
	{ "o2_npcChat", "\xC0r0spp" }, // FIXME: Non-talkie metadata; talkie opcode is 0spp
	// 0x8c
	{ "o2_deinitObject", "\xC0r0p" },
	{ "o2_playTimSequence", "\xC0r0s" },
	{ "o2_makeBookOrCauldronAppear", "\xC0r0p" },
	{ "o2_setSpecialSceneScriptState", "\xC0r0p" },
	// 0x90
	{ "o2_clearSpecialSceneScriptState", "\xC0r0p" },
	{ "o2_querySpecialSceneScriptState", "\xC0r0p" },
	{ "o2_resetInputColorCode", "\xC0r" },
	{ "o2_setHiddenItemsEntry", "\xC0r0pp" },
	// 0x94
	{ "o2_getHiddenItemsEntry", "\xC0r0p" },
	{ "o2_mushroomEffect", "\xC0r" },
	{ "o2_wsaClose", "\xC0r0p" },
	{ "o2_meanWhileScene", "\xC0r0p" },
	// 0x98
	{ "o2_customChat", "\xC0r0spp" },
	{ "o2_customChatFinish", "\xC0r" },
	{ "o2_setupSceneAnimation", "\xC0r0pppppppppppps" },
	{ "o2_stopSceneAnimation", "\xC0r0pp" },
	// 0x9c
	{ "o2_disableTimer", "\xC0r0p" },
	{ "o2_enableTimer", "\xC0r0p" },
	{ "o2_setTimerCountdown", "\xC0r0pp" },
	{ "o2_processPaletteIndex", "\xC0r0pppppp" },
	// 0xa0
	{ "o2_updateTwoSceneAnims", "\xC0r0pppp" },
	{ "o2_getRainbowRoomData", "\xC0r0p" },
	{ "o2_drawSceneShapeEx", "\xC0r0pppp" },
	{ "o2_midiSoundFadeout", "\xC0r" },
	// 0xa4
	{ "o2_getSfxDriver", "\xC0r" },
	{ "o2_getVocSupport", "\xC0r" },
	{ "o2_getMusicDriver", "\xC0r" },
	{ "o2_setVocHigh", "\xC0r0p" },
	// 0xa8
	{ "o2_getVocHigh", "\xC0r" },
	{ "o2_zanthiaChat", "\xC0r0sp" },
	{ "o2_isVoiceEnabled", "\xC0r" },
	{ "o2_isVoicePlaying", "\xC0r" },
	// 0xac
	{ "o2_stopVoicePlaying", "\xC0r" },
	{ "o2_getGameLanguage", "\xC0r" },
	{ "o2_demoFinale", "\xC0r" },
	{ "o2_dummyAF", "\xC0r" }
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
			std::stringstream s;
			s << "\"" << posString(i) << "\"";
			_engine->_textStrings.push_back(s.str());
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
				OPCODE_MD("boolNegate", kUnaryOp, 0, false, false, "!");
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
	// We only have the entry points, but the end points are not known, so create placeholder function entries
	for (size_t i = 0; i < funcAddrs.size(); i++) {
		_engine->_functions[funcAddrs[i]] = Function(addrMap[funcAddrs[i]], addrMap[funcAddrs[i]]);
		_engine->_functions[funcAddrs[i]]._name = "global_";
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

