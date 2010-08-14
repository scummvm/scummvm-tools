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

#include <boost/format.hpp>

FunctionData::FunctionData(std::string name, std::string metadata)
{
	_name = name;
	_metadata = metadata;
}

void Kyra::Kyra2Disassembler::setupKyra2Funcs() {
	_funcCount = 176;
	_funcs = new FunctionData[_funcCount];
	int i = 0;
#define FUNC(name, metadata) _funcs[i++] = FunctionData(name, metadata);
	// 0x00
	FUNC("o2_setCharacterFacingRefresh", "rppp");
	FUNC("o2_setCharacterPos", "rppp");
	FUNC("o2_defineObject", "rpspppp");
	FUNC("o2_refreshCharacter", "rpppp");
	// 0x04
	FUNC("o2_getCharacterX", "r");
	FUNC("o2_getCharacterY", "r");
	FUNC("o2_getCharacterFacing", "r");
	FUNC("o2_getCharacterScene", "r");
	// 0x08
	FUNC("o2_setSceneComment", "rs");
	FUNC("unk09", "rp");
	FUNC("unk0A", "rp");
	FUNC("o2_setCharacterAnimFrame", "rppp");
	// 0x0c
	FUNC("o2_setCharacterFacingOverwrite", "rp");
	FUNC("o2_trySceneChange", "rpppp");
	FUNC("o2_moveCharacter", "rppp");
	FUNC("o2_customCharacterChat", "rspppp");
	// 0x10
	FUNC("o2_soundFadeOut", "r");
	FUNC("o2_showChapterMessage", "rpp");
	FUNC("o2_restoreTalkTextMessageBkgd", "r");
	FUNC("o2_printString", "rspppp");
	// 0x14
	FUNC("o2_wsaClose", "rp");
	FUNC("o2_backUpScreen", "rp");
	FUNC("o2_restoreScreen", "rp");
	FUNC("o2_displayWsaFrame", "rppppppppp");
	// 0x18
	FUNC("o2_displayWsaSequentialFramesLooping", "rpppppppp");
	FUNC("o2_wsaOpen", "rsp");
	FUNC("o2_displayWsaSequentialFrames", "rppppppp");
	FUNC("o2_displayWsaSequence", "rpppppp");
	// 0x1c
	FUNC("o2_addItemToInventory", "rppp");
	FUNC("o2_drawShape", "rppppp");
	FUNC("o2_addItemToCurScene", "rppp");
	FUNC("o2_limitMouseRange", "rpppp");
	// 0x20
	FUNC("o2_checkForItem", "rpp");
	FUNC("o2_loadSoundFile", "rp");
	FUNC("o2_removeSlotFromInventory", "rp");
	FUNC("o2_defineItem", "rpppp");
	// 0x24
	FUNC("o2_removeItemFromInventory", "rp");
	FUNC("o2_countItemInInventory", "rpp");
	FUNC("o2_countItemsInScene", "rp");
	FUNC("o1_queryGameFlag", "rp");
	// 0x28
	FUNC("o1_resetGameFlag", "rp");
	FUNC("o1_setGameFlag", "rp");
	FUNC("o1_setHandItem", "rp");
	FUNC("o1_removeHandItem", "r");
	// 0x2c
	FUNC("o1_getMouseState", "r");
	FUNC("o1_hideMouse", "r");
	FUNC("o2_addSpecialExit", "rppppp");
	FUNC("o1_setMousePos", "rpp");
	// 0x30
	FUNC("o1_showMouse", "r");
	FUNC("o2_drawBox", "rppppp");
	FUNC("o2_wipeDownMouseItem", "rppp");
	FUNC("o2_getElapsedSecs", "r");
	// 0x34
	FUNC("o2_getTimerDelay", "rp");
	FUNC("o1_playSoundEffect", "rp");
	FUNC("o2_delaySecs", "rp");
	FUNC("o2_delay", "rpp");
	// 0x38
	FUNC("o2_dummy38", "r");
	FUNC("o2_setTimerDelay", "rpp");
	FUNC("o2_setScaleTableItem", "rpp");
	FUNC("o2_setDrawLayerTableItem", "rpp");
	// 0x3c
	FUNC("o2_setCharPalEntry", "rpp");
	FUNC("o2_loadZShapes", "rp");
	FUNC("o2_drawSceneShape", "rpppp");
	FUNC("o2_drawSceneShapeOnPage", "rppppp");
	// 0x40
	FUNC("o2_disableAnimObject", "rp");
	FUNC("o2_enableAnimObject", "rp");
	FUNC("o2_dummy42", "r");
	FUNC("o2_loadPalette384", "rs");
	// 0x44
	FUNC("o2_setPalette384", "r");
	FUNC("o2_restoreBackBuffer", "rp");
	FUNC("o2_backUpInventoryGfx", "r");
	FUNC("o2_disableSceneAnim", "rp");
	// 0x48
	FUNC("o2_enableSceneAnim", "rp");
	FUNC("o2_restoreInventoryGfx", "r");
	FUNC("o2_setSceneAnimPos2", "rppp");
	FUNC("o2_update", "rp");
	// 0x4c
	FUNC("unk4C_palFade?", "rpp");
	FUNC("o2_fadeScenePal", "rpp");
	FUNC("o2_dummy4E", "r");
	FUNC("o2_dummy4F", "r");
	// 0x50
	FUNC("o2_enterNewScene", "rppppp");
	FUNC("o2_switchScene", "rp");
	FUNC("o2_getShapeFlag1", "rpp");
	FUNC("o2_setPathfinderFlag", "rp");
	// 0x54
	FUNC("o2_getSceneExitToFacing", "rpp");
	FUNC("o2_setLayerFlag", "rp");
	FUNC("o2_setZanthiaPos", "rpp");
	FUNC("o2_loadMusicTrack", "rp");
	// 0x58
	FUNC("o1_playWanderScoreViaMap", "rpp");
	FUNC("o1_playSoundEffect", "rp");
	FUNC("o2_setSceneAnimPos", "rppp");
	FUNC("o1_blockInWalkableRegion", "rpppp");
	// 0x5c
	FUNC("o1_blockOutWalkableRegion", "rpppp");
	FUNC("unk5D", "rppppp");
	FUNC("o2_setCauldronState", "rpp");
	FUNC("o2_showItemString", "rpp");
	// 0x60
	FUNC("o1_getRand", "rpp");
	FUNC("o2_isAnySoundPlaying", "r");
	FUNC("o1_setDeathHandler", "rp");
	FUNC("o2_setDrawNoShapeFlag", "rp");
	// 0x64
	FUNC("o2_setRunFlag", "rp");
	FUNC("o2_showLetter", "rp");
	FUNC("o1_shakeScreen", "rpp");
	FUNC("o1_fillRect", "rpppppp");
	// 0x68
	FUNC("o2_getKey", "r");
	FUNC("unk69", "rpppp");
	FUNC("o2_playFireflyScore", "r");
	FUNC("o2_waitForConfirmationClick", "rp");
	// 0x6c
	FUNC("o2_encodeShape", "rppppp");
	FUNC("o2_defineRoomEntrance", "rppp");
	FUNC("o2_runAnimationScript", "rsppp");
	FUNC("o2_setSpecialSceneScriptRunTime", "rpp");
	// 0x70
	FUNC("o2_defineSceneAnim", "rpppppppppppps");
	FUNC("o2_updateSceneAnim", "rpp");
	FUNC("o2_updateSceneAnim", "rpp");
	FUNC("o2_addToSceneAnimPosAndUpdate", "rppp");
	// 0x74
	FUNC("o2_useItemOnMainChar", "r");
	FUNC("o2_startDialogue", "rp");
	FUNC("o2_randomSceneChat", "r");
	FUNC("o2_setDlgIndex", "rp");
	// 0x78
	FUNC("o2_getDlgIndex", "r");
	FUNC("o2_defineScene", "rpspppppp");
	FUNC("o2_addCauldronStateTableEntry", "rpp");
	FUNC("o2_setCountDown", "rp");
	// 0x7c
	FUNC("o2_getCountDown", "r");
	FUNC("o2_dummy7D", "r");
	FUNC("o2_dummy7E", "r");
	FUNC("o2_pressColorKey", "rp");
	// 0x80
	FUNC("o2_objectChat", "rsp");
	FUNC("o2_changeChapter", "rpp");
	FUNC("o2_getColorCodeFlag1", "r");
	FUNC("o2_setColorCodeFlag1", "rp");
	// 0x84
	FUNC("o2_getColorCodeFlag2", "r");
	FUNC("o2_setColorCodeFlag2", "rp");
	FUNC("o2_getColorCodeValue", "rp");
	FUNC("o2_setColorCodeValue", "rpp");
	// 0x88
	FUNC("o2_countItemInstances", "rp");
	FUNC("o2_removeItemFromScene", "rpp");
	FUNC("o2_initObject", "rp");
	FUNC("o2_npcChat", (_engine->_isTalkie ? "rspp": "rsp"));
	// 0x8c
	FUNC("o2_deinitObject", "rp");
	FUNC("o2_playTimSequence", "rs");
	FUNC("o2_makeBookOrCauldronAppear", "rp");
	FUNC("o2_setSpecialSceneScriptState", "rp");
	// 0x90
	FUNC("o2_clearSpecialSceneScriptState", "rp");
	FUNC("o2_querySpecialSceneScriptState", "rp");
	FUNC("o2_resetInputColorCode", "r");
	FUNC("o2_setHiddenItemsEntry", "rpp");
	// 0x94
	FUNC("o2_getHiddenItemsEntry", "rp");
	FUNC("o2_mushroomEffect", "r");
	FUNC("o2_wsaClose", "rp");
	FUNC("o2_meanWhileScene", "rp");
	// 0x98
	FUNC("o2_customChat", "rspp");
	FUNC("o2_customChatFinish", "r");
	FUNC("o2_setupSceneAnimation", "rpppppppppppps");
	FUNC("o2_stopSceneAnimation", "rpp");
	// 0x9c
	FUNC("o2_disableTimer", "rp");
	FUNC("o2_enableTimer", "rp");
	FUNC("o2_setTimerCountdown", "rpp");
	FUNC("o2_processPaletteIndex", "rpppppp");
	// 0xa0
	FUNC("o2_updateTwoSceneAnims", "rpppp");
	FUNC("o2_getRainbowRoomData", "rp");
	FUNC("o2_drawSceneShapeEx", "rpppp");
	FUNC("o2_midiSoundFadeout", "r");
	// 0xa4
	FUNC("o2_getSfxDriver", "r");
	FUNC("o2_getVocSupport", "r");
	FUNC("o2_getMusicDriver", "r");
	FUNC("o2_setVocHigh", "rp");
	// 0xa8
	FUNC("o2_getVocHigh", "r");
	FUNC("o2_zanthiaChat", "rsp");
	FUNC("o2_isVoiceEnabled", "r");
	FUNC("o2_isVoicePlaying", "r");
	// 0xac
	FUNC("o2_stopVoicePlaying", "r");
	FUNC("o2_getGameLanguage", "r");
	FUNC("o2_demoFinale", "r");
	FUNC("o2_dummyAF", "r" );
#undef FUNC
}

IFFChunk::IFFChunk() {
	_size = 0;
	_data = NULL;
}

Kyra::Kyra2Disassembler::Kyra2Disassembler(Kyra2Engine *engine, std::vector<Instruction> &insts) : Disassembler(insts), _engine(engine) {
	setupKyra2Funcs();
}

Kyra::Kyra2Disassembler::~Kyra2Disassembler() {
	if (_textChunk._data)
		delete[] _textChunk._data;
	if (_ordrChunk._data)
		delete[] _ordrChunk._data;
	if (_dataChunk._data)
		delete[] _dataChunk._data;
	delete[] _funcs;
}

void Kyra::Kyra2Disassembler::doDisassemble() throw(std::exception) {
	// Load data
	IFF_ID id;
	id = _f.readUint32BE();
	if (id != MKID_BE('FORM')) {
		std::stringstream s;
		s << boost::format("Unexpected IFF magic number 0x%08X (expected 0x%08X)!") % id % MKID_BE('FORM');
		throw std::runtime_error(s.str());
	}
	_f.readUint32BE(); // Skip file length
	_formType = _f.readUint32BE();
	if (_formType != MKID_BE('EMC2')) {
		std::stringstream s;
		s << boost::format("Unexpected file type 0x%08X (expected 0x%08X)!") % _formType % MKID_BE('EMC2');
		throw std::runtime_error(s.str());
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
			std::stringstream s;
			s << boost::format("Unexpected chunk type 0x%08X!") % temp._chunkType;
			delete [] temp._data;
			throw std::runtime_error(s.str());
		}
		if (temp._size % 2 != 0) // Skip padding byte
			_f.readByte();
	} while (_f.pos() != (int)_f.size());

	if (_ordrChunk._data == NULL)
		throw std::runtime_error("Missing ORDR chunk");
	if (_dataChunk._data == NULL)
		throw std::runtime_error("Missing DATA chunk");

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
				OPCODE("pushRet", kLoad, 1, false, false);
			} else if (parameter == 1) {
				OPCODE("pushPos", kSpecial, 0, false, false); // Sets up function call
			} else {
				// Error: invalid parameter halts execution
				throw UnknownOpcodeException(address, opcode);
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
				OPCODE("popRet", kStore, -1, false, false);
			} else if (parameter == 1) {
				OPCODE("popPos", kReturn, 0, false, false); // Returns from function call
			} else {
				// Error: invalid parameter halts execution
				throw UnknownOpcodeException(address, opcode);
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
			if ((uint16)parameter >= _funcCount || _funcs[parameter]._name.length() == 0) {
				// Error: unknown function
				throw UnknownOpcodeException(address, opcode);
			}
			OPCODE_MD(_funcs[parameter]._name, kSpecial, 0, false, false, _funcs[parameter]._metadata)
			break;
		case 15:
			parameter *= 2;
			if (parameter < minFuncAddr)
				jumpTargets.insert(_insts.size());
			OPCODE("ifNotJmp", kCondJump, -1, true, false);
			break;
		case 16:
			if (parameter == 0) {
				OPCODE_MD("boolNegate", kUnaryOpPre, 0, false, false, "!");
			} else if (parameter == 1) {
				OPCODE_MD("arithmeticNegate", kUnaryOpPre, 0, false, false,"-");
			} else if (parameter == 2) {
				OPCODE_MD("bitwiseNegate", kUnaryOpPre, 0, false, false, "~");
			} else {
				// Error: invalid parameter halts execution
				throw UnknownOpcodeException(address, opcode);
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
					OPCODE_MD("eval_eq", kBinaryOp, -1, false, false, "==");
					break;
				case 3:
					OPCODE_MD("eval_neq", kBinaryOp, -1, false, false, "!=");
					break;
				case 4:
					OPCODE_MD("eval_leq", kBinaryOp, -1, false, false, "<=");
					break;
				case 5:
					OPCODE_MD("eval_lt", kBinaryOp, -1, false, false, "<");
					break;
				case 6:
					OPCODE_MD("eval_geq", kBinaryOp, -1, false, false, ">=");
					break;
				case 7:
					OPCODE_MD("eval_gt", kBinaryOp, -1, false, false, ">");
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
					// Error: invalid parameter halts execution
					throw UnknownOpcodeException(address, opcode);
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

