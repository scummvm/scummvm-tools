/* DeKyra - Basic Kyrandia script disassembler
 * Copyright (C) 2004  Johannes Schickel
 * Copyright (C) 2004-2006  The ScummVM Team
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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "dekyra.h"
#include "common/endian.h"
#include "common/file.h"
#include "common/util.h"

void printCommandsV1Ref();
void setupCommandsV1(Script *myScript);
void setupTraceCommandsV1(Script *myScript);

void getOpcodesV1(OpcodeEntry *&opcodes, int &opcodesSize);
void getOpcodesV2(OpcodeEntry *&opcodes, int &opcodesSize);

FILE *outputFile = NULL;

int main(int argc, char** argv) {
	if (argc < 2) {
		printf("\nUsage: %s <filename>\n", argv[0]);

		printf("\nParams:\n");
		printf("-t   displays only the text segment\n");
		printf("-e   set engine version (1 for kyra1 (default), 2 for kyra2)\n");
		printf("-o   set optional outputfilename (default: stdout)\n");

		return -1;
	}

	int file = -1;
	int outputfile = -1;
	bool displayText = false;
	int32 engine = 1;

	// search for some parameters
	for (int param = 1; param < argc; ++param) {
		if (*argv[param] != '-' && file == -1)
			file = param;
		else {
			if (argv[param][1] == 't') {
				displayText = true;
			} else if (argv[param][1] == 'e') {
				engine = atoi(argv[param+1]);
				++param;
			} else if (argv[param][1] == 'o' && outputfile == -1) {
				outputfile = ++param;
			}
		}
	}

	if (file == -1) {
		printf("Use:\n"
			   "%s filename\n"
			   "-t   displays only the text segment\n"
			   "-e   set engine version (1 for kyra1 (default), 2 for kyra2)\n"
			   "-o   set optional outputfilename (default: stdout)\n",
			   argv[0]);

		return -1;
	} else if (engine != 1 && engine != 2) {
		printf("-e (engine version) must be set to 1 or 2!\n");
		return -1;
	}

	// currently output goes to stdout
	if (outputfile == -1 || outputfile >= argc) {
		outputFile = stdout;
	} else {
		outputFile = fopen(argv[outputfile], "w");
	}

	Script myScript;
	ScriptData scriptData;
	memset(&scriptData, 0, sizeof(ScriptData));

	OpcodeEntry *opcodes;
	int opcodesSize;

	if (engine == 1) {
		getOpcodesV1(opcodes, opcodesSize);
	} else if (engine == 2) {
		getOpcodesV2(opcodes, opcodesSize);
	}

	if (!myScript.loadScript(argv[file], &scriptData, opcodes, opcodesSize)) {
		printf("ERROR: script loading failed!\n");
		return -1;
	}

	myScript.setEngineVersion(engine);
	if (!displayText) {
		if (engine == 1) {
			setupTraceCommandsV1(&myScript);
		} else if (engine == 2) {
			// TODO: maybe kyra2 has other commands?
			setupTraceCommandsV1(&myScript);
		}

		myScript.processScriptTrace(&scriptData);
	}

	fprintf(outputFile, "/-----------------------------------\\\n");
	fprintf(outputFile, "|                                   |\n");
	fprintf(outputFile, "|  Dekyra output for file:          |\n");
	fprintf(outputFile, "|  %-31s  |\n", argv[file]);
	fprintf(outputFile, "|-----------------------------------|\n");
	fprintf(outputFile, "|  General information:             |\n");
	fprintf(outputFile, "|  Engine version: %1d                |\n", engine);
	fprintf(outputFile, "|  Filesize: %15d bytes  |\n", scriptData.fileSize);
	if (scriptData.text) {
		fprintf(outputFile, "|-----------------------------------|\n");
		fprintf(outputFile, "|  TEXT chunk information:          |\n");
		fprintf(outputFile, "|  Strings: %-22d  |\n", scriptData.numStrings);
		fprintf(outputFile, "|  Text chunk size: %8d bytes  |\n", scriptData.textChunkSize);
	}
	if (!displayText) {
		fprintf(outputFile, "|-----------------------------------|\n");
		fprintf(outputFile, "|  ORDR chunk information:          |\n");
		fprintf(outputFile, "|  Ordr chunk size: %8d bytes  |\n", scriptData.ordrChunkSize);
		fprintf(outputFile, "|  Max scriptfunctions: %-10d  |\n", scriptData.ordrChunkSize / 2);
		fprintf(outputFile, "|  Valid scriptfunctions: %-8d  |\n", scriptData.validORDRFunctions);
		fprintf(outputFile, "|-----------------------------------|\n");
		fprintf(outputFile, "|  DATA chunk information:          |\n");
		fprintf(outputFile, "|  Code chunk size: %8d bytes  |\n", scriptData.dataChunkSize);
		fprintf(outputFile, "|  Detected functionchunks: %-6d  |\n", scriptData.numFunctions);
	}
	fprintf(outputFile, "|                                   |\n");
	fprintf(outputFile, "\\-----------------------------------/\n\n");

	myScript.setEngineVersion(engine);
	myScript.printTextArea(&scriptData, argv[file]);
	if (!displayText) {
		if (engine == 1) {
			printCommandsV1Ref();
			setupCommandsV1(&myScript);
		} else if (engine == 2) {
			// TODO: maybe kyra2 has other commands?
			printCommandsV1Ref();
			setupCommandsV1(&myScript);
		}

		myScript.decodeScript(&scriptData);
	}

	myScript.unloadScript(&scriptData);

	if (outputFile != stdout)
		fclose(outputFile);

	return 0;
}

Script::Script() : _commands(0), _commandsSize(0) {
}

Script::~Script() {
}

bool Script::setCommands(CommandProc *commands, int commandsSize) {
	_commands = commands;
	_commandsSize = commandsSize;
	return true;
}

bool Script::loadScript(const char *filename, ScriptData *scriptData, OpcodeEntry *opcodes, int opcodeSize) {
	Common::File scriptFile(filename, "rb");

	if (!scriptFile.isOpen()) {
		error("couldn't load file '%s'", filename);
		return false;
	}

	uint32 size = scriptFile.size();
	scriptData->fileSize = size;
	uint8 *data = new uint8[size];
	assert(data);
	if (size != scriptFile.read_noThrow(data, size)) {
		delete [] data;
		error("couldn't read all bytes from file '%s'", filename);
		return false;
	}
	scriptFile.close();

	byte *curData = data;

	uint32 formBlockSize = Script::getFORMBlockSize(curData);
	if (formBlockSize == (uint32)-1) {
		delete [] data;
		error("No FORM chunk found in file: '%s'", filename);
		return false;
	}

	uint32 chunkSize = Script::getIFFBlockSize(data, curData, size, TEXT_CHUNK);
	if (chunkSize != (uint32)-1) {
		scriptData->text = new byte[chunkSize];
		scriptData->textChunkSize = chunkSize;
		assert(scriptData->text);

		if (!Script::loadIFFBlock(data, curData, size, TEXT_CHUNK, scriptData->text, chunkSize)) {
			delete [] data;
			unloadScript(scriptData);
			error("Couldn't load TEXT chunk from file: '%s'", filename);
			return false;
		}

		uint16 minTextOffset = 0xFFFF;
		for (int i = 0; i < scriptData->textChunkSize / 2; ++i, ++scriptData->numStrings) {
			if (minTextOffset > READ_BE_UINT16(&((uint16 *)scriptData->text)[i])) {
				minTextOffset = READ_BE_UINT16(&((uint16 *)scriptData->text)[i]);
			}
			if (minTextOffset <= i*2)
				break;
		}
	}

	chunkSize = Script::getIFFBlockSize(data, curData, size, ORDR_CHUNK);
	if (chunkSize == (uint32)-1) {
		delete [] data;
		unloadScript(scriptData);
		error("No ORDR chunk found in file: '%s'", filename);
		return false;
	}

	scriptData->ordr = new byte[chunkSize];
	scriptData->ordrChunkSize = chunkSize;
	assert(scriptData->ordr);

	if (!Script::loadIFFBlock(data, curData, size, ORDR_CHUNK, scriptData->ordr, chunkSize)) {
		delete [] data;
		unloadScript(scriptData);
		error("Couldn't load ORDR chunk from file: '%s'", filename);
		return false;
	}
	chunkSize = chunkSize / 2;
	while (chunkSize--) {
		((uint16*)scriptData->ordr)[chunkSize] = READ_BE_UINT16(&((uint16*)scriptData->ordr)[chunkSize]);
	}

	chunkSize = Script::getIFFBlockSize(data, curData, size, DATA_CHUNK);
	if (chunkSize == (uint32)-1) {
		delete [] data;
		unloadScript(scriptData);
		error("No DATA chunk found in file: '%s'", filename);
		return false;
	}

	scriptData->data = new byte[chunkSize];
	assert(scriptData->data);

	if (!Script::loadIFFBlock(data, curData, size, DATA_CHUNK, scriptData->data, chunkSize)) {
		delete [] data;
		unloadScript(scriptData);
		error("Couldn't load DATA chunk from file: '%s'", filename);
		return false;
	}

	scriptData->dataChunkSize = chunkSize;
	scriptData->opcodes = opcodes;
	scriptData->opcodeSize = opcodeSize;

	delete [] data;
	return true;
}

void Script::unloadScript(ScriptData *data) {
	if (!data)
		return;

	delete [] data->text;
	delete [] data->ordr;
	delete [] data->data;
	data->text = data->ordr = data->data = 0;
}

// decoding
void Script::printTextArea(ScriptData *dataPtr, const char *filename) {
	if (!dataPtr->textChunkSize)
		return;

#define posString(x) (char*)&dataPtr->text[READ_BE_UINT16(&((uint16 *)dataPtr->text)[(x)])]
	fprintf(outputFile, "------------ Text Segment of '%s' --------------\n\n", filename);
	for (int i = 0; i < dataPtr->numStrings; ++i) {
		fprintf(outputFile, "0x%.04X '%s'\n", i, posString(i));
	}
	fprintf(outputFile, "\n");
#undef posString
}

void Script::processScriptTrace(ScriptData *dataPtr) {
	uint8 *ip = dataPtr->data;

	for (int i = 0; i < dataPtr->ordrChunkSize / 2; ++i) {
		if (dataPtr->numFunctions < MAX_FUNCTIONS) {
			dataPtr->functions[dataPtr->numFunctions].id = i;
			dataPtr->functions[dataPtr->numFunctions].startOffset = (((uint16*)dataPtr->ordr)[i]);
			if (dataPtr->functions[dataPtr->numFunctions].startOffset != (uint16)-1) {
				++dataPtr->validORDRFunctions;
				dataPtr->functions[dataPtr->numFunctions].startOffset <<= 1;
				++dataPtr->numFunctions;
			}
		} else {
			warning("losing function");
		}
	}

	for (; ip && ip < (dataPtr->data + dataPtr->dataChunkSize);) {
		dataPtr->curOffset = (uint16)(ip - dataPtr->data);
		int16 parameter = 0;
		int16 code = READ_BE_UINT16(ip); ip += 2;
		int16 opcode = (code >> 8) & 0x1F;

		if (code & 0x8000) {
			opcode = 0;
			parameter = code & 0x7FFF;
		} else if (code & 0x4000) {
			parameter = (int8)(code);
		} else if (code & 0x2000) {
			parameter = READ_BE_UINT16(ip); ip += 2;
		} else {
			parameter = 0;
		}

		if (opcode < _commandsSize) {
			if (_commands[(uint)opcode])
				_commands[(uint)opcode](dataPtr, parameter);
		}
	}

	// TODO: sort the 'function' list (except for the functions with id != -1) after start address
}

int Script::findFunction(ScriptData *dataPtr, uint16 offset) {
	int bestStartOffset = -1;
	int functionFittingBest = -1;

	for (int i = 0; i < dataPtr->numFunctions; ++i) {
		if (dataPtr->functions[i].startOffset == offset)
			return i;

		int temp = (dataPtr->functions[i].startOffset < offset) ? dataPtr->functions[i].startOffset : offset;
		if (temp == offset)
			continue;

		int temp2 = (bestStartOffset > temp) ? bestStartOffset : temp;
		if (temp2 == bestStartOffset)
			continue;

		bestStartOffset = temp2;
		functionFittingBest = i;
	}

	return functionFittingBest;
}

void Script::outputFunctionInfo(ScriptData *dataPtr, uint16 curOffset, bool list) {
	for (int i = 0; i < dataPtr->numFunctions; ++i) {
		if (dataPtr->functions[i].startOffset != curOffset && !list)
			continue;

		if (!list) {
			fprintf(outputFile, "\n------------------------------------------------\n");
			fprintf(outputFile, "Function(chunk) start: ");
		}

		fprintf(outputFile, "num: %d, ID: %d startOffset: 0x%.04X\n", i, dataPtr->functions[i].id, dataPtr->functions[i].startOffset);
		if (dataPtr->functions[i].refs) {
			fprintf(outputFile, "refs:\n");
			for (int i2 = 0; i2 < dataPtr->functions[i].refs; ++i2) {
				uint16 tmpOffset = dataPtr->functions[i].refOffs[i2];
				fprintf(outputFile, "0x%.04X (funcnum: %d) ", dataPtr->functions[i].refOffs[i2], findFunction(dataPtr, tmpOffset));
				if ((i2 % 3) == 2)
					fprintf(outputFile, "\n");
			}
			if (((dataPtr->functions[i].refs - 1)% 3) != 2)
				fprintf(outputFile, "\n");
		}

		if (!list) {
			fprintf(outputFile, "------------------------------------------------\n\n");
			break;
		} else if (i + 1 != dataPtr->numFunctions)  {
			fprintf(outputFile, "------------------------------------------------\n");
		}
	}

	if (list)
		fprintf(outputFile, "\n");
}

void Script::decodeScript(ScriptData *dataPtr) {
	uint8 *ip = dataPtr->data;

	fprintf(outputFile, "\n");
	fprintf(outputFile, "---------- scriptfunction list ----------------\n\n");
	for (int i = 0; i < dataPtr->ordrChunkSize / 2; ++i) {
		uint16 offset = ((uint16*)dataPtr->ordr)[i];
		if (offset != (uint16)-1)
			fprintf(outputFile, "Scriptfunction %d starts at 0x%.04X\n", i, offset << 1);
	}
	fprintf(outputFile, "\n");

	fprintf(outputFile, "--------- detected functionchunk list ----------\n\n");
	outputFunctionInfo(dataPtr, 0, true);

	fprintf(outputFile, "---------- 'disassembled' output ---------------\n");
	for (; ip < (dataPtr->data + dataPtr->dataChunkSize);) {
		uint16 curOffset = (uint16)(ip - dataPtr->data);

		outputFunctionInfo(dataPtr, curOffset);

		fprintf(outputFile, "0x%.04X: ", curOffset);

		int16 parameter = 0;
		int16 code = READ_BE_UINT16(ip); ip += 2;
		int16 opcode = (code >> 8) & 0x1F;

		if (code & 0x8000) {
			opcode = 0;
			parameter = code & 0x7FFF;
		} else if (code & 0x4000) {
			parameter = (int8)(code);
		} else if (code & 0x2000) {
			parameter = READ_BE_UINT16(ip); ip += 2;
		} else {
			parameter = 0;
		}

		fprintf(outputFile, "0x%.02X ", opcode);
		if (opcode >= _commandsSize) {
			fprintf(outputFile, "unknown command\n");
		} else {
			if (_commands[(uint)opcode]) {
				_commands[(uint)opcode](dataPtr, parameter);
			} else {
				fprintf(outputFile, "no valid command\n");
			}
		}
	}
}

// block loading
uint32 Script::getFORMBlockSize(byte *&data) {
	static const uint32 chunkName = FORM_CHUNK;
	if (READ_LE_UINT32(data) != chunkName) {
		return (uint32)-1;
	}
	data += 4;
	uint32 retValue = READ_BE_UINT32(data); data += 4;
	return retValue;
}

uint32 Script::getIFFBlockSize(byte *start, byte *&data, uint32 maxSize, const uint32 chunkName) {
	uint32 size = (uint32)-1;
	bool special = false;

	if (data == (start + maxSize)) {
		data = start + 0x0C;
	}
	while (data < (start + maxSize)) {
		uint32 chunk = READ_LE_UINT32(data); data += 4;
		uint32 size_temp = READ_BE_UINT32(data); data += 4;
		if (chunk != chunkName) {
			if (special) {
				data += (size_temp + 1) & 0xFFFFFFFE;
			} else {
				data = start + 0x0C;
				special = true;
			}
		} else {
			// kill our data
			data = start;
			size = size_temp;
			break;
		}
	}
	return size;
}

bool Script::loadIFFBlock(byte *start, byte *&data, uint32 maxSize, const uint32 chunkName, byte *loadTo, uint32 ptrSize) {
	bool special = false;

	if (data == (start + maxSize)) {
		data = start + 0x0C;
	}
	while (data < (start + maxSize)) {
		uint32 chunk = READ_LE_UINT32(data); data += 4;
		uint32 chunkSize = READ_BE_UINT32(data); data += 4;
		if (chunk != chunkName) {
			if (special) {
				data += (chunkSize + 1) & 0xFFFFFFFE;
			} else {
				data = start + 0x0C;
				special = true;
			}
		} else {
			uint32 loadSize = 0;
			if (chunkSize < ptrSize)
				loadSize = chunkSize;
			else
				loadSize = ptrSize;
			memcpy(loadTo, data, loadSize);
			chunkSize = (chunkSize + 1) & 0xFFFFFFFE;
			if (chunkSize > loadSize) {
				data += (chunkSize - loadSize);
			}
			return true;
		}
	}
	return false;
}

// TODO: move to own file dekyra_v2.cpp if there are special kyra2 commands!
void getOpcodesV2(OpcodeEntry *&opcodes, int &opcodesSize) {
	static OpcodeEntry kyra2OpcodeDesc[] = {
		{ 0x0, "o2_setCharacterShape" },
		{ 0x1, "o2_drawShapeFrame" },
		{ 0x2, "o2_defineObject" },
		{ 0x3, "o2_setCharacterFrame" },
		{ 0x4, "o2_getCharacterPosX" },
		{ 0x5, "o2_getCharacterPosY" },
		{ 0x6, "o2_getCharacterField5" },
		{ 0x7, "o2_getCharacterSceneId" },
		{ 0x8, "o2_setSceneCommentString" },
		{ 0x9, "o2_isBitSet" },
		{ 0xa, "o2_setBit" },
		{ 0xb, "o2_setCharacterField6" },
		{ 0xc, "o2_setCharacterField5" },
		{ 0xd, "o2_restartToChapter1?" },
		{ 0xe, "o2_changeCharacterDirection" },
		{ 0xf, "o2_unk0x0F" },
		{ 0x10, "o2_unk0x10" },
		{ 0x11, "o2_setupNewChapter" },
		{ 0x12, "o2_copyScreenPage" },
		{ 0x13, "o2_printText" },
		{ 0x14, "o2_closeWSAFile" },
		{ 0x15, "o2_copyPage" },
		{ 0x16, "o2_copyScreenPageAlt" },
		{ 0x17, "o2_displayWSASequentialFrames" },
		{ 0x18, "o2_runWSAFrames" },
		{ 0x19, "o2_openWSAFile" },
		{ 0x1a, "o2_displayWSAFrame" },
		{ 0x1b, "o2_runWSAFromBeginningToEnd" },
		{ 0x1c, "o2_addItemToInventory" },
		{ 0x1d, "o2_animateShape" },
		{ 0x1e, "o2_unk0x1E" },
		{ 0x1f, "o2_setMouseBounds" },
		{ 0x20, "o2_unk0x20" },
		{ 0x21, "o2_dummy" },
		{ 0x22, "o2_removeItemFromInventory" },
		{ 0x23, "o2_defineBox?" },
		{ 0x24, "o2_reorderInventory" },
		{ 0x25, "o2_specificItemInInventory" },
		{ 0x26, "o2_getBox" },
		{ 0x27, "o2_queryGameState" },
		{ 0x28, "o2_clearGameState" },
		{ 0x29, "o2_setGameState" },
		{ 0x2a, "o2_createMouseItem" },
		{ 0x2b, "o2_destroyMouseItem" },
		{ 0x2c, "o2_mouseIsPointer" },
		{ 0x2d, "o2_hideMouse" },
		{ 0x2e, "o2_defineRoomDoor" },
		{ 0x2f, "o2_setMousePos" },
		{ 0x30, "o2_showMouse" },
		{ 0x31, "o2_scrollScreen?" },
		{ 0x32, "o2_wipeDownMouseItem" },
		{ 0x33, "o2_getElapsedSeconds" },
		{ 0x34, "o2_getTimerData" },
		{ 0x35, "o2_delayIf6or8" },
		{ 0x36, "o2_delay" },
		{ 0x37, "o2_delayWithOptionalInput" },
		{ 0x38, "o2_setTimerDataTicks" },
		{ 0x39, "o2_setTimerData" },
		{ 0x3a, "o2_setScaleSlot" },
		{ 0x3b, "o2_setDepthSlot" },
		{ 0x3c, "o2_setRoomHotSpotX" },
		{ 0x3d, "o2_loadShape" },
		{ 0x3e, "o2_drawShapeToScreenAndAnimate" },
		{ 0x3f, "o2_drawShapeToScreen" },
		{ 0x40, "o2_unsetSpriteFlag" },
		{ 0x41, "o2_setSpriteFlag" },
		{ 0x42, "o2_dummy" },
		{ 0x43, "o2_loadPalette" },
		{ 0x44, "o2_fadePalette" },
		{ 0x45, "o2_copyDisplayedPage" },
		{ 0x46, "o2_cmd_copyWSARegion" },
		{ 0x47, "o2_unsetSceneFlagBit1" },
		{ 0x48, "o2_setSceneFlagBit1" },
		{ 0x49, "o2_copyScreenPage2" },
		{ 0x4a, "o2_setSceneOffsets" },
		{ 0x4b, "o2_delayCount" },
		{ 0x4c, "o2_changePalette" },
		{ 0x4d, "o2_setCustomPaletteRange" },
		{ 0x4e, "o2_dummy" },
		{ 0x4f, "o2_dummy" },
		{ 0x50, "o2_unk0x50" },
		{ 0x51, "o2_enterNewScene" },
		{ 0x52, "o2_isBlockedOutRegion" },
		{ 0x53, "o2_setScaleMode" },
		{ 0x54, "o2_getRoomExit" },
		{ 0x55, "o2_setRoomHotSpotY" },
		{ 0x56, "o2_setCharacterField6FromPosition" },
		{ 0x57, "o2_dummy" },
		{ 0x58, "o2_unk0x58" },
		{ 0x59, "o2_unk0x59" },
		{ 0x5a, "o2_setSceneUnkData" },
		{ 0x5b, "o2_blockInWalkableRegion" },
		{ 0x5c, "o2_blockOutWalkableRegion" },
		{ 0x5d, "o2_unk0x5D" },
		{ 0x5e, "o2_unk0x5E" },
		{ 0x5f, "o2_unk0x5F" },
		{ 0x60, "o2_random" },
		{ 0x61, "o2_unk0x61" },
		{ 0x62, "o2_freezeMouse" },
		{ 0x63, "o2_setShapeScaledFlag" },
		{ 0x64, "o2_setQuitFlag" },
		{ 0x65, "o2_unk0x65" },
		{ 0x66, "o2_shakeScreen" },
		{ 0x67, "o2_fillRect" },
		{ 0x68, "o2_getKey" },
		{ 0x69, "o2_printTextInBox" },
		{ 0x6a, "o2_dummy" },
		{ 0x6b, "o2_waitForConfirmationMouseClick" },
		{ 0x6c, "o2_swapScreenPages" },
		{ 0x6d, "o2_setSceneDimensions" },
		{ 0x6e, "o2_unk0x6E" },
		{ 0x6f, "o2_setTimer" },
		{ 0x70, "o2_loadSceneDataAlt" },
		{ 0x71, "o2_unk0x71" },
		{ 0x72, "o2_defineSprite" },
		{ 0x73, "o2_scrollScene?" },
		{ 0x74, "o2_runNPCSubscript" },
		{ 0x75, "o2_unk0x75" },
		{ 0x76, "o2_unk0x76" },
		{ 0x77, "o2_setCharacterField2" },
		{ 0x78, "o2_getCharacterField2" },
		{ 0x79, "o2_defineRoom" },
		{ 0x7a, "o2_unk0x7A" },
		{ 0x7b, "o2_setUnkGameFlag0" },
		{ 0x7c, "o2_getUnkGameFlag0" },
		{ 0x7d, "o2_dummy" },
		{ 0x7e, "o2_dummy" },
		{ 0x7f, "o2_unk0x7F" },
		{ 0x80, "o2_unk0x80" },
		{ 0x81, "o2_startNewChapter" },
		{ 0x82, "o2_getUnkGameFlag1" },
		{ 0x83, "o2_setUnkGameFlag1" },
		{ 0x84, "o2_getUnkGameFlag2" },
		{ 0x85, "o2_setUnkGameFlag2" },
		{ 0x86, "o2_getUnkGameFlag3" },
		{ 0x87, "o2_setUnkGameFlag3" },
		{ 0x88, "o2_unk0x88" },
		{ 0x89, "o2_unk0x89" },
		{ 0x8a, "o2_loadSpriteTimerData" },
		{ 0x8b, "o2_unk0x8B" },
		{ 0x8c, "o2_unk0x8C" },
		{ 0x8d, "o2_unk0x8D" },
		{ 0x8e, "o2_unk0x8E" },
		{ 0x8f, "o2_setTimerFlag" },
		{ 0x90, "o2_unsetTimerFlag" },
		{ 0x91, "o2_getTimerFlag" },
		{ 0x92, "o2_unk0x92" },
		{ 0x93, "o2_unk0x93" },
		{ 0x94, "o2_unk0x94" },
		{ 0x95, "o2_processPalette" },
		{ 0x96, "o2_closeWSAFile" },
		{ 0x97, "o2_playMeanwhileScene" },
		{ 0x98, "o2_unk0x98" },
		{ 0x99, "o2_unk0x99" },
		{ 0x9a, "o2_loadSceneData" },
		{ 0x9b, "o2_unk0x9B" },
		{ 0x9c, "o2_unk0x9C" },
		{ 0x9d, "o2_unk0x9D" },
		{ 0x9e, "o2_setTimerDataAndUpdateTimings" },
		{ 0x9f, "o2_setPaletteColor" },
		{ 0xa0, "o2_unk0xA0" },
		{ 0xa1, "o2_getStateData" },
		{ 0xa2, "o2_unk0xA2" },
		{ 0xa3, "o2_ifNotZero" },
		{ 0xa4, "o2_getDelayMode" },
		{ 0xa5, "o2_true" },
		{ 0xa6, "o2_unk0xA6" },
		{ 0xa7, "o2_dummy" }
	};

	opcodes = kyra2OpcodeDesc;
	opcodesSize = ARRAYSIZE(kyra2OpcodeDesc);
}
