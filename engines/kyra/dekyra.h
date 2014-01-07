/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Basic Kyrandia script disassembler */

#ifndef DEKYRA_H
#define DEKYRA_H

#include "common/scummsys.h"

typedef unsigned int uint;

struct OpcodeEntry {
	uint16 opcode;
	const char *name;
};

#define MAX_REFS 30
struct Function {
	int id;
	uint16 startOffset;

	int refs;
	uint16 refOffs[MAX_REFS];
};

#define MAX_FUNCTIONS 400

struct ScriptData {
	int fileSize;

	byte *text;
	int numStrings;
	int textChunkSize;

	byte *data;
	int dataChunkSize;

	byte *ordr;
	int validORDRFunctions;
	int ordrChunkSize;

	OpcodeEntry *opcodes;
	int opcodeSize;

	// trace information
	uint16 curOffset;

	int numFunctions;
	Function functions[MAX_FUNCTIONS];

	Function *getFunction(uint16 startOff) {
		for (int i = 0; i < numFunctions; ++i) {
			if (functions[i].startOffset == startOff)
				return &functions[i];
		}
		return 0;
	}
};

#define FORM_CHUNK 0x4D524F46
#define TEXT_CHUNK 0x54584554
#define DATA_CHUNK 0x41544144
#define ORDR_CHUNK 0x5244524F

typedef void (*CommandProc)(ScriptData *script, int argument);

class Script {
public:
	Script();
	~Script();

	bool setCommands(CommandProc *commands, int commandsSize);
	void setEngineVersion(int ver) { _engine = ver; }

	bool loadScript(const char *filename, ScriptData *data, OpcodeEntry *opcodes, int opcodeSize);
	void unloadScript(ScriptData *data);

	void printTextArea(ScriptData *dataPtr, const char *filename);
	void processScriptTrace(ScriptData *dataPtr);
	void decodeScript(ScriptData *dataPtr);
private:
	int findFunction(ScriptData *dataPtr, uint16 offset);
	void outputFunctionInfo(ScriptData *dataPtr, uint16 curOffset, bool list = false);

	static uint32 getFORMBlockSize(byte *&data);
	static uint32 getIFFBlockSize(byte *start, byte *&data, uint32 maxSize, const uint32 chunk);
	static bool loadIFFBlock(byte *start, byte *&data, uint32 maxSize, const uint32 chunk, byte *loadTo, uint32 ptrSize);

	int _engine;
	CommandProc *_commands;
	int _commandsSize;
};

#endif
