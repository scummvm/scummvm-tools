/* DeKyra - Basic Kyrandia script disassembler
 * Copyright (C) 2004-  Johannes Schickel
 * Copyright (C) 2004-  The ScummVM Team
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * $Header$
 *
 */

#include "dekyra.h"

int main(int argc, char** argv) {

	if (argc < 2) {
		printf("Use:\n"
			   "%s filename\n"
			   "-t   displays text segment\n"
			   "-n   display only function 'n'. n should be a integer\n",
			   argv[0]);
			   
		return false;
	}
	
	uint32 file = 0xFFFFFFFE;
	bool displayText = false;
	bool displayOnlyOneScript = false;
	int32 scriptnum = 0;
	
	// search for some parameters
	for (int param = 1; param < argc; ++param) {
		if (*argv[param] != '-' && file == 0xFFFFFFFE)
			file = param;
		else {
			if(argv[param][1] == 't')
				displayText = true;
			else {
				scriptnum = atoi(&argv[param][1]);
				
				if (scriptnum >= 0 && scriptnum < 256)
					displayOnlyOneScript = true;
			}				
		}
	}
	
	if (file == 0xFFFFFFFE) {
		printf("Use:\n"
			   "%s filename\n"
			   "-t   displays text segment\n"
			   "-n   display only function 'n'\n",
			   argv[0]);
			   
		return false;
	}
	
	Script myscript(argv[file]);
	
	if (myscript.isOpen()) {
		if (displayText) {
			myscript.decodeTextArea();
			printf("\n\n");
		}
		
		if (!displayOnlyOneScript) {
			myscript.decodeScript();
		} else {
			if (!myscript.decodeSpecialScript(scriptnum)) {
				error("in this script is no script with number %d", scriptnum);
			}
		}
		
	} else {
		error("couldn't find file '%s'", argv[1]);
	}	
	
	return true;
}

Script::Script(const char* filename) {
	_scriptFile = 0;
	FILE* script = fopen(filename, "r");
	
	if (!script)
		return;
		
	_scriptSize = fileSize(script);
  	_scriptFile = new uint8[_scriptSize];
	assert(_scriptFile); 
	fread(_scriptFile, sizeof(uint8) * _scriptSize, 1, script);
	fseek(script, 0, SEEK_SET);
 	
	uint8 chunkName[sizeof("EMC2ORDR") + 1];
	
	// so lets look for our chunks :)
	while(true) {
		if ((uint32)ftell(script) >= _scriptSize) {
			break;
		}		
		// lets read only the first 4 chars
		fread(chunkName, sizeof(uint8) * 4, 1, script);
		chunkName[4] = '\0';
		// check name of chunk
		if (!strcmp((char*)chunkName, "FORM")) {			
			// FreeKyra swaps the size I only read it in BigEndian :)
			_chunks[kForm]._size = readUint32BE(script);				
		} else if (!strcmp((char*)chunkName, "TEXT")) {
			uint32 text_size = readUint32BE(script);
			text_size += text_size % 2 != 0 ? 1 : 0;
			
			_chunks[kText]._data = _scriptFile + ftell(script);
			_chunks[kText]._size = TO_BE_16(*((uint16*)_chunks[kText]._data)) >> 1;
			_chunks[kText]._additional = _chunks[kText]._data + (_chunks[kText]._size << 1);				
			fseek(script, text_size, SEEK_CUR);
		} else if (!strcmp((char*)chunkName, "DATA")) {
			_chunks[kData]._size = readUint32BE(script);
			_chunks[kData]._data = _scriptFile + ftell(script);				
			// mostly it will be the end of the file because all files should end with a 'DATA' chunk
			fseek(script, _chunks[kData]._size, SEEK_CUR);
		} else {
			// read next 4 chars
			fread(&chunkName[4], sizeof(uint8) * 4, 1, script);
			chunkName[8] = '\0';
			if (!strcmp((char*)chunkName, "EMC2ORDR")) {
				_chunks[kEmc2Ordr]._size = readUint32BE(script) >> 1;
				_chunks[kEmc2Ordr]._data = _scriptFile + ftell(script);					
				fseek(script, _chunks[kEmc2Ordr]._size * 2, SEEK_CUR);
			} else {
				// any unkown chunk or problems with seeking through the file
				error("unknown chunk '%s'", chunkName);
			}
		}
	}
	
	fclose(script);
}

void Script::decodeTextArea(void) {
	printf("TEXT chunk:\n");
	// first is size
	for (uint32 pos = 1; pos < (_chunks[kText]._size << 1); ++pos) {
		if (TO_BE_16(((uint16*)_chunks[kText]._data)[pos]) >= _scriptSize) {
			break;
		}
		uint32 startoffset = TO_BE_16(((uint16*)_chunks[kText]._data)[pos]);
		printf("Index: %d Offset: %d:\n", pos, startoffset);
		/*uint32 endoffset = TO_BE_16(((uint16*)_chunks[kText]._data)[pos+1]);
		printf("\nstartoffset = %d, endoffset = %d\n\n", startoffset, endoffset);
		for (; startoffset < endoffset; ++startoffset) {
			printf("%c", *(char*)(_chunks[kText]._additional + startoffset));
		}
		printf("\n");*/
		printf("%d(%d) : %s\n", pos, startoffset, (char*)(_chunks[kText]._data + startoffset));
	}
}

void Script::decodeScript(void) {
	for (uint32 script = 0; decodeSpecialScript(script); ++script)
					;
}

struct CommandDesc {
	uint16 command;	// normally uint8
	const char* description;
	bool usesArgument;
	
};

struct ScriptDesc {
	int32 script;	// normally uint8
	const char* description;
};

struct OpcodeDesc {
	uint16 opcode;	// normally uint8
	const char* description;
	const char* parameters;
};

// function wich calls opcode procs
const uint16 OPCODE_CALLER = 0x0E;

bool Script::decodeSpecialScript(int32 script) {
	static CommandDesc commandDesc[] = {
		{ 0x00, "c1_goToLine", true },
		{ 0x01, "c1_setReturn", true },
		{ 0x02, "c1_pushRetRec", true },
		{ 0x03, "c1_push", true },
		{ 0x04, "c1_push", true },
		{ 0x05, "c1_pushVar", true },
		{ 0x06, "c1_pushFrameNeg", true },
		{ 0x07, "c1_pushFramePos", true },
		{ 0x08, "c1_popRetRec", true },
		{ 0x09, "c1_popVar", true },
		{ 0x0A, "c1_popFrameNeg", true },
		{ 0x0B, "c1_popFramePos", true },
		{ 0x0C, "c1_addToSP", true },
		{ 0x0D, "c1_subFromSP", true },
		{ 0x0E, "c1_execOpcode", true },
		{ 0x0F, "c1_ifNotGoTo", true },
		{ 0x10, "c1_negate", true },
		{ 0x11, "c1_evaluate", true },
		// normaly only untils 0xFF
		{ 0xFFFF, 0, 0 }
	};
	
	static ScriptDesc scriptDesc[] = {
		{ 0, "kSetupScene" },
		{ 1, "kClickEvent" },
		{ 2, "kActorEvent" },
		{ 4, "kEnterEvent" },
		{ 5, "kExitEvent" },
		{ 7, "kLoadResources" },
		{ 0xFFFF, "unknown script" }
	};
	
	static OpcodeDesc opcodeDesc[] = {
	};
	
	if ((uint32)script >= _chunks[kEmc2Ordr]._size || script < 0) {
		return false;
	}

	bool gotScriptName = false;
	
	printf("Script: ");

	for (uint32 pos = 0; pos < ARRAYSIZE(scriptDesc) - 1; ++pos) {
		if (scriptDesc[pos].script == script) {
			printf("%s:\n", scriptDesc[pos].description);
			gotScriptName = true;
			break;
		}
	}
	
	if (!gotScriptName) {
		printf("%s:\n", scriptDesc[ARRAYSIZE(scriptDesc) - 1].description);
	}
	
	uint8 _currentCommand = 0;
	uint8 _argument = 0;	
	_currentPos = (TO_BE_16(_chunks[kEmc2Ordr]._data[script]) << 1) + 2;	
	uint8* script_start = _chunks[kData]._data;
	bool gotArgument = true;
	
	uint32 nextScriptStartsAt = getNextScriptPos(_currentPos);
	
	while(true) {
		if ((uint32)_currentPos > _chunks[kData]._size || (uint32)_currentPos >= nextScriptStartsAt) {
			break;
		}
	
		gotArgument = true;
		_currentCommand = *(script_start + _currentPos++);
			
		// gets out 
		if (_currentCommand & 0x80) {
			_argument = ((_currentCommand & 0x0F) << 8) | *(script_start + _currentPos++);
			_currentCommand &= 0xF0;
		} else if (_currentCommand & 0x40) {
			_argument = *(script_start + _currentPos++);
		} else if (_currentCommand & 0x20) {
			_currentPos++;
				
			uint16 tmp = *(uint16*)(script_start + _currentPos);
			tmp &= 0xFF7F;
			
			_argument = TO_BE_16(tmp);
			_currentPos += 2;
		} else {
			gotArgument = false;
		}
			
		_currentCommand &= 0x1f;
		
		// prints the offset
		printf("0x%x:\t", _currentPos);
		
		bool gotCommand = false;
		
		// lets get out what the command means
		for (uint32 pos = 0; pos < ARRAYSIZE(commandDesc) - 1; ++pos) {
			if (commandDesc[pos].command == _currentCommand) {
				gotCommand = true;
				printf("%s", commandDesc[pos].description);
				
				if (commandDesc[pos].usesArgument && commandDesc[pos].command != OPCODE_CALLER) {
					printf("(0x%x)", _argument);
				} else if(commandDesc[pos].usesArgument && commandDesc[pos].command == OPCODE_CALLER) {
					bool gotOpcode = false;
					// lets look for our opcodes
					for (uint32 pos2 = 0; pos2 < ARRAYSIZE(opcodeDesc); ++pos2) {
						if (opcodeDesc[pos2].opcode == _argument) {
							printf("(%s(%s))", opcodeDesc[pos2].description, opcodeDesc[pos2].parameters);
							gotOpcode = true;
							break;
						}
					}
					
					if (!gotOpcode) {
						printf("(0x%x(unknown))", _argument);
					}			
				}
				
				break;
			}
		}
		
		// prints our command number + arg
		if (!gotCommand) {
			printf("0x%x(0x%x)", _currentCommand, _argument);
		}
		
		if (!gotArgument) {
			printf("\t; couldn't get argument! maybe command is wrong.");
		}
		
		printf("\n");
	}
	
	printf("\n-------------\n");
	
	return true;
}

uint32 Script::getNextScriptPos(uint32 current_start) {
	uint32 pos = 0xFFFFFFFE;
	
	for (uint32 tmp = 0; tmp < _chunks[kEmc2Ordr]._size; ++tmp) {
		if ((uint32)((TO_BE_16(_chunks[kEmc2Ordr]._data[tmp]) << 1) + 2) > current_start &&
			(uint32)((TO_BE_16(_chunks[kEmc2Ordr]._data[tmp]) << 1) + 2) < pos) {
			pos = ((TO_BE_16(_chunks[kEmc2Ordr]._data[tmp]) << 1) + 2);
		}
	}
	
	if (pos > _scriptSize) {
		pos = _scriptSize;
	}
	
	return pos;
}
