/* DeKyra - Basic Kyrandia script disassembler
 * Copyright (C) 2004  Johannes Schickel
 * Copyright (C) 2004-2005  The ScummVM Team
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
			return true;
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
	_stackPos = 0;
	FILE* script = fopen(filename, "rb");
	
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
		printf("%d(at %d) : %s\n", pos, startoffset, (char*)(_chunks[kText]._data + startoffset));
	}
}

void Script::decodeScript(void) {
	bool ok = true;
	for (uint32 script = 0; ok; ++script) {
		// skip the click event
		if (script == 1)
			continue;
		ok = decodeSpecialScript(script);
	}
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
};

// function wich calls opcode procs
const uint16 OPCODE_CALLER = 0x0E;

bool Script::decodeSpecialScript(int32 script) {
	static CommandDesc commandDesc[] = {
		{ 0x00, "goToLine", true },
		{ 0x01, "setReturn", true },
		{ 0x02, "pushRetRec", true },
		{ 0x03, "push", true },
		{ 0x04, "push", true },
		{ 0x05, "pushVar", true },
		{ 0x06, "pushFrameNeg", true },
		{ 0x07, "pushFramePos", true },
		{ 0x08, "popRetRec", true },
		{ 0x09, "popVar", true },
		{ 0x0A, "popFrameNeg", true },
		{ 0x0B, "popFramePos", true },
		{ 0x0C, "addToSP", true },
		{ 0x0D, "subFromSP", true },
		{ 0x0E, "execOpcode", true },
		{ 0x0F, "ifNotGoTo", true },
		{ 0x10, "negate", true },
		{ 0x11, "evaluate", true },
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
		{ 0x00 ,"o1_Magic_In_Mouse_Item" },
		{ 0x01 ,"o1_Character_Says" },
		{ 0x02 ,"o1_Pause_Ticks" },
		{ 0x03 ,"o1_Draw_Scene_Anim_Shape" },
		{ 0x04 ,"o1_Query_Game_Flag" },
		{ 0x05 ,"o1_Set_Game_Flag" },
		{ 0x06 ,"o1_Reset_Game_Flag" },
		{ 0x07 ,"o1_Run_NPC_Script" },
		{ 0x08 ,"o1_Set_Special_Exit_List" },
		{ 0x09 ,"o1_Block_In_Walkable_Region" },
		{ 0x0A ,"o1_Block_Out_Walkable_Region" },
		{ 0x0B ,"o1_Walk_Player_To_Point" },
		{ 0x0C ,"o1_Drop_Item_In_Scene" },
		{ 0x0D ,"o1_Draw_Anim_Shape_Into_Scene" },
		{ 0x0E ,"o1_Create_Mouse_Item" },
		{ 0x0F ,"o1_Save_Page_To_Disk" },
		{ 0x10 ,"o1_Load_Page_From_Disk" },
		{ 0x11 ,"o1_Scene_Anim_On" },
		{ 0x12 ,"o1_Scene_Anim_Off" },
		{ 0x13 ,"o1_Elapsed_Seconds" },
		{ 0x14 ,"o1_Mouse_Is_Pointer" },
		{ 0x15 ,"o1_Destroy_Mouse_Item" },
		{ 0x16 ,"o1_Run_Scene_Anim_Until_Done" },
		{ 0x17 ,"o1_Fade_Special_Palette" },
		{ 0x18 ,"o1_Play_AdLib_Sound" },
		{ 0x19 ,"o1_Play_AdLib_Score" },
		{ 0x1A ,"o1_Phase_In_Same_Scene" },
		{ 0x1B ,"o1_Set_Scene_Phasing_Flag" },
		{ 0x1C ,"o1_Reset_Scene_Phasing_Flag" },
		{ 0x1D ,"o1_Query_Scene_Phasing_Flag" },
		{ 0x1E ,"o1_Scene_To_Direction" },
		{ 0x1F ,"o1_Set_Birthstone_Gem" },
		{ 0x20 ,"o1_Place_Item_In_Generic_Map_Scene" },
		{ 0x21 ,"o1_Set_Brandon_Status_Bit" },
		{ 0x22 ,"o1_Pause_Seconds" },
		{ 0x23 ,"o1_Get_Characters_Location" },
		{ 0x24 ,"o1_Run_NPC_Subscript" },
		{ 0x25 ,"o1_Magic_Out_Mouse_Item" },
		{ 0x26 ,"o1_Internal_Anim_On" },
		{ 0x27 ,"o1_Force_Brandon_To_Normal" },
		{ 0x28 ,"o1_Poison_Death_Now" },
		{ 0x29 ,"o1_Set_Scale_Mode" },
		{ 0x2A ,"o1_Open_WSA_File" },
		{ 0x2B ,"o1_Close_WSA_File" },
		{ 0x2C ,"o1_Run_WSA_From_Beginning_To_End" },
		{ 0x2D ,"o1_Display_WSA_Frame" },
		{ 0x2E ,"o1_Enter_New_Scene" },
		{ 0x2F ,"o1_Set_Special_Enter_X_And_Y" },
		{ 0x30 ,"o1_Run_WSA_Frames" },
		{ 0x31 ,"o1_Pop_Brandon_Into_Scene" },
		{ 0x32 ,"o1_Restore_All_Object_Backgrounds" },
		{ 0x33 ,"o1_Set_Custom_Palette_Range" },
		{ 0x34 ,"o1_Custom_Print_Talk_String" },
		{ 0x35 ,"o1_Restore_Custom_Print_Background" },
		{ 0x36 ,"o1_Hide_Mouse" },
		{ 0x37 ,"o1_Show_Mouse" },
		{ 0x38 ,"o1_Get_Character_X" },
		{ 0x39 ,"o1_Get_Character_Y" },
		{ 0x3A ,"o1_Change_Characters_Facing" },
		{ 0x3B ,"o1_Copy_WSA_Region" },
		{ 0x3C ,"o1_Text_Print" },
		{ 0x3D ,"o1_Random" },
		{ 0x3E ,"o1_Load_Sound_File" },
		{ 0x3F ,"o1_Display_WSA_Frame_On_HidPage" },
		{ 0x40 ,"o1_Display_WSA_Sequential_Frames" },
		{ 0x41 ,"o1_Draw_Character_Standing" },
		{ 0x42 ,"o1_Internal_Anim_Off" },
		{ 0x43 ,"o1_Change_Characters_X_And_Y" },
		{ 0x44 ,"o1_Clear_Scene_Animator_Beacon" },
		{ 0x45 ,"o1_Query_Scene_Animator_Beacon" },
		{ 0x46 ,"o1_Refresh_Scene_Animator" },
		{ 0x47 ,"o1_Place_Item_In_Off_Scene" },
		{ 0x48 ,"o1_Wipe_Down_Mouse_Item" },
		{ 0x49 ,"o1_Place_Character_In_Other_Scene" },
		{ 0x4A ,"o1_Get_Key" },
		{ 0x4B ,"o1_Specific_Item_In_Inventory" },
		{ 0x4C ,"o1_Pop_Mobile_NPC_Into_Scene" },
		{ 0x4D ,"o1_Mobile_Character_In_Scene" },
		{ 0x4E ,"o1_Hide_Mobile_Character" },
		{ 0x4F ,"o1_Unhide_Mobile_Character" },
		{ 0x50 ,"o1_Set_Characters_Location" },
		{ 0x51 ,"o1_Walk_Character_To_Point" },
		{ 0x52 ,"o1_Special_Event_Display_Brynns_Note" },
		{ 0x53 ,"o1_Special_Event_Remove_Brynns_Note" },
		{ 0x54 ,"o1_Set_Logic_Page" },
		{ 0x55 ,"o1_Fat_Print" },
		{ 0x56 ,"o1_Preserve_All_Object_Backgrounds" },
		{ 0x57 ,"o1_Update_Scene_Animations" },
		{ 0x58 ,"o1_Scene_Animation_Active" },
		{ 0x59 ,"o1_Set_Characters_Movement_Delay" },
		{ 0x5A ,"o1_Get_Characters_Facing" },
		{ 0x5B ,"o1_Bkgd_Scroll_Scene_And_Masks_Right" },
		{ 0x5C ,"o1_Find_Brightest_Fireberry" },
		{ 0x5D ,"o1_Set_Fireberry_Glow_Palette" },
		{ 0x5E ,"o1_Set_Death_Handler_Flag" },
		{ 0x5F ,"o1_Drink_Potion_Animation" },
		{ 0x60 ,"o1_Make_Amulet_Appear" },
		{ 0x61 ,"o1_Draw_Item_Shape_Into_Scene" },
		{ 0x62 ,"o1_Set_Characters_Current_Frame" },
		{ 0x63 ,"o1_Wait_For_Confirmation_Mouse_Click" },
		{ 0x64 ,"o1_Page_Flip" },
		{ 0x65 ,"o1_Set_Scene_File" },
		{ 0x66 ,"o1_What_Item_In_Marble_Vase" },
		{ 0x67 ,"o1_Set_Item_In_Marble_Vase" },
		{ 0x68 ,"o1_Add_Item_To_Inventory" },
		{ 0x69 ,"o1_Int_Print" },
		{ 0x6A ,"o1_Shake_Screen" },
		{ 0x6B ,"o1_Create_Amulet_Jewel" },
		{ 0x6C ,"o1_Set_Scene_Anim_Curr_XY" },
		{ 0x6D ,"o1_Poison_Brandon_And_Remaps" },
		{ 0x6E ,"o1_Fill_Flask_With_Water" },
		{ 0x6F ,"o1_Get_Characters_Movement_Delay" },
		{ 0x70 ,"o1_Get_Birthstone_Gem" },
		{ 0x71 ,"o1_Query_Brandon_Status_Bit" },
		{ 0x72 ,"o1_Play_Flute_Animation" },
		{ 0x73 ,"o1_Play_Winter_Scroll_Sequence" },
		{ 0x74 ,"o1_Get_Idol_Gem" },
		{ 0x75 ,"o1_Set_Idol_Gem" },
		{ 0x76 ,"o1_Total_Items_In_Scene" },
		{ 0x77 ,"o1_Restore_Brandons_Movement_Delay" },
		{ 0x78 ,"o1_Set_Mouse_Pos" },
		{ 0x79 ,"o1_Get_Mouse_State" },
		{ 0x7A ,"o1_Set_Entrance_Mouse_Cursor_Track" },
		{ 0x7B ,"o1_Item_Appears_On_Ground" },
		{ 0x7C ,"o1_Set_No_Draw_Shapes_Flag" },
		{ 0x7D ,"o1_Fade_Entire_Palette" },
		{ 0x7E ,"o1_Item_On_Ground_Here" },
		{ 0x7F ,"o1_Query_Cauldron_State" },
		{ 0x80 ,"o1_Set_Cauldron_State" },
		{ 0x81 ,"o1_Query_Crystal_State" },
		{ 0x82 ,"o1_Set_Crystal_State" },
		{ 0x83 ,"o1_Set_Palette_Range" },
		{ 0x84 ,"o1_Shrink_Brandon_Down" },
		{ 0x85 ,"o1_Grow_Brandon_Up" },
		{ 0x86 ,"o1_Set_Brandon_Scale_X_And_Y" },
		{ 0x87 ,"o1_Reset_Scale_Mode" },
		{ 0x88 ,"o1_Get_Scale_Depth_Table_Value" },
		{ 0x89 ,"o1_Set_Scale_Depth_Table_Value" },
		{ 0x8A ,"o1_Message" },
		{ 0x8B ,"o1_Check_Click_On_NPC" },
		{ 0x8C ,"o1_Get_Foyer_Item" },
		{ 0x8D ,"o1_Set_Foyer_Item" },
		{ 0x8E ,"o1_Dispel_Magic_Animation" },
		{ 0x8F ,"o1_Set_No_Item_Drop_Region" },
		{ 0x90 ,"o1_Walk_Malcolm_On" },
		{ 0x91 ,"o1_Passive_Protection" },
		{ 0x92 ,"o1_Set_Playing_Loop" },
		{ 0x93 ,"o1_Brandon_To_Stone_Sequence" },
		{ 0x94 ,"o1_Brandon_Healing_Sequence" },
		{ 0x95 ,"o1_Protect_Command_Line" },
		{ 0x96 ,"o1_Pause_Music_Seconds" },
		{ 0x97 ,"o1_Reset_Mask_Region" },
		{ 0x98 ,"o1_Set_Palette_Change_Flag" },
		{ 0x99 ,"o1_Fill_Rect" },
		{ 0x9A ,"o1_Dummy" }
	};
	
	if ((uint32)script >= _chunks[kEmc2Ordr]._size || script < 0) {
		return false;
	}

	bool gotScriptName = false;
	
	printf("Script: ");

	for (uint32 pos = 0; pos < ARRAYSIZE(scriptDesc) - 1; ++pos) {
		if (scriptDesc[pos].script == script) {
			printf("%s:\n" , scriptDesc[pos].description);
			gotScriptName = true;
			break;
		}
	}
	
	if (!gotScriptName) {
		printf("%s:\n" , scriptDesc[ARRAYSIZE(scriptDesc) - 1].description);
	}

	memset(_stack, 0, sizeof(_stack));
	memset(_registers, 0, sizeof(_registers));
	
	_stackPos = 0;
	_instructionPos = (TO_BE_16(((uint16*)_chunks[kEmc2Ordr]._data)[script]) << 1);	
	uint8* script_start = _chunks[kData]._data;
	bool gotArgument = true;
	
	// uint32 nextScriptStartsAt = getNextScriptPos(_instructionPos);
	
	while(true) {
		if ((uint32)_instructionPos > _chunks[kData]._size /*|| (uint32)_instructionPos >= nextScriptStartsAt*/) {
			break;
		}

		// prints the offset
		printf("0x%04x(0x%04x):\t\t" , _instructionPos, (_instructionPos) + (int)(_chunks[kData]._data - _scriptFile));
			
		gotArgument = true;
		_currentCommand = *(script_start + _instructionPos++);
			
		// gets out 
		if (_currentCommand & 0x80) {
			_argument = ((_currentCommand & 0x0F) << 8) | *(script_start + _instructionPos++);
			_currentCommand &= 0xF0;
		} else if (_currentCommand & 0x40) {
			_argument = *(script_start + _instructionPos++);
		} else if (_currentCommand & 0x20) {
			_instructionPos++;
				
			uint16 tmp = *(uint16*)(script_start + _instructionPos);
			tmp &= 0xFF7F;
			
			_argument = TO_BE_16(tmp);
			_instructionPos += 2;
		} else {
			gotArgument = false;
		}
			
		_currentCommand &= 0x1f;

		execCommand(_currentCommand);
		
		bool gotCommand = false;
		
		// lets get out what the command means
		for (uint32 pos = 0; pos < ARRAYSIZE(commandDesc) - 1; ++pos) {
			if (commandDesc[pos].command == _currentCommand) {
				gotCommand = true;
				printf("%s" , commandDesc[pos].description);
				
				if (commandDesc[pos].usesArgument && commandDesc[pos].command != OPCODE_CALLER) {
					printf("(0x%x)" , _argument);
				} else if(commandDesc[pos].usesArgument && commandDesc[pos].command == OPCODE_CALLER) {
					bool gotOpcode = false;
					// lets look for our opcodes
					for (uint32 pos2 = 0; pos2 < ARRAYSIZE(opcodeDesc); ++pos2) {
						if (opcodeDesc[pos2].opcode == _argument) {
							printf("(%s(%s))", opcodeDesc[pos2].description, getParamsOnStack());
							gotOpcode = true;
							break;
						}
					}
					
					if (!gotOpcode) {
						printf("(0x%x(%s))" , _argument, getParamsOnStack());
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

int32 Script::param(int32 index) {
	if (_stackPos - index - 1 >= ARRAYSIZE(_stack) || _stackPos - index - 1 < 0)
		return -0xFFFF;
	return _stack[_stackPos - index - 1];
}
	
const char* Script::stringAtIndex(int32 index) {
	if (index < 0 || (uint32)index >= _chunks[kText]._size)
		return 0;
	
	uint16 offset = TO_BE_16(((uint16*)_chunks[kText]._data)[index]);
	return (const char *)(_chunks[kText]._data + offset);
}

const char* Script::getParamsOnStack(void) {
	static char buffer[1024];
	memset(buffer, 0, 1024 * sizeof(char));

	strcpy(buffer, " ");

	for (int32 pos = 0; param(pos) != -0xFFFF; ++pos) {
		if (pos != 0)
			strcat(buffer, ", ");
		char _buffer[128];
		sprintf(_buffer, "[%d | %s]", param(pos), paramString(pos));
		strcat(buffer, _buffer);
	}

	strcat(buffer, " ");
	
	return buffer;
}

void Script::execCommand(uint32 command) {
#define COMMAND(x) { &Script::x }
	typedef void (Script::*CommandProc)();
	struct CommandEntry {
		CommandProc proc;
	};

	// now we create a list of all Command/Opcode procs and so
	static CommandEntry commandProcs[] = {
		// 0x00
		COMMAND(goToLine),
		COMMAND(setReturn),
		COMMAND(pushRetRec),
		COMMAND(push),
		// 0x04
		COMMAND(push),			
		COMMAND(pushVar),
		COMMAND(pushFrameNeg),
		COMMAND(pushFramePos),
		// 0x08
		COMMAND(popRetRec),
		COMMAND(popVar),
		COMMAND(popFrameNeg),
		COMMAND(popFramePos),
		// 0x0C
		COMMAND(addToSP),
		COMMAND(subFromSP),
		COMMAND(execOpcode),
		COMMAND(ifNotGoTo),
		// 0x10
		COMMAND(negate),
		COMMAND(evaluate)
	};

	static uint16 _numCommands = ARRAYSIZE(commandProcs);
	static CommandEntry* _commands = commandProcs;
	
	if (command < _numCommands) {
		CommandProc currentProc = _commands[command].proc;
		(this->*currentProc)();
	}
}

void Script::goToLine(void) {
	_instructionPos = _argument << 1;
}

void Script::setReturn(void) {
	_returnValue = _argument;
}

void Script::pushRetRec(void) {
	if (!_argument) {
		pushStack(_returnValue);
	} else {
		int32 rec = ((int16)_tempPos << 16) | ((_instructionPos >> 1) + 1);
		pushStack(rec);
		_tempPos = _instructionPos;
	}
}

void Script::push(void) {
	pushStack(_argument);
}

void Script::pushVar(void) {
	printf("position = %d ", _stackPos);
	int32 value = _registers[_argument];
	printf("value = %d ", value);
	pushStack(value);
}

void Script::pushFrameNeg(void) {
	pushStack(_stack[_tempPos + _argument]);
}

void Script::pushFramePos(void) {
	pushStack(_stack[_tempPos - _argument]);
}

void Script::popRetRec(void) {
	if (!_argument) {
		_returnValue = popStack();
	} else {
		if (_stackPos <= 0) {
			_instructionPos = 0xFFFF;
			return;
		}
		int32 rec = popStack();
		
		_tempPos = (int16)((rec & 0xFFFF0000) >> 16);
		_instructionPos = (rec & 0x0000FFFF) * 2;
	}
}

void Script::popVar(void) {
	_registers[_argument] = popStack();
}

void Script::popFrameNeg(void) {
	_stack[_tempPos + _argument] = popStack();
}

void Script::popFramePos(void) {
	_stack[_tempPos - _argument] = popStack();
}

void Script::addToSP(void) {
	_stackPos -= _argument;
}

void Script::subFromSP(void) {
	_stackPos += _argument;
}

void Script::execOpcode(void) {
}

void Script::ifNotGoTo(void) {
	if (!popStack()) {
		_instructionPos = _argument << 1;
	}
}

void Script::negate(void) {
	switch(_argument) {
	case 0:
		topStack() = !topStack();
		break;
	
	case 1:
		topStack() = -topStack();
		break;
	
	case 2:
		topStack() = ~topStack();
		break;
	
	default:
		printf("ERROR: unkown negate instruction %d\n", _argument);
		break;
	};
}

void Script::evaluate(void) {
	int32 x, y;
	int32 res = false;
	
	x = popStack();
	y = popStack();
	
	switch(_argument) {
	case 0:
		res = x && y;
		break;
	
	case 1:
		res = x || y;
		break;

	case 2:
		res = x == y;
		break;
	
	case 3:
		res = x != y;
		break;

	case 4:
		res = x < y;
		break;

	case 5:
		res = x <= y;
		break;
		
	case 6:
		res = x > y;
		break;
	
	case 7:
		res = x >= y;
		break;
	
	case 8:
		res = x + y;
		break;
		
	case 9:
		res = x - y;
		break;
	
	case 10:
		res = x * y;
		break;
		
	case 11:
		res = x / y;
		break;
	
	case 12:
		res = x >> y;
		break;
	
	case 13:
		res = x << y;
		break;
	
	case 14:
		res = x & y;
		break;
	
	case 15:
		res = x | y;
		break;
	
	case 16:
		res = x % y;
		break;
	
	case 17:
		res = x ^ y;
		break;
	
	default:
		printf("ERROR: unknown evaluate command %d\n", _argument);
		break;
	};
	
	pushStack(res);
}

