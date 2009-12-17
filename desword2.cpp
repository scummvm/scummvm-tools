/* Script disassembler for Broken Sword II
 * Copyright (C) 2003-2006  The ScummVM Team
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
 *
 * $URL$
 * $Id$
 *
 */

#include <cstdlib>
#include <cstdio>

#include "common/file.h"

enum {
	GAME_OBJECT = 3,
	SCREEN_MANAGER = 9
};

enum {
	CP_END_SCRIPT			= 0,
	CP_PUSH_LOCAL_VAR32		= 1,
	CP_PUSH_GLOBAL_VAR32		= 2,
	CP_POP_LOCAL_VAR32		= 3,
	CP_CALL_MCODE			= 4,
	CP_PUSH_LOCAL_ADDR		= 5,
	CP_PUSH_INT32			= 6,
	CP_SKIPONFALSE			= 7,
	CP_SKIPALWAYS			= 8,
	CP_SWITCH			= 9,
	CP_ADDNPOP_LOCAL_VAR32		= 10,
	CP_SUBNPOP_LOCAL_VAR32		= 11,
	CP_SKIPONTRUE			= 12,
	CP_POP_GLOBAL_VAR32		= 13,
	CP_ADDNPOP_GLOBAL_VAR32		= 14,
	CP_SUBNPOP_GLOBAL_VAR32		= 15,
	CP_DEBUGON			= 16,
	CP_DEBUGOFF			= 17,
	CP_QUIT				= 18,
	CP_TERMINATE			= 19,
	OP_ISEQUAL			= 20,
	OP_PLUS				= 21,
	OP_MINUS			= 22,
	OP_TIMES			= 23,
	OP_DIVIDE			= 24,
	OP_NOTEQUAL			= 25,
	OP_ANDAND			= 26,
	OP_GTTHAN			= 27,
	OP_LSTHAN			= 28,
	CP_JUMP_ON_RETURNED		= 29,
	CP_TEMP_TEXT_PROCESS		= 30,
	CP_SAVE_MCODE_START		= 31,
	CP_RESTART_SCRIPT		= 32,
	CP_PUSH_STRING			= 33,
	CP_PUSH_DEREFERENCED_STRUCTURE	= 34,
	OP_GTTHANE			= 35,
	OP_LSTHANE			= 36,
	OP_OROR				= 37
};

const char *opcodes[] = {
	/* 00 */
	"fnTestFunction",
	"fnTestFlags",
	"fnRegisterStartPoint",
	"fnInitBackground",
	/* 04 */
	"fnSetSession",
	"fnBackSprite",
	"fnSortSprite",
	"fnForeSprite",
	/* 08 */
	"fnRegisterMouse",
	"fnAnim",
	"fnRandom",
	"fnPreLoad",
	/* 0C */
	"fnAddSubject",
	"fnInteract",
	"fnChoose",
	"fnWalk",
	/* 10 */
	"fnWalkToAnim",
	"fnTurn",
	"fnStandAt",
	"fnStand",
	/* 14 */
	"fnStandAfterAnim",
	"fnPause",
	"fnMegaTableAnim",
	"fnAddMenuObject",
	/* 18 */
	"fnStartConversation",
	"fnEndConversation",
	"fnSetFrame",
	"fnRandomPause",
	/* 1C */
	"fnRegisterFrame",
	"fnNoSprite",
	"fnSendSync",
	"fnUpdatePlayerStats",
	/* 20 */
	"fnPassGraph",
	"fnInitFloorMouse",
	"fnPassMega",
	"fnFaceXY",
	/* 24 */
	"fnEndSession",
	"fnNoHuman",
	"fnAddHuman",
	"fnWeWait",
	/* 28 */
	"fnTheyDoWeWait",
	"fnTheyDo",
	"fnWalkToTalkToMega",
	"fnFadeDown",
	/* 2C */
	"fnISpeak",
	"fnTotalRestart",
	"fnSetWalkGrid",
	"fnSpeechProcess",
	/* 30 */
	"fnSetScaling",
	"fnStartEvent",
	"fnCheckEventWaiting",
	"fnRequestSpeech",
	/* 34 */
	"fnGosub",
	"fnTimedWait",
	"fnPlayFx",
	"fnStopFx",
	/* 38 */
	"fnPlayMusic",
	"fnStopMusic",
	"fnSetValue",
	"fnNewScript",
	/* 3C */
	"fnGetSync",
	"fnWaitSync",
	"fnRegisterWalkGrid",
	"fnReverseMegaTableAnim",
	/* 40 */
	"fnReverseAnim",
	"fnAddToKillList",
	"fnSetStandbyCoords",
	"fnBackPar0Sprite",
	/* 44 */
	"fnBackPar1Sprite",
	"fnForePar0Sprite",
	"fnForePar1Sprite",
	"fnSetPlayerActionEvent",
	/* 48 */
	"fnSetScrollCoordinate",
	"fnStandAtAnim",
	"fnSetScrollLeftMouse",
	"fnSetScrollRightMouse",
	/* 4C */
	"fnColour",
	"fnFlash",
	"fnPreFetch",
	"fnGetPlayerSaveData",
	/* 50 */
	"fnPassPlayerSaveData",
	"fnSendEvent",
	"fnAddWalkGrid",
	"fnRemoveWalkGrid",
	/* 54 */
	"fnCheckForEvent",
	"fnPauseForEvent",
	"fnClearEvent",
	"fnFaceMega",
	/* 58 */
	"fnPlaySequence",
	"fnShadedSprite",
	"fnUnshadedSprite",
	"fnFadeUp",
	/* 60 */
	"fnDisplayMsg",
	"fnSetObjectHeld",
	"fnAddSequenceText",
	"fnResetGlobals",
	/* 64 */
	"fnSetPalette",
	"fnRegisterPointerText",
	"fnFetchWait",
	"fnRelease",
	/* 68 */
	"fnPrepareMusic",
	"fnSoundFetch",
	"fnPrepareMusic",
	"fnSmackerLeadIn",
	/* 6C */
	"fnSmackerLeadOut",
	"fnStopAllFx",
	"fnCheckPlayerActivity",
	"fnResetPlayerActivityDelay",
	/* 70 */
	"fnCheckMusicPlaying",
	"fnPlayCredits",
	"fnSetScrollSpeedNormal",
	"fnSetScrollSpeedSlow",
	/* 74 */
	"fnRemoveChooser",
	"fnSetFxVolAndPan",
	"fnSetFxVol",
	"fnRestoreGame",
	/* 78 */
	"fnRefreshInventory",
	"fnChangeShadows"
};

const char *globalVar(int num) {
	static char buf[40];

	switch (num) {
	case 0:		return "ID";
	case 1:		return "RESULT";
	case 2:		return "PLAYER_ACTION";
	case 305:	return "PLAYER_ID";
	case 13:	return "TALK_FLAG";
	case 4:		return "MOUSE_X";
	case 5:		return "MOUSE_Y";
	case 109:	return "LEFT_BUTTON";
	case 110:	return "RIGHT_BUTTON";
	case 178:	return "CLICKED_ID";
	case 6:		return "IN_SUBJECT";
	case 7:		return "COMBINE_BASE";
	case 14:	return "OBJECT_HELD";
	case 9:		return "SPEECH_ID";
	case 10:	return "INS1";
	case 11:	return "INS2";
	case 12:	return "INS3";
	case 60:	return "INS4";
	case 61:	return "INS5";
	case 59:	return "INS_COMMAND";
	case 141:	return "PLAYER_FEET_X";
	case 142:	return "PLAYER_FEET_Y";
	case 937:	return "PLAYER_CUR_DIR";
	case 62:	return "LOCATION";
	case 345:	return "SCROLL_X";
	case 346:	return "SCROLL_Y";
	case 710:	return "EXIT_CLICK_ID";
	case 713:	return "EXIT_FADING";
	case 912:	return "SYSTEM_TESTING_ANIMS";
	case 1230:	return "SYSTEM_TESTING_TEXT";
	case 1245:	return "SYSTEM_WANT_PREVIOUS_LINE";
	case 686:	return "MOUSE_AVAILABLE";
	case 1115:	return "AUTO_SELECTED";
	case 15:	return "CHOOSER_COUNT_FLAG";
	case 1153:	return "DEMO";
	case 1173:	return "PSXFLAG";
	case 1256:	return "DEAD";
	case 1278:	return "SPEECHANIMFLAG";
	case 1314:	return "SCROLL_OFFSET_X";
	case 111:	return "GAME_LANGUAGE";
	default:
		sprintf(buf, "global(%d)", num);
		return buf;
	}
}

int entry(int argc, char *argv[]) {
	File in;
	uint8 type;
	char name[34];
	int32 size;
	int32 numScripts;
	long scriptBase;
	int32 scriptOffsets[100];	/* Way more than enough */
	int i, j;

	if (argc != 2) {
		printf("Usage: desword2 file\n");
		return EXIT_FAILURE;
	}

	in.open(argv[1], "rb");

	/* Standard header
	 *
	 *  1 byte file type
	 *  1 byte compression type
	 *  4 bytes compressed size
	 *  4 bytes decompressed size
	 * 34 bytes name of object
	 */

	type = in.readByte();
	if (type != GAME_OBJECT && type != SCREEN_MANAGER) {
		printf("This resource type does not include any scripts\n");
		return EXIT_FAILURE;
	}

	in.seek(9, SEEK_CUR);
	in.read_throwsOnError(name, sizeof(name));

	printf("\"%s\"\n", name);

	/* Object hub
	 *
	 *  4 bytes object type
	 *  4 bytes logic level
	 * 12 bytes unused
	 * 12 bytes script_id
	 * 12 bytes script_pc
	 */

	in.seek(44, SEEK_CUR);

	/* Script data
	 *
	 * 4 bytes variable space size
	 * variable space
	 * 4 bytes number of scripts
	 * offsets for each script (4 bytes each)
	 */

	size = (int32) in.readUint32LE();
	in.seek(size, SEEK_CUR);

	numScripts = (int32) in.readUint32LE();

	printf("numScripts = %d\n", numScripts);

	for (i = 0; i < numScripts; i++)
		scriptOffsets[i] = (int32) in.readUint32LE();

	/* Checksum block
	 *
	 * 4 bytes "magic number" (12345678)
	 * 4 bytes code length
	 * 4 bytes checksum
	 */

	in.seek(12, SEEK_CUR);

	scriptBase = in.pos();

	for (i = 0; i < numScripts; i++) {
		int done = 0;
		int pc = 0;

		printf("\nScript #%d\n\n", i + 1);

		in.seek(scriptBase + scriptOffsets[i], SEEK_SET);

		while (!done) {
			uint32 curCommand;
			int32 caseCount;
			int32 parameter;
			int32 value;

			printf("%04x: ", pc);

			curCommand = in.readByte();
			pc++;

			switch (curCommand) {
			case CP_END_SCRIPT:
				printf("END\n");
				done = 1;
				break;
			case CP_PUSH_LOCAL_VAR32:
				parameter = (int16) in.readUint16LE();
				pc += 2;
				printf("PUSH local(%d)\n", parameter);
				break;
			case CP_PUSH_GLOBAL_VAR32:
				parameter = (int16) in.readUint16LE();
				pc += 2;
				printf("PUSH %s\n", globalVar(parameter));
				break;
			case CP_POP_LOCAL_VAR32:
				parameter = (int16) in.readUint16LE();
				pc += 2;
				printf("POP -> local(%d)\n", parameter);
				break;
			case CP_CALL_MCODE:
				parameter = (int16) in.readUint16LE();
				value = (int8) in.readByte();
				pc += 3;
				printf("CALL %d, %s\n", value, opcodes[parameter]);
				break;
			case CP_PUSH_LOCAL_ADDR:
				parameter = (int16) in.readUint16LE();
				pc += 2;
				printf("PUSH &local(%d)\n", parameter);
				break;
			case CP_PUSH_INT32:
				parameter = (int32) in.readUint32LE();
				pc += 4;
				printf("PUSH %d\n", parameter);
				break;
			case CP_SKIPONFALSE:
				parameter = (int32) in.readUint32LE();
				printf("IFNOT POP THEN JUMP %04x\n", pc + parameter);
				pc += 4;
				break;
			case CP_SKIPALWAYS:
				parameter = (int32) in.readUint32LE();
				printf("JUMP %04x\n", pc + parameter);
				pc += 4;
				break;
			case CP_SWITCH:
				caseCount = (int32) in.readUint32LE();
				pc += 4;
				printf("SWITCH POP\n");
				for (j = 0; j < caseCount; j++) {
					int32 to;

					value = (int32) in.readUint32LE();
					to = (int32) in.readUint32LE();

					printf("----:     %-7d -> JUMP %04x\n", value, pc + to);

					pc += 8;
				}

				printf("----:     default -> JUMP %04x\n", pc + (int32) in.readUint32LE());
				pc += 4;
				break;
			case CP_ADDNPOP_LOCAL_VAR32:
				parameter = (int16) in.readUint16LE();
				pc += 2;
				printf("local(%d) += POP\n", parameter);
				break;
			case CP_SUBNPOP_LOCAL_VAR32:
				parameter = (int16) in.readUint16LE();
				pc += 2;
				printf("local(%d) -= POP\n", parameter);
				break;
			case CP_SKIPONTRUE:
				parameter = (int32) in.readUint32LE();
				printf("IF POP THEN JUMP %04x\n", pc + parameter);
				pc += 4;
				break;
			case CP_POP_GLOBAL_VAR32:
				parameter = (int16) in.readUint16LE();
				pc += 2;
				printf("%s = POP\n", globalVar(parameter));
				break;
			case CP_ADDNPOP_GLOBAL_VAR32:
				parameter = (int16) in.readUint16LE();
				pc += 2;
				printf("%s += POP\n", globalVar(parameter));
				break;
			case CP_SUBNPOP_GLOBAL_VAR32:
				parameter = (int16) in.readUint16LE();
				pc += 2;
				printf("%s -= POP\n", globalVar(parameter));
				break;
			case CP_DEBUGON:
				printf("DEBUG ON\n");
				break;
			case CP_DEBUGOFF:
				printf("DEBUG OFF\n");
				break;
			case CP_QUIT:
				printf("QUIT\n");	/* For a cycle only */
				break;
			case CP_TERMINATE:
				printf("TERMINATE\n");
				break;
			case OP_ISEQUAL:
				printf("PUSH POP == POP\n");
				break;
			case OP_PLUS:
				printf("PUSH POP + POP\n");
				break;
			case OP_MINUS:
				printf("PUSH POP - POP\n");
				break;
			case OP_TIMES:
				printf("PUSH POP * POP\n");
				break;
			case OP_DIVIDE:
				printf("PUSH POP / POP\n");
				break;
			case OP_NOTEQUAL:
				printf("PUSH POP != POP\n");
				break;
			case OP_ANDAND:
				printf("PUSH POP && POP\n");
				break;
			case OP_GTTHAN:
				printf("PUSH POP > POP\n");
				break;
			case OP_LSTHAN:
				printf("PUSH POP < POP\n");
				break;
			case CP_JUMP_ON_RETURNED:
				in.readByte();
				printf("JUMP_ON_RETURNED\n");
				pc++;
				break;
			case CP_TEMP_TEXT_PROCESS:
				in.readUint32LE();
				pc += 4;
				printf("TEMP_TEXT_PROCESS\n");
				break;
			case CP_SAVE_MCODE_START:
				printf("SAVE_MCODE_START\n");
				break;
			case CP_RESTART_SCRIPT:
				printf("RESTART_SCRIPT\n");
				break;
			case CP_PUSH_STRING:
				parameter = (int8) in.readByte();
				printf("PUSH \"");
				for (j = 0; j < parameter; j++) {
					byte c = in.readByte();
					fputc(c, stdout);
					pc++;
				}
				printf("\"\n");
				in.readByte();
				pc += 2;
				break;
			case CP_PUSH_DEREFERENCED_STRUCTURE:
				parameter = (int32) in.readUint32LE();
				pc += 4;
				printf("PUSH far(%d)\n", parameter);
				break;
			case OP_GTTHANE:
				printf("PUSH POP >= POP\n");
				break;
			case OP_LSTHANE:
				printf("PUSH POP <= POP\n");
				break;
			case OP_OROR:
				printf("PUSH POP || POP\n");
				break;
			default:
				printf("<unknown>\n");
				break;
			}
		}
	}

	return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
	// Catch exceptions
	try {
		return entry(argc, argv);
	} catch(std::exception &e) {
		printf("Fatal Error: %s\n", e.what());
	}
	return -1;
}


