/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

/* Basic Kyrandia script disassembler */

#include <stdio.h>

#include "dekyra.h"
#include "common/endian.h"
#include "common/util.h"

extern FILE *outputFile;

// command help
void printCommandsV1Ref() {
	fprintf(outputFile, "------------- Command Reference ----------------\n\n");
	fprintf(outputFile, "c1_jumpTo             - jumps to the given param\n\n");
	fprintf(outputFile, "c1_setRetValue        - sets the return value\n\n");
	fprintf(outputFile, "c1_pushRetOrPos       - dependent on param:\n");
	fprintf(outputFile, "                      - 0: pushes return value\n");
	fprintf(outputFile, "                      - 1: pushes the next instruction address and the bp and updates the bp to sp+2\n");
	fprintf(outputFile, "                      - else: breaks execution\n\n");
	fprintf(outputFile, "c1_push               - push an integer to the stack (is also be used for string indexes)\n\n");
	fprintf(outputFile, "c1_pushVar            - pushes var[param] to the stack\n\n");
	fprintf(outputFile, "c1_pushBPNeg          - pushes the value at stack[(-(param + 2))+ bp] to the stack\n\n");
	fprintf(outputFile, "c1_pushBPAdd          - pushes the value at stack[(param - 1) + bp] to the stack\n\n");
	fprintf(outputFile, "c1_popRetOrPos        - dependent on param:\n");
	fprintf(outputFile, "                      - 0: pops return value\n");
	fprintf(outputFile, "                      - 1: pops the bp and after it the ip from the stack\n");
	fprintf(outputFile, "                      - else: breaks execution\n\n");
	fprintf(outputFile, "c1_popVar             - pops the stack top to var[param]\n\n");
	fprintf(outputFile, "c1_popBPNeg           - pops the stack top to stack[-(param + 2) + bp]\n\n");
	fprintf(outputFile, "c1_popBPAdd           - pops the stack top to stack[(param - 1) + bp]\n\n");
	fprintf(outputFile, "c1_execOpcode         - executes a function and sets the return value to the return value of the called function\n\n");
	fprintf(outputFile, "c1_ifNotJmp           - jumps to address param if the stack top is zero\n\n");
	fprintf(outputFile, "c1_negate             - dependent on param:\n");
	fprintf(outputFile, "                      - 0: sets the stack top to 1 if the stack top is non-zero, if the stack top is zero it doesn't do anything\n");
	fprintf(outputFile, "                      - 1: sets the stack top to -(stack top)\n");
	fprintf(outputFile, "                      - 2: sets the stack top to ~(stack top)\n");
	fprintf(outputFile, "                      - else: breaks execution\n\n");
	fprintf(outputFile, "c1_eval               - pops val1 and val2 from the stack and does the given evaulation and pushes the result of it to the stack\n\n");
	fprintf(outputFile, "c1_setRetAndJmp       - pops the return value from the stack, after that pops the new ip from the stack and jumps to that position\n\n");
}

// commands
void c1_jumpTo(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_jumpTo 0x%.04X\n", (((uint)argument) << 1) & 0xFFFF);
}

void c1_setRetValue(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_setRetValue %d\n", argument);
}

void c1_pushRetOrPos(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_pushRetOrPos %d ", argument);
	switch (argument) {
	case 0:
	case 1:
		break;

	default:
		fprintf(outputFile, "; called with illegal param! breaks execution!");
		break;
	}
	fprintf(outputFile, "\n");
}

void c1_push(ScriptData *dataPtr, int argument) {
#define posString(x) (char*)&dataPtr->text[READ_BE_UINT16(&((uint16 *)dataPtr->text)[(x)])]
	if (argument < dataPtr->numStrings && argument > 0) {
		fprintf(outputFile, "c1_push %d ; could be string '%s'\n", argument, posString(argument));
	} else {
		fprintf(outputFile, "c1_push %d\n", argument);
	}
#undef posString
}

void c1_pushVar(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_pushVar %d\n", argument);
}

void c1_pushBPNeg(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_pushBPNeg %d\n", argument);
}

void c1_pushBPAdd(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_pushBPAdd %d\n", argument);
}

void c1_popRetOrPos(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_popRetOrPos %d ", argument);
	switch (argument) {
	case 0:
	case 1:
		break;

	default:
		fprintf(outputFile, "; called with illegal param! breaks execution!");
		break;
	}
	fprintf(outputFile, "\n");
}

void c1_popVar(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_popVar %d\n", argument);
}

void c1_popBPNeg(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_popBPNeg %d\n", argument);
}

void c1_popBPAdd(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_popBPAdd %d\n", argument);
}

void c1_addSP(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_addSP %d\n", argument);
}

void c1_subSP(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_subSP %d\n", argument);
}

void c1_execOpcode(ScriptData *script, int argument) {
	if ((uint8)argument >= (uint8)script->opcodeSize) {
		fprintf(outputFile, "c1_execOpcode %d ; tries to call illegal opcode param and breaks exectuion\n", (uint8)argument);
	} else {
		fprintf(outputFile, "c1_execOpcode %d ; functionname: '%s'\n", (uint8)argument, script->opcodes[(uint8)argument].name);
	}
}

void c1_ifNotJmp(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_ifNotJmp 0x%.04X\n", (((uint)argument) << 1) & 0xFFFF);
}

void c1_negate(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_negate %d ", argument);
	switch (argument) {
	case 0:
	case 1:
	case 2:
		break;

	default:
		fprintf(outputFile, "; called with illegal param! breaks execution");
		break;
	}
	fprintf(outputFile, "\n");
}

void c1_eval(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_eval %d ; (C syntax): '", argument);

	switch (argument) {
	case 0:
		fprintf(outputFile, "(val2 && val1)");
		break;

	case 1:
		fprintf(outputFile, "(val2 || val1)");
		break;

	case 2:
		fprintf(outputFile, "(val1 == val2)");
		break;

	case 3:
		fprintf(outputFile, "(val1 != val2)");
		break;

	case 4:
		fprintf(outputFile, "(val1 > val2)");
		break;

	case 5:
		fprintf(outputFile, "(val1 >= val2)");
		break;

	case 6:
		fprintf(outputFile, "(val1 < val2)");
		break;

	case 7:
		fprintf(outputFile, "(val1 <= val2)");
		break;

	case 8:
		fprintf(outputFile, "val1 + val2");
		break;

	case 9:
		fprintf(outputFile, "val2 - val1");
		break;

	case 10:
		fprintf(outputFile, "val1 * val2");
		break;

	case 11:
		fprintf(outputFile, "val2 / val1");
		break;

	case 12:
		fprintf(outputFile, "val2 >> val1");
		break;

	case 13:
		fprintf(outputFile, "val2 << val1");
		break;

	case 14:
		fprintf(outputFile, "val1 & val2");
		break;

	case 15:
		fprintf(outputFile, "val1 | val2");
		break;

	case 16:
		fprintf(outputFile, "val2 %% val1");
		break;

	case 17:
		fprintf(outputFile, "val1 ^ val2");
		break;

	default:
		fprintf(outputFile, "!error breaking exectution!");
		break;
	}

	fprintf(outputFile, "'\n");
}

void c1_setRetAndJmp(ScriptData *script, int argument) {
	fprintf(outputFile, "c1_setRetAndJmp\n");
}

void setupCommandsV1(Script *myScript) {
	static CommandProc commands[] = {
		&c1_jumpTo,
		&c1_setRetValue,
		&c1_pushRetOrPos,
		&c1_push,
		&c1_push,
		&c1_pushVar,
		&c1_pushBPNeg,
		&c1_pushBPAdd,
		&c1_popRetOrPos,
		&c1_popVar,
		&c1_popBPNeg,
		&c1_popBPAdd,
		&c1_addSP,
		&c1_subSP,
		&c1_execOpcode,
		&c1_ifNotJmp,
		&c1_negate,
		&c1_eval,
		&c1_setRetAndJmp
	};

	myScript->setCommands(commands, ARRAYSIZE(commands));
}

// trace commands
void c1_traceJumpTo(ScriptData *script, int argument) {
	Function *call = script->getFunction(((uint)argument) << 1);
	if (call) {
		for (int i = 0; i < call->refs; ++i) {
			if (call->refOffs[i] == script->curOffset)
				return;
		}

		if (call->refs < MAX_REFS) {
			call->refOffs[call->refs++] = script->curOffset;
		} else {
			warning("losing ref");
		}
	} else {
		if (script->numFunctions < MAX_FUNCTIONS) {
			call = &script->functions[script->numFunctions];
			call->id = -1;
			call->startOffset = ((uint)argument) << 1;
			call->refOffs[call->refs++] = script->curOffset;
			++script->numFunctions;
		} else {
			warning("losing function");
		}
	}
}

void c1_traceIfNotJmp(ScriptData *script, int argument) {
	Function *call = script->getFunction(((uint)argument) << 1);
	if (call) {
		for (int i = 0; i < call->refs; ++i) {
			if (call->refOffs[i] == script->curOffset)
				return;
		}

		if (call->refs < MAX_REFS) {
			call->refOffs[call->refs++] = script->curOffset;
		} else {
			warning("losing ref");
		}
	} else {
		if (script->numFunctions < MAX_FUNCTIONS) {
			call = &script->functions[script->numFunctions];
			call->id = -1;
			call->startOffset = ((uint)argument) << 1;
			call->refOffs[call->refs++] = script->curOffset;
			++script->numFunctions;
		} else {
			warning("losing function");
		}
	}
}

void setupTraceCommandsV1(Script *myScript) {
	static CommandProc commands[] = {
		&c1_traceJumpTo,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		&c1_traceIfNotJmp,
		0,
		0,
		0
	};

	myScript->setCommands(commands, ARRAYSIZE(commands));
}

// opcode tables
void getOpcodesV1(OpcodeEntry *&opcodes, int &opcodesSize) {
	static OpcodeEntry kyra1OpcodeDesc[] = {
		{ 0x00 ,"o1_magicInMouseItem" },
		{ 0x01 ,"o1_characterSays" },
		{ 0x02 ,"o1_pauseTicks" },
		{ 0x03 ,"o1_drawSceneAnimShape" },
		{ 0x04 ,"o1_queryGameFlag" },
		{ 0x05 ,"o1_setGameFlag" },
		{ 0x06 ,"o1_resetGameFlag" },
		{ 0x07 ,"o1_runNPCScript" },
		{ 0x08 ,"o1_setSpecialExitList" },
		{ 0x09 ,"o1_blockInWalkableRegion" },
		{ 0x0A ,"o1_blockOutWalkableRegion" },
		{ 0x0B ,"o1_walkPlayerToPoint" },
		{ 0x0C ,"o1_dropItemInScene" },
		{ 0x0D ,"o1_drawAnimShapeIntoScene" },
		{ 0x0E ,"o1_createMouseItem" },
		{ 0x0F ,"o1_savePageToDisk" },
		{ 0x10 ,"o1_sceneAnimOn" },
		{ 0x11 ,"o1_sceneAnimOff" },
		{ 0x12 ,"o1_elapsedSeconds" },
		{ 0x13 ,"o1_mouseIsPointer" },
		{ 0x14 ,"o1_destroyMouseItem" },
		{ 0x15 ,"o1_runSceneAnimUntilDone" },
		{ 0x16 ,"o1_fadeSpecialPalette" },
		{ 0x17 ,"o1_playAdLibSound" },
		{ 0x18 ,"o1_playAdLibScore" },
		{ 0x19 ,"o1_phaseInSameScene" },
		{ 0x1a ,"o1_setScenePhasingFlag" },
		{ 0x1b ,"o1_resetScenePhasingFlag" },
		{ 0x1c ,"o1_queryScenePhasingFlag" },
		{ 0x1d ,"o1_sceneToDirection" },
		{ 0x1e ,"o1_setBirthstoneGem" },
		{ 0x1f ,"o1_rlaceItemInGenericMapScene" },
		{ 0x20 ,"o1_setBrandonStatusBit" },
		{ 0x21 ,"o1_pauseSeconds" },
		{ 0x22 ,"o1_getCharactersLocation" },
		{ 0x23 ,"o1_runNPCSubscript" },
		{ 0x24 ,"o1_magicOutMouseItem" },
		{ 0x25 ,"o1_internalAnimOn" },
		{ 0x26 ,"o1_forceBrandonToNormal" },
		{ 0x27 ,"o1_poisonDeathNow" },
		{ 0x28 ,"o1_setScaleMode" },
		{ 0x29 ,"o1_openWSAFile" },
		{ 0x2a ,"o1_closeWSAFile" },
		{ 0x2b ,"o1_runWSAFromBeginningToEnd" },
		{ 0x2c ,"o1_displayWSAFrame" },
		{ 0x2d ,"o1_enterNewScene" },
		{ 0x2e ,"o1_setSpecialEnterXAndY" },
		{ 0x2f ,"o1_runWSAFrames" },
		{ 0x30 ,"o1_popBrandonIntoScene" },
		{ 0x31 ,"o1_restoreAllObjectBackgrounds" },
		{ 0x32 ,"o1_setCustomPaletteRange" },
		{ 0x33 ,"o1_loadPageFromDisk" },
		{ 0x34 ,"o1_customPrintTalkString" },
		{ 0x35 ,"o1_restoreCustomPrintBackground" },
		{ 0x36 ,"o1_hideMouse" },
		{ 0x37 ,"o1_showMouse" },
		{ 0x38 ,"o1_getCharacterX" },
		{ 0x39 ,"o1_getCharacterY" },
		{ 0x3A ,"o1_changeCharactersFacing" },
		{ 0x3B ,"o1_copyWSARegion" },
		{ 0x3C ,"o1_textPrint" },
		{ 0x3D ,"o1_random" },
		{ 0x3E ,"o1_loadSoundFile" },
		{ 0x3F ,"o1_displayWSAFrameOnHidPage" },
		{ 0x40 ,"o1_displayWSASequentialFrames" },
		{ 0x41 ,"o1_drawCharacterStanding" },
		{ 0x42 ,"o1_internalAnimOff" },
		{ 0x43 ,"o1_changeCharactersXAndY" },
		{ 0x44 ,"o1_clearSceneAnimatorBeacon" },
		{ 0x45 ,"o1_querySceneAnimatorBeacon" },
		{ 0x46 ,"o1_refreshSceneAnimator" },
		{ 0x47 ,"o1_placeItemInOffScene" },
		{ 0x48 ,"o1_wipeDownMouseItem" },
		{ 0x49 ,"o1_placeCharacterInOtherScene" },
		{ 0x4A ,"o1_getKey" },
		{ 0x4B ,"o1_specificItemInInventory" },
		{ 0x4C ,"o1_popMobileNPCIntoScene" },
		{ 0x4D ,"o1_mobileCharacterInScene" },
		{ 0x4E ,"o1_hideMobileCharacter" },
		{ 0x4F ,"o1_unhideMobileCharacter" },
		{ 0x50 ,"o1_setCharactersLocation" },
		{ 0x51 ,"o1_walkCharacterToPoint" },
		{ 0x52 ,"o1_specialEventDisplayBrynnsNote" },
		{ 0x53 ,"o1_specialEventRemoveBrynnsNote" },
		{ 0x54 ,"o1_setLogicPage" },
		{ 0x55 ,"o1_fatPrint" },
		{ 0x56 ,"o1_preserveAllObjectBackgrounds" },
		{ 0x57 ,"o1_updateSceneAnimations" },
		{ 0x58 ,"o1_sceneAnimationActive" },
		{ 0x59 ,"o1_setCharactersMovementDelay" },
		{ 0x5A ,"o1_getCharactersFacing" },
		{ 0x5B ,"o1_bkgdScrollSceneAndMasksRight" },
		{ 0x5C ,"o1_dispelMagicAnimation" },
		{ 0x5d ,"o1_findBrightestFireberry" },
		{ 0x5e ,"o1_setFireberryGlowPalette" },
		{ 0x5f ,"o1_setDeathHandlerFlag" },
		{ 0x60 ,"o1_drinkPotionAnimation" },
		{ 0x61 ,"o1_makeAmuletAppear" },
		{ 0x62 ,"o1_drawItemShapeIntoScene" },
		{ 0x63 ,"o1_setCharactersCurrentFrame" },
		{ 0x64 ,"o1_waitForConfirmationMouseClick" },
		{ 0x65 ,"o1_pageFlip" },
		{ 0x66 ,"o1_setSceneFile" },
		{ 0x67 ,"o1_whatItemInMarbleVase" },
		{ 0x68 ,"o1_setItemInMarbleVase" },
		{ 0x69 ,"o1_addItemToInventory" },
		{ 0x6a ,"o1_intPrint" },
		{ 0x6b ,"o1_shakeScreen" },
		{ 0x6c ,"o1_createAmuletJewel" },
		{ 0x6d ,"o1_setSceneAnimCurrXY" },
		{ 0x6e ,"o1_poisonBrandonAndRemaps" },
		{ 0x6f ,"o1_fillFlaskWithWater" },
		{ 0x70 ,"o1_getCharactersMovementDelay" },
		{ 0x71 ,"o1_getBirthstoneGem" },
		{ 0x72 ,"o1_queryBrandonStatusBit" },
		{ 0x73 ,"o1_playFluteAnimation" },
		{ 0x74 ,"o1_playWinterScrollSequence" },
		{ 0x75 ,"o1_getIdolGem" },
		{ 0x76 ,"o1_setIdolGem" },
		{ 0x77 ,"o1_totalItemsInScene" },
		{ 0x78 ,"o1_restoreBrandonsMovementDelay" },
		{ 0x79 ,"o1_setMousePos" },
		{ 0x7a ,"o1_getMouseState" },
		{ 0x7b ,"o1_setEntranceMouseCursorTrack" },
		{ 0x7c ,"o1_itemAppearsOnGround" },
		{ 0x7d ,"o1_setNoDrawShapesFlag" },
		{ 0x7e ,"o1_fadeEntirePalette" },
		{ 0x7f ,"o1_itemOnGroundHere" },
		{ 0x80 ,"o1_queryCauldronState" },
		{ 0x81 ,"o1_setCauldronState" },
		{ 0x82 ,"o1_queryCrystalState" },
		{ 0x83 ,"o1_setCrystalState" },
		{ 0x84 ,"o1_setPaletteRange" },
		{ 0x85 ,"o1_shrinkBrandonDown" },
		{ 0x86 ,"o1_growBrandonUp" },
		{ 0x87 ,"o1_setBrandonScaleXAndY" },
		{ 0x88 ,"o1_resetScaleMode" },
		{ 0x89 ,"o1_getScaleDepthTableValue" },
		{ 0x8a ,"o1_setScaleDepthTableValue" },
		{ 0x8b ,"o1_message" },
		{ 0x8c ,"o1_checkClickOnNPC" },
		{ 0x8d ,"o1_getFoyerItem" },
		{ 0x8e ,"o1_setFoyerItem" },
		{ 0x8F ,"o1_setNoItemDropRegion" },
		{ 0x90 ,"o1_walkMalcolmOn" },
		{ 0x91 ,"o1_passiveProtection" },
		{ 0x92 ,"o1_setPlayingLoop" },
		{ 0x93 ,"o1_brandonToStoneSequence" },
		{ 0x94 ,"o1_brandonHealingSequence" },
		{ 0x95 ,"o1_protectCommandLine" },
		{ 0x96 ,"o1_pauseMusicSeconds" },
		{ 0x97 ,"o1_resetMaskRegion" },
		{ 0x98 ,"o1_setPaletteChangeFlag" },
		{ 0x99 ,"o1_fillRect" },
		{ 0x9a ,"o1_vocUnload" },
		{ 0x9b ,"o1_vocLoad" },
		{ 0x9c ,"o1_dummy" }
	};

	opcodes = kyra1OpcodeDesc;
	opcodesSize = ARRAYSIZE(kyra1OpcodeDesc);
}
