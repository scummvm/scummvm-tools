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

#include "engines/twine/lba1.h"
#include <stdio.h>
#include <string.h>

#include "common/array.h"
#include "common/util.h"
#include "common/memstream.h"

struct ScriptContext {
	Common::MemoryReadStream stream;
	Common::Array<int> offsets;
	int level;
	int comportmentId;
	bool comportement;
};

typedef void ScriptFunc(ScriptContext &ctx);

struct ScriptFunction {
	const char *name;
	ScriptFunc *function;
};

#define MAPFUNC(name, func) \
	{ name, func }

static const int initialLevel = 2;
static const int indentWidth = 2;

static void mEND(ScriptContext &ctx) {
	printf("%*sEND\n", ctx.level, " ");
}

static void mNOP(ScriptContext &ctx) {
	printf("%*sNOP\n", ctx.level, " ");
}

static void mBODY(ScriptContext &ctx) {
	printf("%*sBODY %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void mANIM(ScriptContext &ctx) {
	printf("%*sANIM %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void mGOTO_POINT(ScriptContext &ctx) {
	printf("%*sGOTO_POINT %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void mWAIT_ANIM(ScriptContext &ctx) {
	printf("%*sWAIT_ANIM\n", ctx.level, " ");
}

static void mLOOP(ScriptContext &ctx) {
	printf("%*sLOOP\n", ctx.level, " ");
}

static void mANGLE(ScriptContext &ctx) {
	printf("%*sANGLE %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mPOS_POINT(ScriptContext &ctx) {
	printf("%*sPOS_POINT %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void mLABEL(ScriptContext &ctx) {
	ctx.level = initialLevel;
	printf("%*sLABEL %i\n", ctx.level, " ", (int)ctx.stream.readByte());
	ctx.level += indentWidth;
}

static void mGOTO(ScriptContext &ctx) {
	printf("%*sGOTO_SYM_POINT %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mSTOP(ScriptContext &ctx) {
	printf("%*sSTOP\n", ctx.level, " ");
}

static void mGOTO_SYM_POINT(ScriptContext &ctx) {
	printf("%*sGOTO_SYM_POINT %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void mWAIT_NUM_ANIM(ScriptContext &ctx) {
	const int32 animRepeats = ctx.stream.readByte();
	const int32 animPos = ctx.stream.readByte();
	printf("%*sWAIT_NUM_ANIM %i %i\n", ctx.level, " ", animRepeats, animPos);
}

static void mSAMPLE(ScriptContext &ctx) {
	printf("%*sSAMPLE %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mGOTO_POINT_3D(ScriptContext &ctx) {
	printf("%*sGOTO_POINT_3D %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void mSPEED(ScriptContext &ctx) {
	printf("%*sSPEED %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mBACKGROUND(ScriptContext &ctx) {
	printf("%*sBACKGROUND %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void mWAIT_NUM_SECOND(ScriptContext &ctx) {
	printf("%*sWAIT_NUM_SECOND %i %i\n", ctx.level, " ", (int)ctx.stream.readByte(), ctx.stream.readSint32LE());
}

static void mNO_BODY(ScriptContext &ctx) {
	printf("%*sNO_BODY\n", ctx.level, " ");
}

static void mBETA(ScriptContext &ctx) {
	printf("%*sBETA %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mOPEN_LEFT(ScriptContext &ctx) {
	printf("%*sOPEN_LEFT %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mOPEN_RIGHT(ScriptContext &ctx) {
	printf("%*sOPEN_RIGHT %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mOPEN_UP(ScriptContext &ctx) {
	printf("%*sOPEN_UP %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mOPEN_DOWN(ScriptContext &ctx) {
	printf("%*sOPEN_DOWN %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mCLOSE(ScriptContext &ctx) {
	printf("%*sCLOSE\n", ctx.level, " ");
}

static void mWAIT_DOOR(ScriptContext &ctx) {
	printf("%*sWAIT_DOOR\n", ctx.level, " ");
}

static void mSAMPLE_RND(ScriptContext &ctx) {
	printf("%*sSAMPLE_RND %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mSAMPLE_ALWAYS(ScriptContext &ctx) {
	printf("%*sSAMPLE_ALWAYS %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mSAMPLE_STOP(ScriptContext &ctx) {
	printf("%*sSAMPLE_STOP %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mPLAY_FLA(ScriptContext &ctx) {
	int strIdx = 0;
	char movie[64];
	do {
		const byte c = ctx.stream.readByte();
		movie[strIdx++] = c;
		if (c == '\0') {
			break;
		}
		if (strIdx >= ARRAYSIZE(movie)) {
			error("Max string size exceeded for fla name");
		}
	} while (true);

	printf("%*sPLAY_FLA %s\n", ctx.level, " ", movie);
}

static void mREPEAT_SAMPLE(ScriptContext &ctx) {
	printf("%*sREPEAT_SAMPLE %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mSIMPLE_SAMPLE(ScriptContext &ctx) {
	printf("%*sSIMPLE_SAMPLE %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mFACE_HERO(ScriptContext &ctx) {
	printf("%*sFACE_HERO %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void mANGLE_RND(ScriptContext &ctx) {
	const int16 val1 = ctx.stream.readSint16LE();
	const int16 val2 = ctx.stream.readSint16LE();
	printf("%*sANGLE_RND %i %i\n", ctx.level, " ", (int)val1, (int)val2);
}

static const ScriptFunction moveScriptFunctions[] = {
	/*0x00*/ MAPFUNC("END", mEND),
	/*0x01*/ MAPFUNC("NOP", mNOP),
	/*0x02*/ MAPFUNC("BODY", mBODY),
	/*0x03*/ MAPFUNC("ANIM", mANIM),
	/*0x04*/ MAPFUNC("GOTO_POINT", mGOTO_POINT),
	/*0x05*/ MAPFUNC("WAIT_ANIM", mWAIT_ANIM),
	/*0x06*/ MAPFUNC("LOOP", mLOOP),
	/*0x07*/ MAPFUNC("ANGLE", mANGLE),
	/*0x08*/ MAPFUNC("POS_POINT", mPOS_POINT),
	/*0x09*/ MAPFUNC("LABEL", mLABEL),
	/*0x0A*/ MAPFUNC("GOTO", mGOTO),
	/*0x0B*/ MAPFUNC("STOP", mSTOP),
	/*0x0C*/ MAPFUNC("GOTO_SYM_POINT", mGOTO_SYM_POINT),
	/*0x0D*/ MAPFUNC("WAIT_NUM_ANIM", mWAIT_NUM_ANIM),
	/*0x0E*/ MAPFUNC("SAMPLE", mSAMPLE),
	/*0x0F*/ MAPFUNC("GOTO_POINT_3D", mGOTO_POINT_3D),
	/*0x10*/ MAPFUNC("SPEED", mSPEED),
	/*0x11*/ MAPFUNC("BACKGROUND", mBACKGROUND),
	/*0x12*/ MAPFUNC("WAIT_NUM_SECOND", mWAIT_NUM_SECOND),
	/*0x13*/ MAPFUNC("NO_BODY", mNO_BODY),
	/*0x14*/ MAPFUNC("BETA", mBETA),
	/*0x15*/ MAPFUNC("OPEN_LEFT", mOPEN_LEFT),
	/*0x16*/ MAPFUNC("OPEN_RIGHT", mOPEN_RIGHT),
	/*0x17*/ MAPFUNC("OPEN_UP", mOPEN_UP),
	/*0x18*/ MAPFUNC("OPEN_DOWN", mOPEN_DOWN),
	/*0x19*/ MAPFUNC("CLOSE", mCLOSE),
	/*0x1A*/ MAPFUNC("WAIT_DOOR", mWAIT_DOOR),
	/*0x1B*/ MAPFUNC("SAMPLE_RND", mSAMPLE_RND),
	/*0x1C*/ MAPFUNC("SAMPLE_ALWAYS", mSAMPLE_ALWAYS),
	/*0x1D*/ MAPFUNC("SAMPLE_STOP", mSAMPLE_STOP),
	/*0x1E*/ MAPFUNC("PLAY_FLA", mPLAY_FLA),
	/*0x1F*/ MAPFUNC("REPEAT_SAMPLE", mREPEAT_SAMPLE),
	/*0x20*/ MAPFUNC("SIMPLE_SAMPLE", mSIMPLE_SAMPLE),
	/*0x21*/ MAPFUNC("FACE_HERO", mFACE_HERO),
	/*0x22*/ MAPFUNC("ANGLE_RND", mANGLE_RND)
};


/** Script condition operators */
static const char *LifeScriptOperators[] = {
	"==",
	">",
	"<",
	">=",
	"<=",
	"!="
};

/** Script condition command opcodes */
enum LifeScriptConditions {
	/*0x00*/ kcCOL = 0,              /*<! Current actor collision with another actor. (Parameter = Actor Index) */
	/*0x01*/ kcCOL_OBJ = 1,          /*<! Actor collision with the actor passed as parameter. (Parameter = Actor Index, Parameter = Actor Index) */
	/*0x02*/ kcDISTANCE = 2,         /*<! Distance between the current actor and the actor passed as parameter. (Parameter = Actor Index, Parameter = Distance between) */
	/*0x03*/ kcZONE = 3,             /*<! Current actor tread on zone passed as parameter. (Parameter = Zone Index) */
	/*0x04*/ kcZONE_OBJ = 4,         /*<! The actor passed as parameter will tread on zone passed as parameter. (Parameter = Actor Index, Parameter = Zone Index) */
	/*0x05*/ kcBODY = 5,             /*<! Body of the current actor. (Parameter = Body Index) */
	/*0x06*/ kcBODY_OBJ = 6,         /*<! Body of the actor passed as parameter. (Parameter = Body Index) */
	/*0x07*/ kcANIM = 7,             /*<! Body Animation of the current actor. (Parameter = Animation Index) */
	/*0x08*/ kcANIM_OBJ = 8,         /*<! Body Animation of the actor passed as parameter. (Parameter = Animation Index) */
	/*0x09*/ kcL_TRACK = 9,          /*<! Current actor track. (Parameter = Track Index) */
	/*0x0A*/ kcL_TRACK_OBJ = 10,     /*<! Track of the actor passed as parameter. (Parameter = Track Index) */
	/*0x0B*/ kcFLAG_CUBE = 11,       /*<! Game Cube Flags. (Parameter = Cube Flag Index, Parameter = 0 (not set), = 1 (set))k */
	/*0x0C*/ kcCONE_VIEW = 12,       /*<! The actor passed as parameter have a "vision in circle". (Parameter = Actor Index, Parameter = Distance) */
	/*0x0D*/ kcHIT_BY = 13,          /*<! Current actor hited by the actor passed as parameter. (Parameter = Actor Index) */
	/*0x0E*/ kcACTION = 14,          /*<! Hero action behavior. (Parameter = Behaviour Index) */
	/*0x0F*/ kcFLAG_GAME = 15,       /*<! Game Flags (See further list). (Parameter = Flag Index, Parameter = 0 (not set), = 1 (set)) */
	/*0x10*/ kcLIFE_POINT = 16,      /*<! Current actor life points. (Parameter = Life points) */
	/*0x11*/ kcLIFE_POINT_OBJ = 17,  /*<! Life points of the current actor passed as parameter. (Parameter = Life points) */
	/*0x12*/ kcNUM_LITTLE_KEYS = 18, /*<! Number of keys. (Parameter = Number of keys) */
	/*0x13*/ kcNUM_GOLD_PIECES = 19, /*<! Coins/Gold Amount. (Parameter = Coins/Gold amount) */
	/*0x14*/ kcBEHAVIOUR = 20,       /*<! Hero behaviour. (Parameter = Behaviour Index) */
	/*0x15*/ kcCHAPTER = 21,         /*<! Story Chapters. (Parameter = Chapter Index) */
	/*0x16*/ kcDISTANCE_3D = 22,     /*<! Distance between the actor passed as parameter and the current actor. (Parameter = Actor Index, Parameter = Distance) */
	/*0x17*/ kcMAGIC_LEVEL = 23,
	/*0x18*/ kcMAGIC_POINTS = 24,
	/*0x19*/ kcUSE_INVENTORY = 25,   /*<! Use inventory object. (Parameter = Object Index in the inventory, Paramenter = 0 (Not in Inventory), = 1 (In the Inventory)) */
	/*0x1A*/ kcCHOICE = 26,          /*<! Menu choice. (Parameter = Text Index in the current Text Bank) */
	/*0x1B*/ kcFUEL = 27,            /*<! Amount of fuel gas the Hero have in his inventory. (Parameter = Gas amount) */
	/*0x1C*/ kcCARRIED_BY = 28,      /*<! The current is carried by the actor passed as paramenter. (Parameter = Actor Index) */
	/*0x1D*/ kcCDROM = 29            /*<! CDROM audio tracks. (Parameter = Audio Tracks Index) */
};

static int32 processLifeConditions(ScriptContext &ctx) {
	int32 conditionValueSize = 1;
	int32 conditionOpcode = ctx.stream.readByte();
	switch (conditionOpcode) {
	case kcCOL:
		printf("collision");
		break;
	case kcCOL_OBJ: {
		int32 actorIdx = ctx.stream.readByte();
		printf("col_obj %i", actorIdx);
		break;
	}
	case kcDISTANCE: {
		int32 actorIdx = ctx.stream.readByte();
		printf("distance %i", actorIdx);
		conditionValueSize = 2;
		break;
	}
	case kcZONE:
		printf("zone");
		break;
	case kcZONE_OBJ: {
		int32 actorIdx = ctx.stream.readByte();
		printf("zone_obj %i", actorIdx);
		break;
	}
	case kcBODY:
		printf("body");
		break;
	case kcBODY_OBJ: {
		int32 actorIdx = ctx.stream.readByte();
		printf("body_obj %i", actorIdx);
		break;
	}
	case kcANIM:
		printf("anim");
		break;
	case kcANIM_OBJ: {
		int32 actorIdx = ctx.stream.readByte();
		printf("anim_obj %i", actorIdx);
		break;
	}
	case kcL_TRACK:
		printf("track");
		break;
	case kcL_TRACK_OBJ: {
		int32 actorIdx = ctx.stream.readByte();
		printf("track_obj %i", actorIdx);
		break;
	}
	case kcFLAG_CUBE: {
		int32 flagIdx = ctx.stream.readByte();
		printf("flag_cube %i", flagIdx);
		break;
	}
	case kcCONE_VIEW: {
		int32 targetActorIdx = ctx.stream.readByte();
		printf("cone_view %i", targetActorIdx);
		break;
	}
	case kcHIT_BY:
		printf("hit_by");
		break;
	case kcACTION:
		printf("action");
		break;
	case kcFLAG_GAME: {
		int32 flagIdx = ctx.stream.readByte();
		printf("flag_game %i", flagIdx);
		break;
	}
	case kcLIFE_POINT:
		printf("life_point");
		break;
	case kcLIFE_POINT_OBJ: {
		int32 actorIdx = ctx.stream.readByte();
		printf("life_point_obj %i", actorIdx);
		break;
	}
	case kcNUM_LITTLE_KEYS:
		printf("num_little_keys");
		break;
	case kcNUM_GOLD_PIECES:
		printf("num_gold_pieces");
		conditionValueSize = 2;
		break;
	case kcBEHAVIOUR:
		printf("behaviour");
		break;
	case kcCHAPTER:
		printf("chapter");
		break;
	case kcDISTANCE_3D: {
		int32 targetActorIdx = ctx.stream.readByte();
		printf("distance_3d %i ", targetActorIdx);
		conditionValueSize = 2;
		break;
	}
	case kcMAGIC_LEVEL:
		printf("magic_level");
		break;
	case kcMAGIC_POINTS:
		printf("magic_points");
		break;
	case kcUSE_INVENTORY: {
		int32 item = ctx.stream.readByte();
		printf("use_inventory %i", item);
		break;
	}
	case kcCHOICE:
		printf("choice");
		conditionValueSize = 2;
		break;
	case kcFUEL:
		printf("fuel");
		break;
	case kcCARRIED_BY:
		printf("carried_by");
		break;
	case kcCDROM:
		printf("cdrom");
		break;
	default:
		error("Actor condition opcode %d", conditionOpcode);
		break;
	}

	return conditionValueSize;
}

static void processLifeOperators(ScriptContext &ctx, int32 valueSize) {
	const uint8 operatorCode = ctx.stream.readByte();
	if (operatorCode >= ARRAYSIZE(LifeScriptOperators)) {
		error("Invalid operator %i", (int)operatorCode);
	}
	printf(" %s ", LifeScriptOperators[operatorCode]);
	int32 conditionValue;
	if (valueSize == 1) {
		conditionValue = ctx.stream.readByte();
	} else if (valueSize == 2) {
		conditionValue = ctx.stream.readSint16LE();
	} else {
		error("Unknown operator value size %d", valueSize);
	}
	printf("%i", conditionValue);
}

static void lEMPTY(ScriptContext &ctx) {
	printf("%*sEMPTY\n", ctx.level, " ");
}

static void lEND(ScriptContext &ctx) {
	printf("%*sEND\n", ctx.level, " ");
	ctx.level -= indentWidth;
}

static void lNOP(ScriptContext &ctx) {
	printf("%*sNOP\n", ctx.level, " ");
}

static void lOFFSET(ScriptContext &ctx) {
	const int16 offset = ctx.stream.readSint16LE();
	printf("%*sOFFSET %i\n", ctx.level, " ", (int)offset);
}

static void lSNIF(ScriptContext &ctx) {
	printf("%*sSWITCH_NO_IF ", ctx.level, " ");
	const int32 valueSize = processLifeConditions(ctx);
	processLifeOperators(ctx, valueSize);
	printf("\n");
	const int16 offset = ctx.stream.readSint16LE();
	ctx.offsets.push_back(offset);
	ctx.level += indentWidth;
}

static void lNEVERIF(ScriptContext &ctx) {
	printf("%*sNEVER_IF ", ctx.level, " ");
	const int32 valueSize = processLifeConditions(ctx);
	processLifeOperators(ctx, valueSize);
	const int16 offset = ctx.stream.readSint16LE();
	ctx.offsets.push_back(offset);
	printf("\n");
	ctx.level += indentWidth;
}

static void lOR_IF(ScriptContext &ctx) {
	printf("%*sOR_IF ", ctx.level, " ");
	const int32 valueSize = processLifeConditions(ctx);
	processLifeOperators(ctx, valueSize);
	const int16 offset = ctx.stream.readSint16LE();
	printf("\n");
}

static void lNO_IF(ScriptContext &ctx) {
	printf("%*sNO_IF\n", ctx.level, " ");
	ctx.level += indentWidth;
}

static void lIF(ScriptContext &ctx) {
	printf("%*sIF ", ctx.level, " ");
	const int32 valueSize = processLifeConditions(ctx);
	processLifeOperators(ctx, valueSize);
	const int16 offset = ctx.stream.readSint16LE();
	ctx.offsets.push_back(offset);
	printf("\n");
	ctx.level += indentWidth;
}

static void lSWIF(ScriptContext &ctx) {
	printf("%*sSWITCH_IF ", ctx.level, " ");
	const int32 valueSize = processLifeConditions(ctx);
	processLifeOperators(ctx, valueSize);
	const int16 offset = ctx.stream.readSint16LE();
	ctx.offsets.push_back(offset);
	printf("\n");
	ctx.level += indentWidth;
}

static void lONEIF(ScriptContext &ctx) {
	printf("%*sONEIF ", ctx.level, " ");
	const int32 valueSize = processLifeConditions(ctx);
	processLifeOperators(ctx, valueSize);
	const int16 offset = ctx.stream.readSint16LE();
	ctx.offsets.push_back(offset);
	printf("\n");
	ctx.level += indentWidth;
}

static void lELSE(ScriptContext &ctx) {
	const int16 offset = ctx.stream.readSint16LE();
	ctx.offsets.push_back(ctx.stream.pos());
	ctx.level -= indentWidth;
	printf("%*sELSE (offset %i)\n", ctx.level, " ", offset);
	ctx.level += indentWidth;
}

static void lLABEL(ScriptContext &ctx) {
	printf("%*sLABEL %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lRETURN(ScriptContext &ctx) {
	printf("%*sRETURN\n", ctx.level, " ");
	ctx.level -= indentWidth;
}

static void lBODY(ScriptContext &ctx) {
	const int32 bodyIdx = ctx.stream.readByte();
	printf("%*sBODY %i\n", ctx.level, " ", (int)bodyIdx);
}

static void lBODY_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int32 otherBodyIdx = ctx.stream.readByte();
	printf("%*sBODY_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)otherBodyIdx);
}

static void lANIM(ScriptContext &ctx) {
	const int32 animIdx = ctx.stream.readByte();
	printf("%*sANIM %i\n", ctx.level, " ", (int)animIdx);
}

static void lANIM_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int32 otherAnimIdx = ctx.stream.readByte();
	printf("%*sANIM_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)otherAnimIdx);
}

static void lSET_LIFE(ScriptContext &ctx) {
	const int16 offset = ctx.stream.readSint16LE();
	printf("%*sSET_LIFE %i\n", ctx.level, " ", (int)offset);
}

static void lSET_LIFE_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int16 offset = ctx.stream.readSint16LE();
	printf("%*sSET_LIFE_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)offset);
}

static void lSET_TRACK(ScriptContext &ctx) {
	const int16 offset = ctx.stream.readSint16LE();
	printf("%*sSET_TRACK %i\n", ctx.level, " ", (int)offset);
}

static void lSET_TRACK_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int16 offset = ctx.stream.readSint16LE();
	printf("%*sSET_TRACK_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)offset);
}

static void lMESSAGE(ScriptContext &ctx) {
	printf("%*sMESSAGE %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void lFALLABLE(ScriptContext &ctx) {
	printf("%*sFALLABLE %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lSET_DIRMODE(ScriptContext &ctx) {
	const int32 controlMode = ctx.stream.readByte();
	if (controlMode == 2) { // kFollow
		printf("%*sSET_DIRMODE %i %i\n", ctx.level, " ", (int)controlMode, (int)ctx.stream.readByte());
	} else {
		printf("%*sSET_DIRMODE %i\n", ctx.level, " ", (int)controlMode);
	}
}

static void lSET_DIRMODE_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int32 controlMode = ctx.stream.readByte();

	if (controlMode == 2) {
		int32 followedActor = ctx.stream.readByte();
		printf("%*sSET_DIRMODE_OBJ %i %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)controlMode, (int)followedActor);
	} else {
		printf("%*sSET_DIRMODE_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)controlMode);
	}
}

static void lCAM_FOLLOW(ScriptContext &ctx) {
	printf("%*sCAM_FOLLOW %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lSET_BEHAVIOUR(ScriptContext &ctx) {
	const int32 behavior = ctx.stream.readByte();
	printf("%*sSET_BEHAVIOUR %i\n", ctx.level, " ", (int)behavior);
}

static void lSET_FLAG_CUBE(ScriptContext &ctx) {
	const int32 flagIdx = ctx.stream.readByte();
	const int32 flagValue = ctx.stream.readByte();
	printf("%*sSET_FLAG_CUBE %i %i\n", ctx.level, " ", (int)flagIdx, (int)flagValue);
}

static void lCOMPORTEMENT(ScriptContext &ctx) {
	printf("%*sCOMPORTEMENT %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lSET_COMPORTEMENT(ScriptContext &ctx) {
	printf("%*sSET_COMPORTEMENT %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void lSET_COMPORTEMENT_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int16 pos = ctx.stream.readSint16LE();
	printf("%*sSET_COMPORTEMENT_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)pos);
}

static void lEND_COMPORTEMENT(ScriptContext &ctx) {
	ctx.level -= indentWidth;
	printf("%*sEND_COMPORTEMENT\n", ctx.level, " ");
	ctx.comportement = false;
}

static void lSET_FLAG_GAME(ScriptContext &ctx) {
	const uint8 flagIdx = ctx.stream.readByte();
	const uint8 flagValue = ctx.stream.readByte();
	printf("%*sSET_FLAG_GAME %i %i\n", ctx.level, " ", (int)flagIdx, (int)flagValue);
}

static void lKILL_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	printf("%*slKILL_OBJ %i\n", ctx.level, " ", (int)otherActorIdx);
}

static void lSUICIDE(ScriptContext &ctx) {
	printf("%*sSUICIDE\n", ctx.level, " ");
}

static void lUSE_ONE_LITTLE_KEY(ScriptContext &ctx) {
	printf("%*sUSE_ONE_LITTLE_KEY\n", ctx.level, " ");
}

static void lGIVE_GOLD_PIECES(ScriptContext &ctx) {
	const int16 kashes = ctx.stream.readSint16LE();
	printf("%*sGIVE_GOLD_PIECES %i\n", ctx.level, " ", (int)kashes);
}

static void lEND_LIFE(ScriptContext &ctx) {
	printf("%*sEND_LIFE\n", ctx.level, " ");
}

static void lSTOP_L_TRACK(ScriptContext &ctx) {
	printf("%*sSTOP_L_TRACK\n", ctx.level, " ");
}

static void lRESTORE_L_TRACK(ScriptContext &ctx) {
	printf("%*sRESTORE_L_TRACK\n", ctx.level, " ");
}

static void lMESSAGE_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int32 int32x = ctx.stream.readSint16LE();
	printf("%*sMESSAGE_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)int32x);
}

static void lINC_CHAPTER(ScriptContext &ctx) {
	printf("%*sINC_CHAPTER\n", ctx.level, " ");
}

static void lFOUND_OBJECT(ScriptContext &ctx) {
	printf("%*sFOUND_OBJECT %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lSET_DOOR_LEFT(ScriptContext &ctx) {
	const int32 distance = ctx.stream.readSint16LE();
	printf("%*sSET_DOOR_LEFT %i\n", ctx.level, " ", (int)distance);
}

static void lSET_DOOR_RIGHT(ScriptContext &ctx) {
	const int32 distance = ctx.stream.readSint16LE();
	printf("%*sSET_DOOR_RIGHT %i\n", ctx.level, " ", (int)distance);
}

static void lSET_DOOR_UP(ScriptContext &ctx) {
	const int32 distance = ctx.stream.readSint16LE();
	printf("%*sSET_DOOR_UP %i\n", ctx.level, " ", (int)distance);
}

static void lSET_DOOR_DOWN(ScriptContext &ctx) {
	const int32 distance = ctx.stream.readSint16LE();
	printf("%*sSET_DOOR_DOWN %i\n", ctx.level, " ", (int)distance);
}

static void lGIVE_BONUS(ScriptContext &ctx) {
	const int32 flag = ctx.stream.readByte();
	printf("%*sGIVE_BONUS %i\n", ctx.level, " ", (int)flag);
}

static void lCHANGE_CUBE(ScriptContext &ctx) {
	const int32 sceneIdx = ctx.stream.readByte();
	printf("%*sCHANGE_CUBE %i\n", ctx.level, " ", (int)sceneIdx);
}

static void lOBJ_COL(ScriptContext &ctx) {
	const int32 collision = ctx.stream.readByte();
	printf("%*sOBJ_COL %i\n", ctx.level, " ", (int)collision);
}

static void lBRICK_COL(ScriptContext &ctx) {
	const int32 collision = ctx.stream.readByte();
	printf("%*sBRICK_COL %i\n", ctx.level, " ", (int)collision);
}

static void lINVISIBLE(ScriptContext &ctx) {
	printf("%*sINVISIBLE %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lZOOM(ScriptContext &ctx) {
	const int zoomScreen = ctx.stream.readByte();
	printf("%*sZOOM %i\n", ctx.level, " ", zoomScreen);
}

static void lPOS_POINT(ScriptContext &ctx) {
	const int32 trackIdx = ctx.stream.readByte();
	printf("%*sPOS_POINT %i\n", ctx.level, " ", (int)trackIdx);
}

static void lSET_MAGIC_LEVEL(ScriptContext &ctx) {
	printf("%*sSET_MAGIC_LEVEL %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lSUB_MAGIC_POINT(ScriptContext &ctx) {
	const int16 magicPoints = (int16)ctx.stream.readByte();
	printf("%*sSET_MAGIC_POINT %i\n", ctx.level, " ", (int)magicPoints);
}

static void lSET_LIFE_POINT_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int32 lifeValue = ctx.stream.readByte();
	printf("%*sSET_LIFE_POINT_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)lifeValue);
}

static void lSUB_LIFE_POINT_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int32 lifeValue = ctx.stream.readByte();
	printf("%*sSUB_LIFE_POINT_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)lifeValue);
}

static void lHIT_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int32 strengthOfHit = ctx.stream.readByte();
	printf("%*sHIT_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)strengthOfHit);
}

static void lPLAY_FLA(ScriptContext &ctx) {
	int strIdx = 0;
	char movie[64];
	do {
		const byte c = ctx.stream.readByte();
		movie[strIdx++] = c;
		if (c == '\0') {
			break;
		}
		if (strIdx >= ARRAYSIZE(movie)) {
			error("Max string size exceeded for fla name");
		}
	} while (true);
	printf("%*sPLAY_FLA %s\n", ctx.level, " ", movie);
}

static void lPLAY_MIDI(ScriptContext &ctx) {
	const int32 midiIdx = ctx.stream.readByte();
	printf("%*sPLAY_MIDI %i\n", ctx.level, " ", (int)midiIdx);
}

static void lINC_CLOVER_BOX(ScriptContext &ctx) {
	printf("%*sINC_CLOVER_BOX\n", ctx.level, " ");
}

static void lSET_USED_INVENTORY(ScriptContext &ctx) {
	const int32 item = ctx.stream.readByte();
	printf("%*sSET_USED_INVENTORY %i\n", ctx.level, " ", (int)item);
}

static void lADD_CHOICE(ScriptContext &ctx) {
	const int32 choiceIdx = ctx.stream.readSint16LE();
	printf("%*sADD_CHOICE %i\n", ctx.level, " ", (int)choiceIdx);
}

static void lASK_CHOICE(ScriptContext &ctx) {
	const int32 choiceIdx = ctx.stream.readSint16LE();
	printf("%*sASK_CHOICE %i\n", ctx.level, " ", (int)choiceIdx);
}

static void lBIG_MESSAGE(ScriptContext &ctx) {
	const int32 int32x = ctx.stream.readSint16LE();
	printf("%*sBIG_MESSAGE %i\n", ctx.level, " ", (int)int32x);
}

static void lINIT_PINGOUIN(ScriptContext &ctx) {
	const int16 penguinActor = ctx.stream.readByte();
	printf("%*sINIT_PINGOUIN %i\n", ctx.level, " ", (int)penguinActor);
}

static void lSET_HOLO_POS(ScriptContext &ctx) {
	const int32 location = ctx.stream.readByte();
	printf("%*sSET_HOLO_POS %i\n", ctx.level, " ", (int)location);
}

static void lCLR_HOLO_POS(ScriptContext &ctx) {
	const int32 location = ctx.stream.readByte();
	printf("%*sCLR_HOLO_POS %i\n", ctx.level, " ", (int)location);
}

static void lADD_FUEL(ScriptContext &ctx) {
	const int16 value = ctx.stream.readByte();
	printf("%*sADD_FUEL %i\n", ctx.level, " ", (int)value);
}

static void lSUB_FUEL(ScriptContext &ctx) {
	const int16 value = ctx.stream.readByte();
	printf("%*sSUB_FUEL %i\n", ctx.level, " ", (int)value);
}

static void lSET_GRM(ScriptContext &ctx) {
	printf("%*sSET_GRM %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lSAY_MESSAGE(ScriptContext &ctx) {
	const int32 textEntry = ctx.stream.readSint16LE();
	printf("%*sSAY_MESSAGE %i\n", ctx.level, " ", (int)textEntry);
}

static void lSAY_MESSAGE_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int32 textEntry = ctx.stream.readSint16LE();
	printf("%*sSAY_MESSAGE_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)textEntry);
}

static void lFULL_POINT(ScriptContext &ctx) {
	printf("%*sFULL_POINT\n", ctx.level, " ");
}

static void lBETA(ScriptContext &ctx) {
	const int32 newAngle = ctx.stream.readSint16LE();
	printf("%*sBETA %i\n", ctx.level, " ", (int)newAngle);
}

static void lGRM_OFF(ScriptContext &ctx) {
	printf("%*sGRM_OFF\n", ctx.level, " ");
}

static void lFADE_PAL_RED(ScriptContext &ctx) {
	printf("%*sFADE_PAL_RED\n", ctx.level, " ");
}

static void lFADE_ALARM_RED(ScriptContext &ctx) {
	printf("%*sFADE_ALARM_RED\n", ctx.level, " ");
}

static void lFADE_ALARM_PAL(ScriptContext &ctx) {
	printf("%*sFADE_ALARM_PAL\n", ctx.level, " ");
}

static void lFADE_RED_PAL(ScriptContext &ctx) {
	printf("%*sFADE_RED_PAL\n", ctx.level, " ");
}

static void lFADE_RED_ALARM(ScriptContext &ctx) {
	printf("%*sFADE_RED_ALARM\n", ctx.level, " ");
}

static void lFADE_PAL_ALARM(ScriptContext &ctx) {
	printf("%*sFADE_PAL_ALARM\n", ctx.level, " ");
}

static void lEXPLODE_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	printf("%*sEXPLODE_OBJ %i\n", ctx.level, " ", (int)otherActorIdx);
}

static void lBUBBLE_ON(ScriptContext &ctx) {
	printf("%*sBUBBLE_ON\n", ctx.level, " ");
}

static void lBUBBLE_OFF(ScriptContext &ctx) {
	printf("%*sBUBBLE_OFF\n", ctx.level, " ");
}

static void lASK_CHOICE_OBJ(ScriptContext &ctx) {
	const int32 otherActorIdx = ctx.stream.readByte();
	const int32 choiceIdx = ctx.stream.readSint16LE();
	printf("%*sASK_CHOICE_OBJ %i %i\n", ctx.level, " ", (int)otherActorIdx, (int)choiceIdx);
}

static void lSET_DARK_PAL(ScriptContext &ctx) {
	printf("%*sSET_DARK_PAL\n", ctx.level, " ");
}

static void lSET_NORMAL_PAL(ScriptContext &ctx) {
	printf("%*sSET_NORMAL_PAL\n", ctx.level, " ");
}

static void lMESSAGE_SENDELL(ScriptContext &ctx) {
	printf("%*sMESSAGE_SENDELL\n", ctx.level, " ");
}

static void lANIM_SET(ScriptContext &ctx) {
	printf("%*sANIM_SET %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lHOLOMAP_TRAJ(ScriptContext &ctx) {
	printf("%*sHOLOMAP_TRAJ %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lGAME_OVER(ScriptContext &ctx) {
	printf("%*sGAME_OVER\n", ctx.level, " ");
}

static void lTHE_END(ScriptContext &ctx) {
	printf("%*sTHE_END\n", ctx.level, " ");
}

static void lMIDI_OFF(ScriptContext &ctx) {
	printf("%*sMIDI_OFF\n", ctx.level, " ");
}

static void lPLAY_CD_TRACK(ScriptContext &ctx) {
	printf("%*sPLAY_CD_TRACK %i\n", ctx.level, " ", (int)ctx.stream.readByte());
}

static void lPROJ_ISO(ScriptContext &ctx) {
	printf("%*sPROJ_ISO\n", ctx.level, " ");
}

static void lPROJ_3D(ScriptContext &ctx) {
	printf("%*sPROJ_3D\n", ctx.level, " ");
}

static void lTEXT(ScriptContext &ctx) {
	printf("%*sTEXT %i\n", ctx.level, " ", (int)ctx.stream.readSint16LE());
}

static void lCLEAR_TEXT(ScriptContext &ctx) {
	printf("%*sCLEAR_TEXT\n", ctx.level, " ");
}

static void lBRUTAL_EXIT(ScriptContext &ctx) {
	printf("%*sBRUTAL_EXIT\n", ctx.level, " ");
}

static const ScriptFunction lifeScriptFunctions[] = {
	/*0x00*/ MAPFUNC("END", lEND),
	/*0x01*/ MAPFUNC("NOP", lNOP),
	/*0x02*/ MAPFUNC("SNIF", lSNIF),
	/*0x03*/ MAPFUNC("OFFSET", lOFFSET),
	/*0x04*/ MAPFUNC("NEVERIF", lNEVERIF),
	/*0x05*/ MAPFUNC("", lEMPTY), // unused
	/*0x06*/ MAPFUNC("NO_IF", lNO_IF),
	/*0x07*/ MAPFUNC("", lEMPTY), // unused
	/*0x08*/ MAPFUNC("", lEMPTY), // unused
	/*0x09*/ MAPFUNC("", lEMPTY), // unused
	/*0x0A*/ MAPFUNC("LABEL", lLABEL),
	/*0x0B*/ MAPFUNC("RETURN", lRETURN),
	/*0x0C*/ MAPFUNC("IF", lIF),
	/*0x0D*/ MAPFUNC("SWIF", lSWIF),
	/*0x0E*/ MAPFUNC("ONEIF", lONEIF),
	/*0x0F*/ MAPFUNC("ELSE", lELSE),
	/*0x10*/ MAPFUNC("ENDIF", lEMPTY),
	/*0x11*/ MAPFUNC("BODY", lBODY),
	/*0x12*/ MAPFUNC("BODY_OBJ", lBODY_OBJ),
	/*0x13*/ MAPFUNC("ANIM", lANIM),
	/*0x14*/ MAPFUNC("ANIM_OBJ", lANIM_OBJ),
	/*0x15*/ MAPFUNC("SET_LIFE", lSET_LIFE),
	/*0x16*/ MAPFUNC("SET_LIFE_OBJ", lSET_LIFE_OBJ),
	/*0x17*/ MAPFUNC("SET_TRACK", lSET_TRACK),
	/*0x18*/ MAPFUNC("SET_TRACK_OBJ", lSET_TRACK_OBJ),
	/*0x19*/ MAPFUNC("MESSAGE", lMESSAGE),
	/*0x1A*/ MAPFUNC("FALLABLE", lFALLABLE),
	/*0x1B*/ MAPFUNC("SET_DIRMODE", lSET_DIRMODE),
	/*0x1C*/ MAPFUNC("SET_DIRMODE_OBJ", lSET_DIRMODE_OBJ),
	/*0x1D*/ MAPFUNC("CAM_FOLLOW", lCAM_FOLLOW),
	/*0x1E*/ MAPFUNC("SET_BEHAVIOUR", lSET_BEHAVIOUR),
	/*0x1F*/ MAPFUNC("SET_FLAG_CUBE", lSET_FLAG_CUBE),
	/*0x20*/ MAPFUNC("COMPORTEMENT", lCOMPORTEMENT),
	/*0x21*/ MAPFUNC("SET_COMPORTEMENT", lSET_COMPORTEMENT),
	/*0x22*/ MAPFUNC("SET_COMPORTEMENT_OBJ", lSET_COMPORTEMENT_OBJ),
	/*0x23*/ MAPFUNC("END_COMPORTEMENT", lEND_COMPORTEMENT),
	/*0x24*/ MAPFUNC("SET_FLAG_GAME", lSET_FLAG_GAME),
	/*0x25*/ MAPFUNC("KILL_OBJ", lKILL_OBJ),
	/*0x26*/ MAPFUNC("SUICIDE", lSUICIDE),
	/*0x27*/ MAPFUNC("USE_ONE_LITTLE_KEY", lUSE_ONE_LITTLE_KEY),
	/*0x28*/ MAPFUNC("GIVE_GOLD_PIECES", lGIVE_GOLD_PIECES),
	/*0x29*/ MAPFUNC("END_LIFE", lEND_LIFE),
	/*0x2A*/ MAPFUNC("STOP_L_TRACK", lSTOP_L_TRACK),
	/*0x2B*/ MAPFUNC("RESTORE_L_TRACK", lRESTORE_L_TRACK),
	/*0x2C*/ MAPFUNC("MESSAGE_OBJ", lMESSAGE_OBJ),
	/*0x2D*/ MAPFUNC("INC_CHAPTER", lINC_CHAPTER),
	/*0x2E*/ MAPFUNC("FOUND_OBJECT", lFOUND_OBJECT),
	/*0x2F*/ MAPFUNC("SET_DOOR_LEFT", lSET_DOOR_LEFT),
	/*0x30*/ MAPFUNC("SET_DOOR_RIGHT", lSET_DOOR_RIGHT),
	/*0x31*/ MAPFUNC("SET_DOOR_UP", lSET_DOOR_UP),
	/*0x32*/ MAPFUNC("SET_DOOR_DOWN", lSET_DOOR_DOWN),
	/*0x33*/ MAPFUNC("GIVE_BONUS", lGIVE_BONUS),
	/*0x34*/ MAPFUNC("CHANGE_CUBE", lCHANGE_CUBE),
	/*0x35*/ MAPFUNC("OBJ_COL", lOBJ_COL),
	/*0x36*/ MAPFUNC("BRICK_COL", lBRICK_COL),
	/*0x37*/ MAPFUNC("OR_IF", lOR_IF),
	/*0x38*/ MAPFUNC("INVISIBLE", lINVISIBLE),
	/*0x39*/ MAPFUNC("ZOOM", lZOOM),
	/*0x3A*/ MAPFUNC("POS_POINT", lPOS_POINT),
	/*0x3B*/ MAPFUNC("SET_MAGIC_LEVEL", lSET_MAGIC_LEVEL),
	/*0x3C*/ MAPFUNC("SUB_MAGIC_POINT", lSUB_MAGIC_POINT),
	/*0x3D*/ MAPFUNC("SET_LIFE_POINT_OBJ", lSET_LIFE_POINT_OBJ),
	/*0x3E*/ MAPFUNC("SUB_LIFE_POINT_OBJ", lSUB_LIFE_POINT_OBJ),
	/*0x3F*/ MAPFUNC("HIT_OBJ", lHIT_OBJ),
	/*0x40*/ MAPFUNC("PLAY_FLA", lPLAY_FLA),
	/*0x41*/ MAPFUNC("PLAY_MIDI", lPLAY_MIDI),
	/*0x42*/ MAPFUNC("INC_CLOVER_BOX", lINC_CLOVER_BOX),
	/*0x43*/ MAPFUNC("SET_USED_INVENTORY", lSET_USED_INVENTORY),
	/*0x44*/ MAPFUNC("ADD_CHOICE", lADD_CHOICE),
	/*0x45*/ MAPFUNC("ASK_CHOICE", lASK_CHOICE),
	/*0x46*/ MAPFUNC("BIG_MESSAGE", lBIG_MESSAGE),
	/*0x47*/ MAPFUNC("INIT_PINGOUIN", lINIT_PINGOUIN),
	/*0x48*/ MAPFUNC("SET_HOLO_POS", lSET_HOLO_POS),
	/*0x49*/ MAPFUNC("CLR_HOLO_POS", lCLR_HOLO_POS),
	/*0x4A*/ MAPFUNC("ADD_FUEL", lADD_FUEL),
	/*0x4B*/ MAPFUNC("SUB_FUEL", lSUB_FUEL),
	/*0x4C*/ MAPFUNC("SET_GRM", lSET_GRM),
	/*0x4D*/ MAPFUNC("SAY_MESSAGE", lSAY_MESSAGE),
	/*0x4E*/ MAPFUNC("SAY_MESSAGE_OBJ", lSAY_MESSAGE_OBJ),
	/*0x4F*/ MAPFUNC("FULL_POINT", lFULL_POINT),
	/*0x50*/ MAPFUNC("BETA", lBETA),
	/*0x51*/ MAPFUNC("GRM_OFF", lGRM_OFF),
	/*0x52*/ MAPFUNC("FADE_PAL_RED", lFADE_PAL_RED),
	/*0x53*/ MAPFUNC("FADE_ALARM_RED", lFADE_ALARM_RED),
	/*0x54*/ MAPFUNC("FADE_ALARM_PAL", lFADE_ALARM_PAL),
	/*0x55*/ MAPFUNC("FADE_RED_PAL", lFADE_RED_PAL),
	/*0x56*/ MAPFUNC("FADE_RED_ALARM", lFADE_RED_ALARM),
	/*0x57*/ MAPFUNC("FADE_PAL_ALARM", lFADE_PAL_ALARM),
	/*0x58*/ MAPFUNC("EXPLODE_OBJ", lEXPLODE_OBJ),
	/*0x59*/ MAPFUNC("BUBBLE_ON", lBUBBLE_ON),
	/*0x5A*/ MAPFUNC("BUBBLE_OFF", lBUBBLE_OFF),
	/*0x5B*/ MAPFUNC("ASK_CHOICE_OBJ", lASK_CHOICE_OBJ),
	/*0x5C*/ MAPFUNC("SET_DARK_PAL", lSET_DARK_PAL),
	/*0x5D*/ MAPFUNC("SET_NORMAL_PAL", lSET_NORMAL_PAL),
	/*0x5E*/ MAPFUNC("MESSAGE_SENDELL", lMESSAGE_SENDELL),
	/*0x5F*/ MAPFUNC("ANIM_SET", lANIM_SET),
	/*0x60*/ MAPFUNC("HOLOMAP_TRAJ", lHOLOMAP_TRAJ),
	/*0x61*/ MAPFUNC("GAME_OVER", lGAME_OVER),
	/*0x62*/ MAPFUNC("THE_END", lTHE_END),
	/*0x63*/ MAPFUNC("MIDI_OFF", lMIDI_OFF),
	/*0x64*/ MAPFUNC("PLAY_CD_TRACK", lPLAY_CD_TRACK),
	/*0x65*/ MAPFUNC("PROJ_ISO", lPROJ_ISO),
	/*0x66*/ MAPFUNC("PROJ_3D", lPROJ_3D),
	/*0x67*/ MAPFUNC("TEXT", lTEXT),
	/*0x68*/ MAPFUNC("CLEAR_TEXT", lCLEAR_TEXT),
	/*0x69*/ MAPFUNC("BRUTAL_EXIT", lBRUTAL_EXIT)
};

static int decompileLBA1MoveScript(int actorIdx, const uint8 *data, int16 size) {
	Common::MemoryReadStream stream(data, size);
	ScriptContext ctx{stream, {}, initialLevel, 0};

	printf("Actor %i\n", actorIdx);

	while (ctx.stream.pos() < ctx.stream.size()) {
		const byte scriptOpcode = ctx.stream.readByte();
		if (scriptOpcode < ARRAYSIZE(moveScriptFunctions)) {
			moveScriptFunctions[scriptOpcode].function(ctx);
		} else {
			fprintf(stderr, "Actor %d with wrong offset/opcode - Offset: %d/%d (opcode: %u)", actorIdx, (int)ctx.stream.pos() - 1, (int)ctx.stream.size(), scriptOpcode);
			return 1;
		}
	};

	return 0;
}

static int decompileLBA1LifeScript(int actorIdx, const uint8 *data, int16 size) {
	Common::MemoryReadStream stream(data, size);
	ScriptContext ctx{stream, {}, initialLevel, 0, true};

	printf("Actor %i\n", actorIdx);

	printf("%*sCOMPORTMENT main\n", ctx.level, " ");
	ctx.level += indentWidth;
	while (ctx.stream.pos() < ctx.stream.size()) {
		const byte scriptOpcode = ctx.stream.readByte();
		if (scriptOpcode < ARRAYSIZE(lifeScriptFunctions)) {
			if (scriptOpcode && !ctx.comportement) {
				++ctx.comportmentId;
				printf("%*sCOMPORTEMENT %i\n", ctx.level, " ", ctx.comportmentId);
				ctx.level += indentWidth;
				ctx.comportement = true;
			}
			lifeScriptFunctions[scriptOpcode].function(ctx);
			while (!ctx.offsets.empty()) {
				if (ctx.stream.pos() == ctx.offsets.back()) {
					ctx.level -= indentWidth;
					printf("%*sENDIF\n", ctx.level, " ");
					ctx.offsets.pop_back();
				} else {
					break;
				}
			}
		} else {
			fprintf(stderr, "Actor %d with wrong offset/opcode - Offset: %d/%d (opcode: %u)", actorIdx, (int)ctx.stream.pos() - 1, (int)ctx.stream.size(), scriptOpcode);
			return 1;
		}
	};

	return 0;
}

int decompileLBA1(const uint8 *data, int size) {
	Common::MemoryReadStream stream(data, size);
	uint8_t sceneTextBank = stream.readByte();
	uint8_t currentGameOverScene = stream.readByte();
	stream.skip(4);
	int16 _alphaLight = (int16)stream.readUint16LE();
	int16 _betaLight = (int16)stream.readUint16LE();

	uint16 sampleAmbiance[4];
	uint16 sampleRepeat[4];
	uint16 sampleRound[4];

	for (int i = 0; i < 4; ++i) {
		sampleAmbiance[i] = stream.readUint16LE();
		sampleRepeat[i] = stream.readUint16LE();
		sampleRound[i] = stream.readUint16LE();
	}

	uint16 sampleMinDelay = stream.readUint16LE();
	uint16 sampleMinDelayRnd = stream.readUint16LE();

	uint8 sceneMusic = stream.readByte();

	int16 sceneHeroPosx = (int16)stream.readUint16LE();
	int16 sceneHeroPosy = (int16)stream.readUint16LE();
	int16 sceneHeroPosz = (int16)stream.readUint16LE();

	int16 moveScriptSize = (int16)stream.readUint16LE();
	const uint8 *moveScript = data + stream.pos();
	stream.skip(moveScriptSize);
	decompileLBA1MoveScript(0, moveScript, moveScriptSize);

	int16 lifeScriptSize = (int16)stream.readUint16LE();
	const uint8 *lifeScript = data + stream.pos();
	stream.skip(lifeScriptSize);
	decompileLBA1LifeScript(0, lifeScript, lifeScriptSize);

	int16 sceneNumActors = (int16)stream.readUint16LE();
	int cnt = 1;
	for (int32 a = 1; a < sceneNumActors; a++, cnt++) {
		uint16 staticflags = stream.readUint16LE();
		uint16 body = stream.readUint16LE();
		uint8 genBody = stream.readByte();
		uint8 genAnim = stream.readByte();
		int16 sprite = stream.readSint16LE();
		int16 posx = stream.readSint16LE();
		int16 posy = stream.readSint16LE();
		int16 posz = stream.readSint16LE();
		uint8 strengthOfHit = stream.readByte();
		uint16 bonusflags = stream.readUint16LE();
		int16 beta = stream.readSint16LE();
		int16 speed = stream.readSint16LE();
		uint16 controlMode = stream.readUint16LE();
		int16 cropLeft = stream.readSint16LE();
		int16 cropTop = stream.readSint16LE();
		int16 cropRight = stream.readSint16LE();
		int16 cropBottom = stream.readSint16LE();
		uint8 bonusAmount = stream.readByte();
		uint8 talkColor = stream.readByte();
		uint8 armor = stream.readByte();
		uint8 lifePoints = stream.readByte();

		moveScriptSize = (int16)stream.readUint16LE();
		moveScript = data + stream.pos();
		stream.skip(moveScriptSize);
		decompileLBA1MoveScript(a, moveScript, moveScriptSize);

		lifeScriptSize = (int16)stream.readUint16LE();
		lifeScript = data + stream.pos();
		stream.skip(lifeScriptSize);
		decompileLBA1LifeScript(a, lifeScript, lifeScriptSize);
	}

	int16 sceneNumZones = stream.readSint16LE();
	for (int16 i = 0; i < sceneNumZones; i++) {
		int16 zoneminsx = stream.readSint16LE();
		int16 zoneminsy = stream.readSint16LE();
		int16 zoneminsz = stream.readSint16LE();

		int16 zonemaxsx = stream.readSint16LE();
		int16 zonemaxsy = stream.readSint16LE();
		int16 zonemaxsz = stream.readSint16LE();

		uint16 zonetype = stream.readUint16LE();
		int16 zonenum = stream.readSint16LE();

		int16 info0 = stream.readSint16LE();
		int16 info1 = stream.readSint16LE();
		int16 info2 = stream.readSint16LE();
		int16 info3 = stream.readSint16LE();
	}

	uint16 sceneNumTracks = stream.readUint16LE();
	for (uint16 i = 0; i < sceneNumTracks; i++) {
		int16 pointx = stream.readSint16LE();
		int16 pointy = stream.readSint16LE();
		int16 pointz = stream.readSint16LE();
	}

	if (stream.err()) {
		return 1;
	}
	return 0;
}
