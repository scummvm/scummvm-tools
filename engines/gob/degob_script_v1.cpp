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

/* GobEngine Script disassembler */

#include "degob_script.h"
#include "common/endian.h"
#include "common/util.h"

#define OPCODET(x) _OPCODET(Script_v1, x)
#define OPCODEF(x) _OPCODEF(Script_v1, x)
#define OPCODEB(x) _OPCODEB(Script_v1, x)

const int Script_v1::_goblinFuncLookUp[][2] = {
	{1, 0},
	{2, 1},
	{3, 2},
	{4, 3},
	{5, 4},
	{6, 5},
	{7, 6},
	{8, 7},
	{9, 8},
	{10, 9},
	{12, 10},
	{13, 11},
	{14, 12},
	{15, 13},
	{16, 14},
	{21, 15},
	{22, 16},
	{23, 17},
	{24, 18},
	{25, 19},
	{26, 20},
	{27, 21},
	{28, 22},
	{29, 23},
	{30, 24},
	{32, 25},
	{33, 26},
	{34, 27},
	{35, 28},
	{36, 29},
	{37, 30},
	{40, 31},
	{41, 32},
	{42, 33},
	{43, 34},
	{44, 35},
	{50, 36},
	{52, 37},
	{53, 38},
	{150, 39},
	{152, 40},
	{200, 41},
	{201, 42},
	{202, 43},
	{203, 44},
	{204, 45},
	{250, 46},
	{251, 47},
	{252, 48},
	{500, 49},
	{502, 50},
	{503, 51},
	{600, 52},
	{601, 53},
	{602, 54},
	{603, 55},
	{604, 56},
	{605, 57},
	{1000, 58},
	{1001, 59},
	{1002, 60},
	{1003, 61},
	{1004, 62},
	{1005, 63},
	{1006, 64},
	{1008, 65},
	{1009, 66},
	{1010, 67},
	{1011, 68},
	{1015, 69},
	{2005, 70},
	{39, 71}
};

Script_v1::Script_v1(byte *totData, uint32 totSize, ExtTable *extTable) :
	Script(totData, totSize, extTable) {

	setupOpcodes();
}

Script_v1::~Script_v1() {
}

void Script_v1::setupOpcodes() {
	static const OpcodeDrawEntryV1 opcodesDraw[256] = {
		/* 00 */
		{OPCODEF(o1_loadMult), {PARAM_NONE}},
		{OPCODET(o1_playMult), {PARAM_UINT16}},
		{OPCODET(o1_freeMultKeys), {PARAM_UINT16}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 04 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{OPCODET(o1_initCursor), {PARAM_VARINDEX, PARAM_VARINDEX, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		/* 08 */
		{OPCODET(o1_initCursorAnim), {PARAM_EXPR, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_clearCursorAnim), {PARAM_EXPR}},
		{OPCODET(o1_setRenderFlags), {PARAM_EXPR}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 0C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 10 */
		{OPCODEF(o1_loadAnim), {PARAM_NONE}},
		{OPCODET(o1_freeAnim), {PARAM_EXPR}},
		{OPCODET(o1_updateAnim), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_UINT16}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 14 */
		{OPCODET(o1_initMult), {PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODET(o1_freeMult), {PARAM_NONE}},
		{OPCODET(o1_animate), {PARAM_NONE}},
		{OPCODEF(o1_loadMultObject), {PARAM_NONE}},
		/* 18 */
		{OPCODET(o1_getAnimLayerInfo), {PARAM_EXPR, PARAM_EXPR, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODET(o1_getObjAnimSize), {PARAM_EXPR, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODEF(o1_loadStatic), {PARAM_NONE}},
		{OPCODET(o1_freeStatic), {PARAM_EXPR}},
		/* 1C */
		{OPCODET(o1_renderStatic), {PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_loadCurLayer), {PARAM_EXPR, PARAM_EXPR}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 20 */
		{OPCODET(o1_playCDTrack), {PARAM_EXPR}},
		{OPCODET(o1_getCDTrackPos), {PARAM_NONE}},
		{OPCODET(o1_stopCD), {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 24 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 28 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 2C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 30 */
		{OPCODET(o1_loadFontToSprite), {PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_freeFontToSprite), {PARAM_INT16}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 34 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 38 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 3C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 40 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 44 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 48 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 4C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 50 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 54 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 58 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 5C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 60 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 64 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 68 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 6C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 70 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 74 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 78 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 7C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 80 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 84 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 88 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 8C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 90 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 94 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 98 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 9C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* A0 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* A4 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* A8 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* AC */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* B0 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* B4 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* B8 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* BC */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* C0 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* C4 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* C8 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* CC */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* D0 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* D4 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* D8 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* DC */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* E0 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* E4 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* E8 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* EC */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* F0 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* F4 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* F8 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* FC */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
	};

	static const OpcodeFuncEntryV1 opcodesFunc[80] = {
		/* 00 */
		{OPCODEF(o1_callSub), {PARAM_NONE}},
		{OPCODEF(o1_callSub), {PARAM_NONE}},
		{OPCODET(o1_printTotText), {PARAM_INT16}},
		{OPCODET(o1_loadCursor), {PARAM_INT16, PARAM_INT8}},
		/* 04 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{OPCODEF(o1_switch), {PARAM_NONE}},
		{OPCODEF(o1_repeatUntil), {PARAM_NONE}},
		{OPCODEF(o1_whileDo), {PARAM_NONE}},
		/* 08 */
		{OPCODEF(o1_if), {PARAM_NONE}},
		{OPCODEF(o1_assign), {PARAM_NONE}},
		{OPCODEF(o1_loadSpriteToPos), {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 0C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 10 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{OPCODEF(o1_printText), {PARAM_NONE}},
		{OPCODEF(o1_loadTot), {PARAM_NONE}},
		{OPCODEF(o1_palLoad), {PARAM_NONE}},
		/* 14 */
		{OPCODET(o1_keyFunc), {PARAM_INT16}},
		{OPCODET(o1_capturePush), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_capturePop), {PARAM_NONE}},
		{OPCODET(o1_animPalInit), {PARAM_INT16, PARAM_EXPR, PARAM_EXPR}},
		/* 18 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 1C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{OPCODEF(o1_drawOperations), {PARAM_NONE}},
		{OPCODEF(o1_setcmdCount), {PARAM_NONE}},
		/* 20 */
		{OPCODEF(o1_return), {PARAM_NONE}},
		{OPCODET(o1_renewTimeInVars), {PARAM_NONE}},
		{OPCODET(o1_speakerOn), {PARAM_EXPR}},
		{OPCODET(o1_speakerOff), {PARAM_NONE}},
		/* 24 */
		{OPCODET(o1_putPixel), {PARAM_INT16, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		{OPCODEF(o1_goblinFunc), {PARAM_NONE}},
		{OPCODET(o1_createSprite), {PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_freeSprite), {PARAM_INT16}},
		/* 28 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 2C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 30 */
		{OPCODEF(o1_returnTo), {PARAM_NONE}},
		{OPCODET(o1_loadSpriteContent), {PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_copySprite), {PARAM_INT16, PARAM_INT16, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_INT16}},
		{OPCODET(o1_fillRect), {PARAM_INT16, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		/* 34 */
		{OPCODET(o1_drawLine), {PARAM_INT16, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_strToLong), {PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODET(o1_invalidate), {PARAM_INT16, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_setBackDelta), {PARAM_EXPR, PARAM_EXPR}},
		/* 38 */
		{OPCODET(o1_playSound), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_stopSound), {PARAM_EXPR}},
		{OPCODEF(o1_loadSound), {PARAM_NONE}},
		{OPCODET(o1_freeSoundSlot), {PARAM_EXPR}},
		/* 3C */
		{OPCODET(o1_waitEndPlay), {PARAM_NONE}},
		{OPCODET(o1_playComposition), {PARAM_VARINDEX, PARAM_EXPR}},
		{OPCODET(o1_getFreeMem), {PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODET(o1_checkData), {PARAM_EXPR, PARAM_VARINDEX}},
		/* 40 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{OPCODET(o1_cleanupStr), {PARAM_VARINDEX}},
		{OPCODET(o1_insertStr), {PARAM_VARINDEX, PARAM_EXPR}},
		{OPCODET(o1_cutStr), {PARAM_VARINDEX, PARAM_EXPR, PARAM_EXPR}},
		/* 44 */
		{OPCODET(o1_strstr), {PARAM_VARINDEX, PARAM_EXPR, PARAM_VARINDEX}},
		{OPCODET(o1_istrlen), {PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODET(o1_setMousePos), {PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_setFrameRate), {PARAM_EXPR}},
		/* 48 */
		{OPCODET(o1_animatePalette), {PARAM_NONE}},
		{OPCODET(o1_animateCursor), {PARAM_NONE}},
		{OPCODET(o1_blitCursor), {PARAM_NONE}},
		{OPCODET(o1_loadFont), {PARAM_EXPR, PARAM_INT16}},
		/* 4C */
		{OPCODET(o1_freeFont), {PARAM_INT16}},
		{OPCODET(o1_readData), {PARAM_EXPR, PARAM_VARINDEX, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_writeData), {PARAM_EXPR, PARAM_VARINDEX, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_manageDataFile), {PARAM_EXPR}},
	};

	static const OpcodeGoblinEntryV1 opcodesGoblin[72] = {
		/* 00 */
		{OPCODET(o1_setState), {PARAM_GOB}},
		{OPCODET(o1_setCurFrame), {PARAM_GOB}},
		{OPCODET(o1_setNextState), {PARAM_GOB}},
		{OPCODET(o1_setMultState), {PARAM_GOB}},
		/* 04 */
		{OPCODET(o1_setOrder), {PARAM_GOB}},
		{OPCODET(o1_setActionStartState), {PARAM_GOB}},
		{OPCODET(o1_setCurLookDir), {PARAM_GOB}},
		{OPCODET(o1_setType), {PARAM_GOB}},
		/* 08 */
		{OPCODET(o1_setNoTick), {PARAM_GOB}},
		{OPCODET(o1_setPickable), {PARAM_GOB}},
		{OPCODET(o1_setXPos), {PARAM_GOB}},
		{OPCODET(o1_setYPos), {PARAM_GOB}},
		/* 0C */
		{OPCODET(o1_setDoAnim), {PARAM_GOB}},
		{OPCODET(o1_setRelaxTime), {PARAM_GOB}},
		{OPCODET(o1_setMaxTick), {PARAM_GOB}},
		{OPCODET(o1_getState), {PARAM_GOB}},
		/* 10 */
		{OPCODET(o1_getCurFrame), {PARAM_GOB}},
		{OPCODET(o1_getNextState), {PARAM_GOB}},
		{OPCODET(o1_getMultState), {PARAM_GOB}},
		{OPCODET(o1_getOrder), {PARAM_GOB}},
		/* 14 */
		{OPCODET(o1_getActionStartState), {PARAM_GOB}},
		{OPCODET(o1_getCurLookDir), {PARAM_GOB}},
		{OPCODET(o1_getType), {PARAM_GOB}},
		{OPCODET(o1_getNoTick), {PARAM_GOB}},
		/* 18 */
		{OPCODET(o1_getPickable), {PARAM_GOB}},
		{OPCODET(o1_getObjMaxFrame), {PARAM_GOB}},
		{OPCODET(o1_getXPos), {PARAM_GOB}},
		{OPCODET(o1_getYPos), {PARAM_GOB}},
		/* 1C */
		{OPCODET(o1_getDoAnim), {PARAM_GOB}},
		{OPCODET(o1_getRelaxTime), {PARAM_GOB}},
		{OPCODET(o1_getMaxTick), {PARAM_GOB}},
		{OPCODET(o1_manipulateMap), {PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		/* 20 */
		{OPCODET(o1_getItem), {PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_manipulateMapIndirect), {PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_getItemIndirect), {PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_setPassMap), {PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		/* 24 */
		{OPCODET(o1_setGoblinPosH), {PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_getGoblinPosXH), {PARAM_INT16}},
		{OPCODET(o1_getGoblinPosYH), {PARAM_INT16}},
		{OPCODET(o1_setGoblinMultState), {PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		/* 28 */
		{OPCODET(o1_setGoblinUnk14), {PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_setItemIdInPocket), {PARAM_INT16}},
		{OPCODET(o1_setItemIndInPocket), {PARAM_INT16}},
		{OPCODET(o1_getItemIdInPocket), {PARAM_NONE}},
		/* 2C */
		{OPCODET(o1_getItemIndInPocket), {PARAM_NONE}},
		{OPCODET(o1_setItemPos), {PARAM_NONE}},
		{OPCODET(o1_setGoblinPos), {PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_setGoblinState), {PARAM_INT16, PARAM_INT16}},
		/* 30 */
		{OPCODET(o1_setGoblinStateRedraw), {PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_decRelaxTime), {PARAM_INT16}},
		{OPCODET(o1_getGoblinPosX), {PARAM_INT16}},
		{OPCODET(o1_getGoblinPosY), {PARAM_INT16}},
		/* 34 */
		{OPCODET(o1_clearPathExistence), {PARAM_NONE}},
		{OPCODET(o1_setGoblinVisible), {PARAM_INT16}},
		{OPCODET(o1_setGoblinInvisible), {PARAM_INT16}},
		{OPCODET(o1_getObjectIntersect), {PARAM_INT16, PARAM_INT16}},
		/* 38 */
		{OPCODET(o1_getGoblinIntersect), {PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_setItemPos), {PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_loadObjects), {PARAM_INT16}},
		{OPCODET(o1_freeObjects), {PARAM_NONE}},
		/* 3C */
		{OPCODET(o1_animateObjects), {PARAM_NONE}},
		{OPCODET(o1_drawObjects), {PARAM_NONE}},
		{OPCODET(o1_loadMap), {PARAM_NONE}},
		{OPCODET(o1_moveGoblin), {PARAM_INT16, PARAM_INT16}},
		/* 40 */
		{OPCODET(o1_switchGoblin), {PARAM_NONE}},
		{OPCODET(o1_loadGoblin), {PARAM_NONE}},
		{OPCODET(o1_writeTreatItem), {PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_moveGoblin0), {PARAM_NONE}},
		/* 44 */
		{OPCODET(o1_setGoblinTarget), {PARAM_INT16}},
		{OPCODET(o1_setGoblinObjectsPos), {PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_initGoblin), {PARAM_NONE}},
		{OPCODEF(o1_dummy), {PARAM_NONE}}
	};

	_opcodesDrawV1 = opcodesDraw;
	_opcodesFuncV1 = opcodesFunc;
	_opcodesGoblinV1 = opcodesGoblin;
}

void Script_v1::drawOpcode(byte i, FuncParams &params) {
	FuncType type = _opcodesDrawV1[i].type;
	params.desc = _opcodesDrawV1[i].desc;
	OpcodeDrawProcV1 op = _opcodesDrawV1[i].proc;

	if (type == TYPE_NONE)
		error("No such opcodeDraw: %d", i);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC))
		printFuncDesc(params, _opcodesDrawV1[i].params);
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_v1::funcOpcode(byte i, byte j, FuncParams &params) {
	int n = i*16 + j;
	FuncType type = TYPE_NONE;
	OpcodeFuncProcV1 op = 0;

	if ((i <= 4) && (j <= 15)) {
		op = _opcodesFuncV1[n].proc;
		params.desc = _opcodesFuncV1[n].desc;
		type = _opcodesFuncV1[n].type;
	}

	if (type == TYPE_NONE)
		error("No such opcodeFunc: %d.%d", i, j);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC))
		printFuncDesc(params, _opcodesFuncV1[n].params);
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_v1::goblinOpcode(int i, FuncParams &params) {
	int n = -1;
	for (int j = 0; j < ARRAYSIZE(_goblinFuncLookUp); j++)
		if (_goblinFuncLookUp[j][0] == i) {
			n = _goblinFuncLookUp[j][1];
			break;
		}

	FuncType type = TYPE_NONE;
	OpcodeGoblinProcV1 op = 0;

	if (n >= 0) {
		op = _opcodesGoblinV1[n].proc;
		params.desc = _opcodesGoblinV1[n].desc;
		type = _opcodesGoblinV1[n].type;
	}

	if (type == TYPE_NONE)
		error("No such opcodeGoblin: %d (%d)", i, n);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC)) {
		const Param *param = _opcodesGoblinV1[n].params;
		if (*param == PARAM_GOB)
			printFuncDesc(params);
		else
			printFuncDesc(params, param);
	}
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_v1::o1_drawOperations(FuncParams &params) {
	drawOpcode(readUint8(), params);
}

void Script_v1::o1_goblinFunc(FuncParams &params) {
	FuncParams gobParams;
	//bool objDescSet = false;
	int16 cmd;

	gobParams.extraData = 0;
	gobParams.objIndex = -1;

	cmd = (int16) readUint16();
	skip(2);

	if ((cmd > 0) && (cmd < 17)) {
		//objDescSet = true;
		gobParams.objIndex = (int16) readUint16();
		gobParams.extraData = (int16) readUint16();
	}

	if ((cmd > 90) && (cmd < 107)) {
		//objDescSet = true;
		gobParams.objIndex = (int16) readUint16();
		gobParams.extraData = (int16) readUint16();
		cmd -= 90;
	}

	if ((cmd > 110) && (cmd < 128)) {
		//objDescSet = true;
		gobParams.objIndex = (int16) readUint16();
		cmd -= 90;
	} else if ((cmd > 20) && (cmd < 38)) {
		//objDescSet = true;
		gobParams.objIndex = (int16) readUint16();
	}

	goblinOpcode(cmd, gobParams);
}

void Script_v1::o1_callSub(FuncParams &params) {
	uint16 offset = readUint16();

	printIndent();

	uint32 pos = getPos();

	seek(offset);

	if (peekUint8() == 1) {
		print("sub_%d();\n", offset);
		if (offset >= 128)
			addFuncOffset(offset);
	} else if (peekUint8() == 2)
		print("_hotspots->evaluate()(%d);\n", offset);
	else
		print("<Unknown block type %d (%d)>\n", peekUint8(), offset);

	seek(pos);
}

void Script_v1::o1_switch(FuncParams &params) {
	uint32 nextPos;
	int16 len;

	printIndent();
	print("switch (%s) {\n", readVarIndex().c_str());

	len = (int8) readUint8();
	while (len != -5) {
		for (int16 i = 0; i < len; i++) {
			printIndent();
			print("case %s:\n", readExpr().c_str());
		}

		incIndent();

		nextPos = getPos() + getBlockSize();

		funcBlock(0);

		seek(nextPos);

		printIndent();
		print("break;\n");

		decIndent();

		len = (int8) readUint8();
	}

	if ((peekUint8() >> 4) == 4) {
		printIndent();
		print("default:\n");
		incIndent();

		skip(1);
		nextPos = getPos() + getBlockSize();

		funcBlock(0);

		seek(nextPos);

		printIndent();
		print("break;\n");

		decIndent();
	}

	printIndent();
	print("}\n");
}

void Script_v1::o1_repeatUntil(FuncParams &params) {
	uint32 nextPos = getPos() + getBlockSize() + 1;

	printIndent();
	print("repeat {\n");
	incIndent();

	funcBlock(1);

	seek(nextPos);

	decIndent();
	printIndent();
	print("} until (%s);\n", readExpr().c_str());
}

void Script_v1::o1_whileDo(FuncParams &params) {
	printIndent();
	print("while (%s) {\n", readExpr().c_str());
	incIndent();

	uint32 nextPos = getPos() + getBlockSize();

	funcBlock(1);

	seek(nextPos);

	decIndent();
	printIndent();
	print("}\n");
}

void Script_v1::o1_if(FuncParams &params) {
	printIndent();
	print("if (%s) {\n", readExpr().c_str());
	incIndent();

	uint32 nextPos = getPos() + getBlockSize();

	funcBlock(0);

	seek(nextPos);

	if ((readUint8() >> 4) == 12) {
		decIndent();
		printIndent();
		print("} else {\n");
		incIndent();

		nextPos = getPos() + getBlockSize();

		funcBlock(0);

		seek(nextPos);
	}

	decIndent();
	printIndent();
	print("}\n");
}

void Script_v1::o1_return(FuncParams &params) {
	printIndent();
	print("return;\n");
}

void Script_v1::o1_returnTo(FuncParams &params) {
	printIndent();
	print("return;\n");
}

void Script_v1::o1_setcmdCount(FuncParams &params) {
	params.cmdCount = readUint8();
	params.counter = 0;
}

void Script_v1::o1_assign(FuncParams &params) {
	printIndent();
	std::string varIndex = readVarIndex();
	std::string expr = readExpr();

	print("%s = %s;\n", varIndex.c_str(), expr.c_str());
}

void Script_v1::o1_palLoad(FuncParams &params) {
	byte cmd = readUint8();

	startFunc(params);
	print("%d, %d", (cmd & 0x80) != 0, cmd & 0x7F);
	endFunc();

	switch (cmd & 0x7F) {
	case 48:
		skip(48);
		break;

	case 49:
		skip(18);
		break;

	case 50:
		skip(16);
		break;

	case 51:
		skip(2);
		break;

	case 52:
		skip(48);
		break;

	case 53:
	case 55:
	case 56:
		skip(2);
		break;

	case 61:
		skip(4);
		break;
	}
}

void Script_v1::o1_loadSpriteToPos(FuncParams &params) {
	startFunc(params);
	print("%d, ", (int16) readUint16());
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());
	uint8 tDest = readUint8();
	print("%d, %d", tDest & 1, ((int8) (tDest >> 1)) - 1);
	endFunc();

	skip(1);
}

void Script_v1::o1_printText(FuncParams &params) {
	startFunc(params);
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());
	print("%s", readExpr().c_str());

	do {
		print(", \"");
		for (int i = 0; (((char) (peekUint8())) != '.') && (peekUint8() != 200); i++)
			print("%c", (char) readUint8());

		if (peekUint8() != 200) {
			skip(1);

			print("\", ");
			switch (peekUint8()) {
			case 16:
			case 17:
			case 18:
			case 23:
			case 24:
			case 25:
			case 26:
			case 27:
			case 28:
				print("%s", readVarIndex().c_str());
				break;
			}
			skip(1);
		} else
			print("\"");

	} while (peekUint8() != 200);

	endFunc();

	skip(1);
}

void Script_v1::o1_loadTot(FuncParams &params) {
	startFunc(params);

	int8 size = (int8) readUint8();
	if ((size & 0x80) == 0) {
		for (int i = 0; i < size; i++)
			print("%c", (char) readUint8());
	} else
		print("%s", readExpr().c_str());
	print(".tot");

	endFunc();
}

void Script_v1::o1_loadSound(FuncParams &params) {
	int16 id;

	startFunc(params);
	print("%s, ", readExpr().c_str());
	id = readUint16();
	print("%d", id);
	if (id == -1) {
		print(", %s", peekString());
		skip(9);
	}
	endFunc();
}

void Script_v1::o1_loadMult(FuncParams &params) {
	uint16 id = readUint16();

	startFunc(params);
	print("%d", id);
	endFunc();

	if (!_extTable)
		error("EXT file needed");

	uint32 size;
	byte *extData = _extTable->getItem(id - 30000, size);
	byte *data = extData;

	int32 count1, count2;

	count1 = ((int8) data[0]) + 1;
	count2 = ((int8) data[1]) + 1;
	data += 2;
	// Statics
	for (int i = 0; i < count1; i++, data += 14) {
		int16 sSize;

		readExpr();
		sSize = (int16) readUint16();
		skip(sSize * 2);
		sSize = (int16) readUint16();
		skip(2 + sSize * 8);
	}
	// Anims
	for (int i = 0; i < count2; i++, data += 14) {
		readExpr();
		int16 sSize = (int16) readUint16();
		skip(2 + sSize * 8);
	}

	// FPS
	data += 2;

	// StaticKeys
	count1 = (int16) READ_LE_UINT16(data);
	data += 2 + count1 * 4;

	// AnimKeys
	for (int i = 0; i < 4; i++) {
		count1 = (int16) READ_LE_UINT16(data);
		data += 2 + count1 * 10;
	}

	// fadePal
	data += 5 * 16 * 3;

	// palFadeKeys
	count1 = (int16) READ_LE_UINT16(data);
	data += 2 + count1 * 7;

	// palKeys
	count1 = (int16) READ_LE_UINT16(data);
	data += 2 + count1 * 80;

	// textKeys
	count1 = (int16) READ_LE_UINT16(data);
	data += 2 + count1 * 28;

	// soundKeys
	count1 = (int16) READ_LE_UINT16(data);
	data += 2;
	for (int i = 0; i < count1; i++, data += 36) {
		int16 cmd = (int16) READ_LE_UINT16(data + 2);

		if ((cmd == 1) || (cmd == 4))
			skip(2);
		else if (cmd == 3)
			skip(6);
		else if (cmd == 5)
			skip(((int16) READ_LE_UINT16(data + 4)) * 2);
	}

	delete[] extData;
}

void Script_v1::o1_loadAnim(FuncParams &params) {
	int16 sSize;

	startFunc(params);
	print("%s, ", readExpr().c_str());
	sSize = (int16) readUint16();
	print("%d, %d", sSize, readUint16());
	endFunc();

	skip(sSize * 8);
}

void Script_v1::o1_loadStatic(FuncParams &params) {
	int16 sSize1, sSize2;

	startFunc(params);
	print("%s, ", readExpr().c_str());
	sSize1 = (int16) readUint16();
	skip(sSize1 * 2);
	sSize2 = (int16) readUint16();
	print("%d, %d, %d", sSize1, sSize2, readUint16());
	endFunc();

	skip(sSize2 * 8);
}

void Script_v1::o1_loadMultObject(FuncParams &params) {
	startFunc(params);
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());

	for (int i = 0; i < 11; i++)
	{
		if (peekUint8() != 99)
		{
			switch (i) {
			case 0:
				printf("animation=");
				break;
			case 1:
				printf("layer=");
				break;
			case 2:
				printf("frame=");
				break;
			case 3:
				printf("animType=");
				break;
			case 4:
				printf("order=");
				break;
			case 5:
				printf("isPaused=");
				break;
			case 6:
				printf("isStatic=");
				break;
			case 7:
				printf("maxTick=");
				break;
			case 8:
				printf("maxFrame=");
				break;
			case 9:
				printf("newLayer=");
				break;
			case 10:
				printf("newAnimation=");
				break;
			default:
				printf("unknownField=");
			}

			printf("%s%s", readExpr().c_str(), (i < 10)?", ":"");
		}
		else
			skip(1);
	}

	endFunc();
}

void Script_v1::o1_dummy(FuncParams &params) {
	seek((uint32) -2, SEEK_CUR);
	uint16 size = readUint16();
	skip(size * 2);
}

void  Script_v1::o1_copySprite(FuncParams &params) {
	startFunc(params);
	if (peekUint8(1) == 0)
		print("%d, ", readUint16());
	else
		print("%s, ", readExpr().c_str());

	if (peekUint8(1) == 0)
		print("%d, ", readUint16());
	else
		print("%s, ", readExpr().c_str());

	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());
	print("%s, ", readExpr().c_str());

	print("%d", readUint16());
	endFunc();
}