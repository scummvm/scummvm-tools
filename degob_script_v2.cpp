/* DeGob - GobEngine Script disassembler
 * Copyright (C) 2008 The ScummVM project
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

#include "degob_script.h"

#define OPCODET(x) _OPCODET(Script_v2, x)
#define OPCODEF(x) _OPCODEF(Script_v2, x)
#define OPCODEB(x) _OPCODEB(Script_v2, x)

const int Script_v2::_goblinFuncLookUp[][2] = {
	{0, 0},
	{1, 1},
	{2, 2},
	{4, 3},
	{5, 4},
	{6, 5},
	{7, 6},
	{8, 7},
	{9, 8},
	{10, 9},
	{12, 10},
	{13, 71},
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
	{100, 39},
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
	{3, 71}
};

Script_v2::Script_v2(byte *totData, uint32 totSize, ExtTable *extTable) :
	Script_v1(totData, totSize, extTable) {

	setupOpcodes();
}

Script_v2::~Script_v2() {
}

void Script_v2::setupOpcodes() {
	static const OpcodeDrawEntryV2 opcodesDraw[256] = {
		/* 00 */
		{OPCODEF(o2_loadMult), {PARAM_NONE}},
		{OPCODEF(o2_playMult), {PARAM_NONE}},
		{OPCODET(o2_freeMultKeys), {PARAM_UINT16}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 04 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{OPCODET(o1_initCursor), {PARAM_VARINDEX, PARAM_VARINDEX, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		/* 08 */
		{OPCODET(o1_initCursorAnim), {PARAM_EXPR, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_clearCursorAnim), {PARAM_EXPR}},
		{OPCODET(o2_setRenderFlags), {PARAM_EXPR}},
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
		{OPCODET(o2_multSub), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		/* 14 */
		{OPCODET(o2_initMult), {PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODET(o1_freeMult), {PARAM_NONE}},
		{OPCODET(o1_animate), {PARAM_NONE}},
		{OPCODEF(o2_loadMultObject), {PARAM_NONE}},
		/* 18 */
		{OPCODET(o1_getAnimLayerInfo), {PARAM_EXPR, PARAM_EXPR, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODET(o1_getObjAnimSize), {PARAM_EXPR, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODEF(o1_loadStatic), {PARAM_NONE}},
		{OPCODET(o1_freeStatic), {PARAM_EXPR}},
		/* 1C */
		{OPCODET(o2_renderStatic), {PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o2_loadCurLayer), {PARAM_EXPR, PARAM_EXPR}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 20 */
		{OPCODET(o2_playCDTrack), {PARAM_EXPR}},
		{OPCODET(o2_waitCDTrackEnd), {PARAM_NONE}},
		{OPCODET(o2_stopCD), {PARAM_NONE}},
		{OPCODET(o2_readLIC), {PARAM_EXPR}},
		/* 24 */
		{OPCODET(o2_freeLIC), {PARAM_NONE}},
		{OPCODET(o2_getCDTrackPos), {PARAM_VARINDEX, PARAM_VARINDEX}},
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
		{OPCODET(o2_loadFontToSprite), {PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
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
		{OPCODEF(o2_totSub), {PARAM_NONE}},
		{OPCODET(o2_switchTotSub), {PARAM_UINT16, PARAM_UINT16}},
		{OPCODEF(o2_pushVars), {PARAM_NONE}},
		{OPCODEF(o2_popVars), {PARAM_NONE}},
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
		{OPCODEF(o2_loadMapObjects), {PARAM_NONE}},
		{OPCODET(o2_freeGoblins), {PARAM_NONE}},
		{OPCODET(o2_moveGoblin), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o2_writeGoblinPos), {PARAM_VARINDEX, PARAM_VARINDEX, PARAM_EXPR}},
		/* 54 */
		{OPCODET(o2_stopGoblin), {PARAM_EXPR}},
		{OPCODET(o2_setGoblinState), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o2_placeGoblin), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
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
		{OPCODET(o2_initScreen), {PARAM_UINT8, PARAM_UINT8, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o2_scroll), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o2_setScrollOffset), {PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o2_playImd), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		/* 84 */
		{OPCODET(o2_getImdInfo), {PARAM_EXPR, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODET(o2_openItk), {PARAM_EXPR}},
		{OPCODET(o2_closeItk), {PARAM_NONE}},
		{OPCODET(o2_setImdFrontSurf), {PARAM_NONE}},
		/* 88 */
		{OPCODET(o2_resetImdFrontSurf), {PARAM_NONE}},
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

	static const OpcodeFuncEntryV2 opcodesFunc[80] = {
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
		{OPCODEF(o2_assign), {PARAM_NONE}},
		{OPCODEF(o1_loadSpriteToPos), {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 0C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 10 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{OPCODEF(o2_printText), {PARAM_NONE}},
		{OPCODEF(o1_loadTot), {PARAM_NONE}},
		{OPCODEF(o1_palLoad), {PARAM_NONE}},
		/* 14 */
		{OPCODET(o1_keyFunc), {PARAM_INT16}},
		{OPCODET(o1_capturePush), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_capturePop), {PARAM_NONE}},
		{OPCODET(o2_animPalInit), {PARAM_INT16, PARAM_EXPR, PARAM_EXPR}},
		/* 18 */
		{OPCODET(o2_addHotspot), {PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_UINT16}},
		{OPCODET(o2_removeHotspot), {PARAM_EXPR}},
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
		{OPCODEF(o2_goblinFunc), {PARAM_NONE}},
		{OPCODET(o2_createSprite), {PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
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
		{OPCODET(o2_stopSound), {PARAM_EXPR}},
		{OPCODEF(o2_loadSound), {PARAM_NONE}},
		{OPCODET(o1_freeSoundSlot), {PARAM_EXPR}},
		/* 3C */
		{OPCODET(o1_waitEndPlay), {PARAM_NONE}},
		{OPCODET(o1_playComposition), {PARAM_VARINDEX, PARAM_EXPR}},
		{OPCODET(o2_getFreeMem), {PARAM_VARINDEX, PARAM_VARINDEX}},
		{OPCODET(o2_checkData), {PARAM_EXPR, PARAM_VARINDEX}},
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
		{OPCODET(o2_readData), {PARAM_EXPR, PARAM_VARINDEX, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o2_writeData), {PARAM_EXPR, PARAM_VARINDEX, PARAM_EXPR, PARAM_EXPR}},
		{OPCODET(o1_manageDataFile), {PARAM_EXPR}},
	};

	static const OpcodeGoblinEntryV2 opcodesGoblin[72] = {
		/* 00 */
		{OPCODEF(o2_loadInfogramesIns), {PARAM_NONE}},
		{OPCODET(o2_startInfogrames), {PARAM_UINT16}},
		{OPCODET(o2_stopInfogrames), {PARAM_UINT16}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 04 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 08 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{OPCODEF(o2_playInfogrames), {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 0C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 10 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 14 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 18 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 1C */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 20 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		/* 24 */
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{OPCODEF(o2_handleGoblins), {PARAM_NONE}},
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
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
		{TYPE_NONE, 0, 0, {PARAM_NONE}},
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
		{OPCODEF(o1_dummy), {PARAM_NONE}}
	};

	_opcodesDrawV2 = opcodesDraw;
	_opcodesFuncV2 = opcodesFunc;
	_opcodesGoblinV2 = opcodesGoblin;
}

void Script_v2::drawOpcode(byte i, FuncParams &params) {
	FuncType type = _opcodesDrawV2[i].type;
	params.desc = _opcodesDrawV2[i].desc;
	OpcodeDrawProcV2 op = _opcodesDrawV2[i].proc;

	if (type == TYPE_NONE)
		error("No such opcodeDraw: %d", i);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC))
		printFuncDesc(params, _opcodesDrawV2[i].params);
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_v2::funcOpcode(byte i, byte j, FuncParams &params) {
	int n = i*16 + j;
	FuncType type = TYPE_NONE;
	OpcodeFuncProcV2 op = 0;

	if ((i <= 4) && (j <= 15)) {
		op = _opcodesFuncV2[n].proc;
		params.desc = _opcodesFuncV2[n].desc;
		type = _opcodesFuncV2[n].type;
	}

	if (type == TYPE_NONE)
		error("No such opcodeFunc: %d.%d", i, j);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC))
		printFuncDesc(params, _opcodesFuncV2[n].params);
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_v2::goblinOpcode(int i, FuncParams &params) {
	int n = -1;
	for (int j = 0; j < ARRAYSIZE(_goblinFuncLookUp); j++)
		if (_goblinFuncLookUp[j][0] == i) {
			n = _goblinFuncLookUp[j][1];
			break;
		}

	FuncType type = TYPE_NONE;
	OpcodeGoblinProcV2 op = 0;

	if (n >= 0) {
		op = _opcodesGoblinV2[n].proc;
		params.desc = _opcodesGoblinV2[n].desc;
		type = _opcodesGoblinV2[n].type;
	}

	if (type == TYPE_NONE)
		error("No such opcodeGoblin: %d (%d)", i, n);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC)) {
		const Param *param = _opcodesGoblinV2[n].params;
		if (*param == PARAM_GOB)
			printFuncDesc(params);
		else
			printFuncDesc(params, param);
	}
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_v2::o2_goblinFunc(FuncParams &params) {
	FuncParams gobParams;
	int16 cmd;

	cmd = (int16) readUint16();
	skip(2);

	if (cmd != 101)
		goblinOpcode(cmd, gobParams);
}

void Script_v2::o2_totSub(FuncParams &params) {
	startFunc(params);

	uint8 length = readUint8();
	if (!(length & 0x80)) {
		for (uint16 i = 0; i < length; i++)
			print("%c", (char) readUint8());
	} else
		print("%s", readExpr().c_str());

	print(", %d", readUint8());

	endFunc();
}

void Script_v2::o2_assign(FuncParams &params) {
	uint8 type = peekUint8();
	std::string varIndex = readVarIndex();

	if (peekUint8() == 99) {
		skip(1);
		uint8 loopCount = readUint8();

		for (uint16 i = 0; i < loopCount; i++) {
			std::string expr = readExpr();
			printIndent();
			print("%s[%d] = %s;\n", varIndex.c_str(), (type == 24) ? (i * 2) : i, expr.c_str());
		}
	} else {
		std::string expr = readExpr();

		printIndent();
		print("%s = %s;\n", varIndex.c_str(), expr.c_str());
	}
}

void Script_v2::o2_pushVars(FuncParams &params) {
	uint8 count = readUint8();

	for (uint16 i = 0; i < count; i++) {
		printIndent();
		if ((peekUint8() == 25) || (peekUint8() == 28)) {
			print("push(%s, animDataSize);\n", readVarIndex().c_str());
			skip(1);
		} else
			print("push(%s, 4);\n", readExpr().c_str());
	}
}

void Script_v2::o2_popVars(FuncParams &params) {
	uint8 count = readUint8();

	for (uint16 i = 0; i < count; i++) {
		printIndent();
		print("pop(%s);\n", readVarIndex().c_str());
	}
}

void Script_v2::o2_loadSound(FuncParams &params) {
	o1_loadSound(params);
}

void Script_v2::o2_loadMult(FuncParams &params) {
	uint16 id;
	bool hasImds;

	id = readUint16();
	if (id & 0x8000) {
		id &= 0x7FFF;
		skip(1);
	}

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
	hasImds = ((count1 & 0x80) != 0);
		count1 &= 0x7F;
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
	data += 2 + count1 * (4 + (hasImds ? 0 : 24));

	// soundKeys
	count1 = (int16) READ_LE_UINT16(data);
	data += 2;
	for (int i = 0; i < count1; i++, data += (12 + (hasImds ? 0 : 24))) {
		int16 cmd = (int16) READ_LE_UINT16(data + 2);

		if ((cmd == 1) || (cmd == 4))
			skip(2);
		else if (cmd == 3)
			skip(4);
	}

	// ImdKeys
	if (hasImds) {
		int16 sSize = (int16) readUint16();
		skip(sSize * 2);

		if (getVerScript() >= 51) {
			sSize = (int16) readUint16();
			if (sSize > 0)
				skip(sSize * 14);
		}
	}

	delete[] extData;
}

void Script_v2::o2_loadMultObject(FuncParams &params) {
	o1_loadMultObject(params);
}

void Script_v2::o2_loadMapObjects(FuncParams &params) {
	int16 id;

	startFunc(params);

	print("%s, ", readVarIndex().c_str());

	id = (int16) readUint16();
	print("%d", id);
	if (id != -1) {
		int16 count = (int16) readUint16();
		print(", %d", count);
		for (int i = 0; i < count; i++)
			print(", %d", (int16) readUint16());
	}

	endFunc();
}

void Script_v2::o2_playMult(FuncParams &params) {
	uint16 multData = readUint16();

	startFunc(params);
	print("%d, %d", multData >> 1, multData & 0x01);
	endFunc();
}

void Script_v2::o2_printText(FuncParams &params) {
	o1_printText(params);
}

void Script_v2::o2_loadInfogramesIns(FuncParams &params) {
	startFunc(params);
	print("var8_%d", readUint16() * 4);
	endFunc();
}

void Script_v2::o2_playInfogrames(FuncParams &params) {
	startFunc(params);
	print("var8_%d", readUint16() * 4);
	endFunc();
}

void Script_v2::o2_handleGoblins(FuncParams &params) {
	startFunc(params);
	print("var32_%d, ", readUint16() * 4);
	print("var32_%d, ", readUint16() * 4);
	print("var32_%d, ", readUint16() * 4);
	print("var32_%d, ", readUint16() * 4);
	print("var32_%d, ", readUint16() * 4);
	print("var32_%d", readUint16() * 4);
	endFunc();
}
