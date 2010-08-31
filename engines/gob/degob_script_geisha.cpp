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
#include "common/endian.h"
#include "common/util.h"

#define OPCODET(x) _OPCODET(Script_Geisha, x)
#define OPCODEF(x) _OPCODEF(Script_Geisha, x)
#define OPCODEB(x) _OPCODEB(Script_Geisha, x)

const int Script_Geisha::_goblinFuncLookUp[][2] = {
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

Script_Geisha::Script_Geisha(byte *totData, uint32 totSize, ExtTable *extTable) :
	Script_v1(totData, totSize, extTable) {

	setupOpcodes();
}

Script_Geisha::~Script_Geisha() {
}

void Script_Geisha::setupOpcodes() {
	static const OpcodeDrawEntryGeisha opcodesDraw[256] = {
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

	static const OpcodeFuncEntryGeisha opcodesFunc[80] = {
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
		{OPCODET(oGeisha_loadSound), {PARAM_EXPR, PARAM_EXPR}},
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

	static const OpcodeGoblinEntryGeisha opcodesGoblin[8] = {
		/* 00 */
		{OPCODET(oGeisha_OG_0), {PARAM_INT16, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(oGeisha_OG_1), {PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(oGeisha_loadGeisha), {PARAM_GOB}},
		{OPCODET(oGeisha_adlibPlay), {PARAM_GOB}},
		/* 04 */
		{OPCODET(oGeisha_adlibStop), {PARAM_GOB}},
		{OPCODET(oGeisha_OG_NOP), {PARAM_GOB}},
		{OPCODET(oGeisha_OG_6), {PARAM_GOB}},
		{OPCODET(oGeisha_OG_7), {PARAM_GOB}},
	};

	_opcodesDrawGeisha = opcodesDraw;
	_opcodesFuncGeisha = opcodesFunc;
	_opcodesGoblinGeisha = opcodesGoblin;
}

void Script_Geisha::drawOpcode(byte i, FuncParams &params) {
	FuncType type = _opcodesDrawGeisha[i].type;
	params.desc = _opcodesDrawGeisha[i].desc;
	OpcodeDrawProcGeisha op = _opcodesDrawGeisha[i].proc;

	if (type == TYPE_NONE)
		error("No such opcodeDraw: %d", i);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC))
		printFuncDesc(params, _opcodesDrawGeisha[i].params);
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_Geisha::funcOpcode(byte i, byte j, FuncParams &params) {
	int n = i*16 + j;
	FuncType type = TYPE_NONE;
	OpcodeFuncProcGeisha op = 0;

	if ((i <= 4) && (j <= 15)) {
		op = _opcodesFuncGeisha[n].proc;
		params.desc = _opcodesFuncGeisha[n].desc;
		type = _opcodesFuncGeisha[n].type;
	}

	if (type == TYPE_NONE)
		error("No such opcodeFunc: %d.%d", i, j);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC))
		printFuncDesc(params, _opcodesFuncGeisha[n].params);
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_Geisha::goblinOpcode(int i, FuncParams &params) {
	int n = -1;
	for (int j = 0; j < ARRAYSIZE(_goblinFuncLookUp); j++)
		if (_goblinFuncLookUp[j][0] == i) {
			n = _goblinFuncLookUp[j][1];
			break;
		}

	FuncType type = TYPE_NONE;
	OpcodeGoblinProcGeisha op = 0;

	if (n >= 0) {
		op = _opcodesGoblinGeisha[n].proc;
		params.desc = _opcodesGoblinGeisha[n].desc;
		type = _opcodesGoblinGeisha[n].type;
	}

	if (type == TYPE_NONE)
		error("No such opcodeGoblin: %d (%d)", i, n);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC)) {
		const Param *param = _opcodesGoblinGeisha[n].params;
		if (*param == PARAM_GOB)
			printFuncDesc(params);
		else
			printFuncDesc(params, param);
	}
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

