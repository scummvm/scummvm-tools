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
#include "common/util.h"

#define OPCODET(x) _OPCODET(Script_Fascin, x)
#define OPCODEF(x) _OPCODEF(Script_Fascin, x)
#define OPCODEB(x) _OPCODEB(Script_Fascin, x)

const int Script_Fascin::_goblinFuncLookUp[][2] = {
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
	{11, 10},
	{12, 11},
	{1000, 12},
	{1001, 13},
	{1002, 14}
};

Script_Fascin::Script_Fascin(byte *totData, uint32 totSize, ExtTable *extTable) :
	Script_v2(totData, totSize, extTable) {

	setupOpcodes();
}

Script_Fascin::~Script_Fascin() {
}

void Script_Fascin::setupOpcodes() {
	static const OpcodeDrawEntryFascin opcodesDraw[256] = {

		/* 00 */
		{OPCODEF(o2_loadMult), {PARAM_NONE}},
		{OPCODEF(o2_playMult), {PARAM_NONE}},
		{OPCODET(o2_freeMultKeys), {PARAM_UINT16}},
		{OPCODET(oFascin_setWinSize), {PARAM_UINT16, PARAM_UINT16, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX, PARAM_VARINDEX}},
		/* 04 */
		{OPCODET(oFascin_closeWin), {PARAM_EXPR}},
		{OPCODET(oFascin_activeWin), {PARAM_EXPR}},
		{OPCODET(oFascin_openWin), {PARAM_EXPR, PARAM_VARINDEX}},
		{OPCODET(o1_initCursor), {PARAM_VARINDEX, PARAM_VARINDEX, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		/* 08 */
		{OPCODET(o1_initCursorAnim), {PARAM_EXPR, PARAM_INT16, PARAM_INT16, PARAM_INT16}},
		{OPCODET(o1_clearCursorAnim), {PARAM_EXPR}},
		{OPCODET(oFascin_setRenderFlags), {PARAM_EXPR}},
		{OPCODET(oFascin_setWinFlags), {PARAM_EXPR}},
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

	static const OpcodeFuncEntryFascin opcodesFunc[80] = {
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
		{OPCODET(oFascin_copySprite), {PARAM_INT16, PARAM_INT16, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_EXPR, PARAM_INT16}},
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

	static const OpcodeGoblinEntryFascin opcodesGoblin[15] = {
		/* 00 */
		{OPCODET(oFascin_playTirb), {PARAM_NONE}},
		{OPCODET(oFascin_playTira), {PARAM_NONE}},
		{OPCODET(oFascin_loadExtasy), {PARAM_NONE}},
		{OPCODET(oFascin_adlibPlay), {PARAM_NONE}},
		/* 04 */
		{OPCODET(oFascin_adlibStop), {PARAM_NONE}},
		{OPCODET(oFascin_adlibUnload), {PARAM_NONE}},
		{OPCODET(oFascin_loadMus1), {PARAM_NONE}},
		{OPCODET(oFascin_loadMus2), {PARAM_NONE}},
		/* 08 */
		{OPCODET(oFascin_loadMus3), {PARAM_NONE}},
		{OPCODET(oFascin_loadBatt1), {PARAM_NONE}},
		{OPCODET(oFascin_loadBatt2), {PARAM_NONE}},
		{OPCODET(oFascin_loadBatt3), {PARAM_NONE}},
		/* 0C */
		{OPCODET(oFascin_loadMod), {PARAM_NONE}},
		{OPCODET(oFascin_playProtracker), {PARAM_NONE}},
		{OPCODET(o2_stopProtracker), {PARAM_NONE}},
	};

	_opcodesDrawFascin = opcodesDraw;
	_opcodesFuncFascin = opcodesFunc;
	_opcodesGoblinFascin = opcodesGoblin;
}

void Script_Fascin::drawOpcode(byte i, FuncParams &params) {
	FuncType type = _opcodesDrawFascin[i].type;
	params.desc = _opcodesDrawFascin[i].desc;
	OpcodeDrawProcFascin op = _opcodesDrawFascin[i].proc;

	if (type == TYPE_NONE)
		error("No such opcodeDraw: %d", i);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC))
		printFuncDesc(params, _opcodesDrawFascin[i].params);
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_Fascin::funcOpcode(byte i, byte j, FuncParams &params) {
	int n = i*16 + j;
	FuncType type = TYPE_NONE;
	OpcodeFuncProcFascin op = 0;

	if ((i <= 4) && (j <= 15)) {
		op = _opcodesFuncFascin[n].proc;
		params.desc = _opcodesFuncFascin[n].desc;
		type = _opcodesFuncFascin[n].type;
	}

	if (type == TYPE_NONE)
		error("No such opcodeFunc: %d.%d", i, j);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC))
		printFuncDesc(params, _opcodesFuncFascin[n].params);
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}

void Script_Fascin::goblinOpcode(int i, FuncParams &params) {
	int n = -1;
	for (int j = 0; j < ARRAYSIZE(_goblinFuncLookUp); j++)
		if (_goblinFuncLookUp[j][0] == i) {
			n = _goblinFuncLookUp[j][1];
			break;
		}

	FuncType type = TYPE_NONE;
	OpcodeGoblinProcFascin op = 0;

	if (n >= 0) {
		op = _opcodesGoblinFascin[n].proc;
		params.desc = _opcodesGoblinFascin[n].desc;
		type = _opcodesGoblinFascin[n].type;
	}

	if (type == TYPE_NONE)
		error("No such opcodeGoblin: %d (%d)", i, n);
	if ((type == TYPE_TEXTDESC) || (type == TYPE_BOTHDESC)) {
		const Param *param = _opcodesGoblinFascin[n].params;
		if (*param == PARAM_GOB)
			printFuncDesc(params);
		else
			printFuncDesc(params, param);
	}
	if ((type == TYPE_FUNCDESC) || (type == TYPE_BOTHDESC))
		(this->*op)(params);
}
