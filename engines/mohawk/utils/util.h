/* Scumm Tools
 * Copyright (C) 2004-2006  The ScummVM Team
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
 * $URL: https://scummvm.svn.sourceforge.net/svnroot/scummvm/tools/branches/gsoc2009-gui/utils/util.h $
 * $Id: util.h 40868 2009-05-24 15:19:28Z lordhoto $
 *
 */

#ifndef COMMON_UTIL_H
#define COMMON_UTIL_H

namespace Common {


#ifdef MIN
#undef MIN
#endif

#ifdef MAX
#undef MAX
#endif

template<typename T> inline T ABS (T x)			{ return (x>=0) ? x : -x; }
template<typename T> inline T MIN (T a, T b)	{ return (a<b) ? a : b; }
template<typename T> inline T MAX (T a, T b)	{ return (a>b) ? a : b; }
template<typename T> inline T CLIP (T v, T amin, T amax)
		{ if (v < amin) return amin; else if (v > amax) return amax; else return v; }

/**
 * List of game language.
 */
enum Language {
	EN_ANY,     // Generic English (when only one game version exist)
	EN_USA,
	EN_GRB,

	DE_DEU,
	FR_FRA,
	IT_ITA,
	PT_BRA,
	ES_ESP,
	JA_JPN,
	ZH_TWN,
	KO_KOR,
	SE_SWE,
	HB_ISR,
	RU_RUS,
	CZ_CZE,
	NL_NLD,
	NB_NOR,
	PL_POL,

	UNK_LANG = -1	// Use default language (i.e. none specified)
};

/**
 * List of game platforms. Specifying a platform for a target can be used to
 * give the game engines a hint for which platform the game data file are.
 * This may be optional or required, depending on the game engine and the
 * game in question.
 */
enum Platform {
	kPlatformPC,
	kPlatformAmiga,
	kPlatformAtariST,
	kPlatformMacintosh,
	kPlatformFMTowns,
	kPlatformWindows,
	kPlatformNES,
	kPlatformC64,
	kPlatformLinux,
	kPlatformAcorn,
	kPlatformSegaCD,
	kPlatform3DO,
//	kPlatformPCEngine,

	kPlatformUnknown = -1
};
}

#endif
