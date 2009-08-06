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
 * $URL$
 * $Id$
 *
 */

#ifndef COMMON_UTIL_H
#define COMMON_UTIL_H

#include <exception>
#include <stdexcept>
#include <string>

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

} // End of Common namespace

/*
 * Some useful types
 */

typedef unsigned char byte;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef signed char int8;
typedef signed short int16;
#ifdef __amigaos4__
#include <exec/types.h>
#include <stdlib.h>
#else
typedef unsigned int uint32;
typedef signed int int32;
#endif

/*
 * Various utility macros
 */

#if defined(_MSC_VER)

	#define scumm_stricmp stricmp
	#define scumm_strnicmp _strnicmp
	#define snprintf _snprintf

	#define SCUMM_LITTLE_ENDIAN
	#pragma once
	#pragma warning( disable : 4068 ) /* turn off "unknown pragma" warning */
	#pragma warning( disable : 4996 ) /* turn off warnings about unsafe functions */

#elif defined(__MINGW32__)

	#define scumm_stricmp strcasecmp
	#define scumm_stricmp stricmp
	#define scumm_strnicmp strnicmp

	#define SCUMM_LITTLE_ENDIAN

#elif defined(UNIX)
	#define scumm_stricmp strcasecmp
	#define scumm_strnicmp strncasecmp

	#if defined(__DECCXX) /* Assume alpha architecture */
	#define INVERSE_MKID
	#define SCUMM_NEED_ALIGNMENT
	#endif

#else

	#error No system type defined

#endif


/**
 * Throw an exception of this type (or subtype of it), if the tool fails fatally.
 * This type is intended for general errors
 */
class ToolException : public std::runtime_error {
public:
	/**
	 * Construct an exception, with an appropriate error message
	 * A return value for the tool should be supplied if none is appropriate
	 * @todo If the tools are even more C++ized, the tool should decide retcode itself, not by exception
	 *
	 * @param error The error message
	 * @param retcode The return value of the process
	 */
	ToolException(std::string error, int retcode = -1) : std::runtime_error(error), _retcode(retcode) {}

	int _retcode;
};

/**
 * Something unexpected happened while reading / writing to a file
 * Usually premature end, or that it could not be opened (write / read protected)
 */
class AbortException : public ToolException {
public: 
	AbortException() : ToolException("Operation was aborted", -2) {}
};

#endif
