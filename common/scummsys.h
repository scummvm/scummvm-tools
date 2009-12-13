/* ScummVM Tools
 * Copyright (C) 2002-2009 The ScummVM project
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

#ifndef COMMON_SCUMMSYS_H
#define COMMON_SCUMMSYS_H


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


/*
 * GCC specific stuff
 */
#if defined(__GNUC__)
	#define GCC_PACK __attribute__((packed))
#else
	#define GCC_PACK
#endif

#ifndef ARRAYSIZE
#define ARRAYSIZE(x) ((int)(sizeof(x) / sizeof(x[0])))
#endif

#ifndef FORCEINLINE
#define FORCEINLINE static inline
#endif

#if defined(__GNUC__)
#define NORETURN_PRE
#define NORETURN_POST	__attribute__((__noreturn__))
#elif defined(_MSC_VER)
#define NORETURN_PRE	_declspec(noreturn)
#define NORETURN_POST
#else
#define NORETURN_PRE
#define NORETURN_POST
#endif


#endif
