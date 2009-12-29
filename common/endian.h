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

#ifndef COMMON_ENDIAN_H
#define COMMON_ENDIAN_H

#include "common/scummsys.h"

#if defined(INVERSE_MKID)
#define MKID_BE(a) ((uint32) \
		(((a) >> 24) & 0x000000FF) | \
		(((a) >>  8) & 0x0000FF00) | \
		(((a) <<  8) & 0x00FF0000) | \
		(((a) << 24) & 0xFF000000))

#else
#  define MKID_BE(a) ((uint32)(a))
#endif

static inline uint32 SWAP_32(uint32 a) {
	return uint16(((a >> 24) & 0xFF) | ((a >> 8) & 0xFF00) | ((a << 8) & 0xFF0000) |
		((a << 24) & 0xFF000000));
}

static inline uint16 SWAP_16(uint16 a) {
	return uint16(((a >> 8) & 0xFF) | ((a << 8) & 0xFF00));
}


FORCEINLINE uint16 READ_LE_UINT16(const void *ptr) {
	const byte *b = (const byte *)ptr;
	return uint16((b[1] << 8) + b[0]);
}
FORCEINLINE uint32 READ_LE_UINT32(const void *ptr) {
	const byte *b = (const byte *)ptr;
	return (b[3] << 24) + (b[2] << 16) + (b[1] << 8) + (b[0]);
}
FORCEINLINE void WRITE_LE_UINT16(void *ptr, uint16 value) {
	byte *b = (byte *)ptr;
	b[0] = (byte)(value >> 0);
	b[1] = (byte)(value >> 8);
}
FORCEINLINE void WRITE_LE_UINT32(void *ptr, uint32 value) {
	byte *b = (byte *)ptr;
	b[0] = (byte)(value >>  0);
	b[1] = (byte)(value >>  8);
	b[2] = (byte)(value >> 16);
	b[3] = (byte)(value >> 24);
}

FORCEINLINE uint16 READ_BE_UINT16(const void *ptr) {
	const byte *b = (const byte *)ptr;
	return uint16((b[0] << 8) + b[1]);
}
FORCEINLINE uint32 READ_BE_UINT32(const void *ptr) {
	const byte *b = (const byte *)ptr;
	return uint32((b[0] << 24) + (b[1] << 16) + (b[2] << 8) + (b[3]));
}
FORCEINLINE void WRITE_BE_UINT16(void *ptr, uint16 value) {
	byte *b = (byte *)ptr;
	b[0] = (byte)(value >> 8);
	b[1] = (byte)(value >> 0);
}
FORCEINLINE void WRITE_BE_UINT32(void *ptr, uint32 value) {
	byte *b = (byte *)ptr;
	b[0] = (byte)(value >> 24);
	b[1] = (byte)(value >> 16);
	b[2] = (byte)(value >>  8);
	b[3] = (byte)(value >>  0);
}


#endif
