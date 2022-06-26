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

#ifndef CGE_STRUCTS_H
#define CGE_STRUCTS_H

#include "common/scummsys.h"

#pragma pack(1)

/**
 * The following defines are copied from the cge engine file btfile.h
 */
#define kBtKeySize   13
#define kBtLevel     2
#define kBtPageSize  1024
#define kBtPageSize2 2048

struct CgeBtKeypack {
	char _key[kBtKeySize];
	uint32 _mark;
	uint16 _size;
};

struct CgeBtKeypack2 {
	char _key[kBtKeySize];
	uint32 _mark;
	uint32 _size;
};

struct CgeInner {
	uint8 _key[kBtKeySize];
	uint16 _down;
};

struct CgeHea {
	uint16 _count;
	uint16 _down;
};

struct BtPage {
	CgeHea _hea;
	union {
		// dummy filler to make proper size of union
		uint8 _data[kBtPageSize - sizeof(CgeHea)];
		// inner version of data: key + word-sized page link
		CgeInner _inn[(kBtPageSize - sizeof(CgeHea)) / sizeof(CgeInner)];
		// leaf version of data: key + all user data
		CgeBtKeypack _lea[(kBtPageSize - sizeof(CgeHea)) / sizeof(CgeBtKeypack)];
	};
};

struct BtPage2 {
	CgeHea _hea;
	union {
		// dummy filler to make proper size of union
		uint8 _data[kBtPageSize2 - sizeof(CgeHea)];
		// inner version of data: key + word-sized page link
		CgeInner _inn[(kBtPageSize2 - sizeof(CgeHea)) / sizeof(CgeInner)];
		// leaf version of data: key + all user data
		CgeBtKeypack2 _lea[(kBtPageSize2 - sizeof(CgeHea)) / sizeof(CgeBtKeypack2)];
	};
};

#pragma pack()

#endif
