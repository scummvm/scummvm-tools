/* ResidualVM - A 3D game interpreter
 *
 * ResidualVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
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
 */

#ifndef LAB_H
#define LAB_H

#include "common/endian.h"
#include <string>
#include <iostream>

#define GT_GRIM 1
#define GT_EMI 2

struct lab_header {
	uint32 magic;
	uint32 magic2;
	uint32 num_entries;
	uint32 string_table_size;
	uint32 string_table_offset;
};

struct lab_entry {
	uint32 fname_offset;
	uint32 start;
	uint32 size;
	uint32 reserved;
};

class Lab {
	std::string _filename;
	uint8 g_type;
	uint32 i;
	uint32 offset;
	uint32 bufSize;
	lab_header head;
	lab_entry *entries;
	char *buf;
	char *str_table;
	FILE *infile;
	void Load(std::string filename);
public:
	Lab(std::string filename) : _filename(filename) {
		// allocate a 1mb buffer to start with
		bufSize = 1024 * 1024;
		buf = (char *)malloc(bufSize);
		Load(filename);
	}
	~Lab() {
		free(buf);
		delete[] str_table;
		delete[] entries;
	}

	std::istream *getFile(std::string filename);
	int getIndex(std::string filename);
	int getLength(std::string filename);
};

std::istream *getFile(std::string filename, Lab *lab);
std::istream *getFile(std::string filename, Lab *lab, int &length);

#endif
