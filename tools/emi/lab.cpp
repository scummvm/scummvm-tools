/* Residual - A 3D game interpreter
 *
 * Residual is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the AUTHORS
 * file distributed with this source distribution.
 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 *
 * $URL:
 * $Id:
 *
 */

#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <iostream>
#include <fstream>
#include <string>
#include "lab.h"

// TODO: Use common/endian for this
uint16 READ_LE_UINT16(const void *ptr) {
	const uint8 *b = (const uint8 *)ptr;
	return (b[1] << 8) + b[0];
}

uint32 READ_LE_UINT32(const void *ptr) {
	const uint8 *b = (const uint8 *)ptr;
	return (b[3] << 24) + (b[2] << 16) + (b[1] << 8) + (b[0]);
}

void Lab::Load(std::string filename) {
	g_type = GT_EMI; // FIXME, detect game-type properly.
	
	infile = fopen(filename.c_str(), "rb");
	if (infile == 0) {
		std::cout << "Can not open source file: " << filename << std::endl;
		exit(1);
	}
	
	fread(&head.magic, 1, 4, infile);
	fread(&head.magic2, 1, 4, infile);
	uint32 num, s_size, s_offset;
	fread(&num, 1, 4, infile);
	fread(&s_size, 1, 4, infile);
	if(g_type == GT_EMI)
		fread(&s_offset,1,4,infile);
	head.num_entries = READ_LE_UINT32(&num);
	head.string_table_size = READ_LE_UINT32(&s_size);
	if (0 != memcmp(&head.magic, "LABN", 4)) {
		std::cout << "There is no LABN header in source lab-file\n";
		exit(1);
	}
	
	entries = new lab_entry[head.num_entries];
	str_table = new char[head.string_table_size];
	if (!str_table || !entries) {
		std::cout << "Could not allocate memory\n";
		exit(1);
	}
	// Grim-stuff
	if(g_type == GT_GRIM) {
		fread(entries, 1, head.num_entries * sizeof(lab_entry), infile);
		
		fread(str_table, 1, head.string_table_size, infile);
		
	} else if(g_type == GT_EMI) { // EMI-stuff
		// EMI has a string-table-offset
		head.string_table_offset = READ_LE_UINT32(&s_offset) - 0x13d0f;
		// Find the string-table
		fseek(infile, head.string_table_offset, SEEK_SET);
		// Read the entire string table into str-table
		fread(str_table, 1, head.string_table_size, infile);
		fseek(infile, 20, SEEK_SET);
		
		// Decrypt the string table
		uint32 j;
		for (j = 0; j < head.string_table_size; j++)
			if (str_table[j] != 0)
				str_table[j] ^= 0x96;
		fread(entries, 1, head.num_entries * sizeof(lab_entry), infile);
	}
}

int Lab::getIndex(std::string filename) {
	for (i = 0; i < head.num_entries; i++) {
		const char *fname = str_table + READ_LE_UINT32(&entries[i].fname_offset);
		std::string test = std::string(fname);
		if (test != filename)
			continue;
		else
		{
			return i;
		}
	}
	return -1;
}

std::istream* Lab::getFile(std::string filename) {
	if (!buf) {
		printf("Could not allocate memory\n");
		exit(1);
	}
	int index = getIndex(filename);
	if (index == -1) {
		return NULL;
	} else {
		offset = READ_LE_UINT32(&entries[i].start);
		uint32 size = READ_LE_UINT32(&entries[i].size);
		if (bufSize < size) {
			bufSize = size;
			char *newBuf = (char *)realloc(buf, bufSize);
			if (!newBuf) {
				printf("Could not reallocate memory\n");
				exit(1);
			} else {
				buf = newBuf;
			}
		}
		std::fstream *stream;
		stream = new std::fstream(_filename.c_str(), std::ios::in | std::ios::binary);
		stream->seekg(offset, std::ios::beg);
		return stream;
	}
	
	return NULL;
}

int Lab::getLength(std::string filename) {
	int index = getIndex(filename);
	if (index == -1)
		return 0;
	return entries[i].size;
}

std::istream *getFile(std::string filename, Lab* lab) {
	std::istream *stream;
	if (lab) {
		return lab->getFile(filename);
	} else {
		stream = new std::fstream(filename.c_str(), std::ios::in | std::ios::binary);
		
		if (!((std::fstream*)stream)->is_open()) {
			std::cout << "Unable to open file " << filename << std::endl;
			return 0;
		}
		return stream;
	}
}

std::istream *getFile(std::string filename, Lab* lab, int& length) {
	std::istream *stream;
	if (lab) {
		length = lab->getLength(filename);
		return lab->getFile(filename);
	} else {
		stream = new std::fstream(filename.c_str(), std::ios::in | std::ios::binary);
		
		if (!((std::fstream*)stream)->is_open()) {
			std::cout << "Unable to open file " << filename << std::endl;
			return 0;
		}
		stream->seekg(0, std::ios::end);
		length = (int)stream->tellg();
		stream->seekg(0, std::ios::beg);
		return stream;
	}
}
