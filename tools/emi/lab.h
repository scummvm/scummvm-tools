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

#ifndef LAB_H
#define LAB_H

#include <string>
#include <iostream>

#define GT_GRIM 1
#define GT_EMI 2

struct lab_header {
	uint32_t magic;
	uint32_t magic2;
	uint32_t num_entries;
	uint32_t string_table_size;
	uint32_t string_table_offset;
};

struct lab_entry {
	uint32_t fname_offset;
	uint32_t start;
	uint32_t size;
	uint32_t reserved;
};

uint16_t READ_LE_UINT16(const void *ptr) {
	const uint8_t *b = (const uint8_t *)ptr;
	return (b[1] << 8) + b[0];
}
uint32_t READ_LE_UINT32(const void *ptr) {
	const uint8_t *b = (const uint8_t *)ptr;
	return (b[3] << 24) + (b[2] << 16) + (b[1] << 8) + (b[0]);
}

class Lab {
	std::string _filename;
	uint8_t g_type;
	uint32_t i;
	uint32_t offset;
	uint32_t bufSize;
	lab_header head;
	lab_entry *entries;
	char *buf;
	char *str_table;
	FILE *infile;
	void Load(std::string filename);
public:
	Lab(std::string filename) : _filename(filename) {
		// allocate a 1mb buffer to start with
		bufSize = 1024*1024;
		buf = (char *)malloc(bufSize);
		Load(filename);
	}
	~Lab() {
		free(buf);
		delete[] str_table;
		delete[] entries;
	}

	std::istream *getFile(std::string filename);
};

std::istream *getFile(std::string filename, Lab* lab);

#endif
