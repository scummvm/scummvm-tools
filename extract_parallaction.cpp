/* extract_parallaction - Extractor for Nippon Safe archives
 * Copyright (C) 2007 The ScummVM project
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

#include "extract_parallaction.h"

Archive::Archive() {
	_file = NULL;
	_fileData = NULL;
	_fileSize = 0;
	_filePos = 0;

	_numFiles = 0;
	_numSlots = 0;
}

Archive::~Archive() {
	closeSubfile();

	if (_file) {
		fclose(_file);
	}

	_file = NULL;
}

bool Archive::isPackedSubfile(byte* data) {
	return (data[0] == 'P' && data[1] == 'P' && data[2] == '2' && data[3] == '0');
}

uint32 Archive::getSizeOfPackedSubfile(byte* packedData, uint32 packedSize) {
	uint32 size = *(uint32*)(packedData + packedSize - 4);

	return ((size & 0xFF00)) | ((size & 0xFF0000) >> 16);
}

int32 Archive::findSubfile(const char* filename) {
	for (uint32 i = 0; i < _numFiles; i++) {
		if (!scumm_stricmp(filename, _names[i])) return i;
	}

	return -1;
}

void Archive::unpackSubfile(byte* packedData, uint32 packedSize) {
	ppdepack(packedData, _fileData, packedSize, _fileSize);
}

void Archive::closeSubfile() {
	if (_fileData) {
		free(_fileData);
	}

	_fileData = NULL;
	_fileSize = 0;
	_filePos = 0;
}

uint32 Archive::getSizeOfSubfile() {
	return _fileSize;
}

void Archive::openSubfile(uint32 index) {
	if (index >= _numFiles) {
		exit(-3);
	}

	closeSubfile();

	uint32 srcOffset = _offsets[index];
	uint32 srcSize = _sizes[index];

	byte *srcData = (byte*)malloc(srcSize);
	fseek(_file, srcOffset, SEEK_SET);
	fread(srcData, 1, srcSize, _file);

	if (isPackedSubfile(srcData)) {
		_fileSize = getSizeOfPackedSubfile(srcData, srcSize);
		_fileData = (byte*)malloc(_fileSize);

		unpackSubfile(srcData, srcSize);

		free(srcData);
	} else {
		_fileSize = srcSize;
		_fileData = srcData;
	}
}

void Archive::openSubfile(const char* filename) {

	int32 index = findSubfile(filename);

	if (index == -1) {
		exit(-2);
	}

	openSubfile(index);

	return;
}

void Archive::readSubfile(byte* buf, uint32 size) {
	assert(size + _filePos <= _fileSize);
	memcpy(buf, _fileData + _filePos, size);
	_filePos += size;
	return;
}

void Archive::open(const char* filename, bool smallArchive) {
	uint16 maxEntries = (smallArchive) ? 180 : 384;

	_file = fopen(filename, "rb");
	if (!_file) {
		exit(10);
	}

	strcpy(_name, filename);

	fseek(_file, ARCHIVE_HEADER_SIZE, SEEK_SET);
	fread(_names, maxEntries + 1, ARCHIVE_FILENAME_LEN, _file);

	uint32 i;
	for (i = 0; i < maxEntries; i++) {
		if (_names[i][0] == '\0') {
			break;
		}
	}

	_numFiles = i;

	if (_numFiles < maxEntries) {
		uint32* t = (uint32*)_names[i];

		for (; i < (uint32)maxEntries + 1; i++) {
			if (*t != 0) {
				break;
			}

			t += 8;
		}
	}

	if (i > (uint32)maxEntries) {
		exit(9);
	}

	_numSlots = i;

	fprintf(stderr, "%i files found in %i slots (%i empty)\n", _numFiles, _numSlots, _numSlots - _numFiles);

	fseek(_file, _numSlots * ARCHIVE_FILENAME_LEN + ARCHIVE_HEADER_SIZE, SEEK_SET);

	for (i = 0; i < _numSlots; i++) {
		_sizes[i] = readUint32BE(_file);
	}

	if (smallArchive) {
		_offsets[0] = 0x1966;
	} else {
		_offsets[0] = 0x4000;
	}

	ftell(_file);

	for (i = 1; i < _numSlots; i++) {
		_offsets[i] = _offsets[i-1] + _sizes[i-1];
	}

	return;
}

void Archive::dumpStructs(FILE* dump) {
	char arcName[32];

	char* s = strrchr(_name, '/');

	if (s == NULL) {
		s = strrchr(_name, '\\');
		if (s == NULL) s = _name;
	}

	char* d = arcName;

	for (; *s; ) {
		*d++ = toupper(*s++);
	}

	*d = '\0';

	for (uint32 i = 0; i < _numFiles; i++) {
		fprintf(dump, "{ \"%s\",%*s%5i, kArchive%s, %7i },\n", _names[i], 32-(int)strlen(_names[i]), " ", _sizes[i], arcName+1, _offsets[i]);
	}
}


#define val(p) ((p)[0]<<16 | (p)[1] << 8 | (p)[2])

uint32  depackedlen(byte* packed, uint32 plen) {
	if (packed[0] != 'P' || packed[1] != 'P' ||
		packed[2] != '2' || packed[3] != '0')
			return 0; /* not a powerpacker file */

	return val(packed+plen-4);
}

static uint32 shift_in;
static uint32 counter = 0;
static byte *source;

static uint32 get_bits(uint32 n) {
	uint32 result = 0;
	uint32 i;

	for (i = 0; i < n; i++) {
		if (counter == 0) {
			counter = 8;
			shift_in = *--source;
		}

		result = (result<<1) | (shift_in & 1);
		shift_in >>= 1;
		counter--;
	}

	return result;
}

void ppdepack(byte* packed, byte* depacked, uint32 plen, uint32 unplen) {
	byte *dest;
	int n_bits;
	int idx;
	uint32 bytes;
	int to_add;
	uint32 offset;
	byte offset_sizes[4];
	uint32 i;

	shift_in = 0;
	counter = 0;

	offset_sizes[0] = packed[4];	/* skip signature */
	offset_sizes[1] = packed[5];
	offset_sizes[2] = packed[6];
	offset_sizes[3] = packed[7];

	/* initialize source of bits */
	source = packed + plen - 4;

	dest = depacked + unplen;

	/* skip bits */
	get_bits(source[3]);

	/* do it forever, i.e., while the whole file isn't unpacked */
	while (1) {
		/* copy some bytes from the source anyway */
		if (get_bits(1) == 0) {
			bytes = 0;

			do {
				to_add = get_bits(2);
				bytes += to_add;
			} while (to_add == 3);

			for (i = 0; i <= bytes; i++) {
				*--dest = get_bits(8);
			}

			if (dest <= depacked) {
				return;
			}
		}

		/* decode what to copy from the destination file */
		idx = get_bits(2);
		n_bits = offset_sizes[idx];
		/* bytes to copy */
		bytes = idx + 1;

		if (bytes == 4)	{ /* 4 means >=4 */
			/* and maybe a bigger offset */
			if (get_bits(1) == 0) {
				offset = get_bits(7);
			} else {
				offset = get_bits(n_bits);
			}

			do {
				to_add = get_bits(3);
				bytes += to_add;
			} while (to_add == 7);
		} else {
			offset = get_bits(n_bits);
		}

		for (i = 0; i <= bytes; i++) {
			dest[-1] = dest[offset];
			dest--;
		}

		if (dest <= depacked) {
			return;
		}
	}

}

void optDump(const char* file, const char* dir, bool smallArchive) {
	Archive arc;
	arc.open(file, smallArchive);

	for (uint32 i = 0; i < arc._numFiles; i++) {

		arc.openSubfile(i);

		char path[260];
		strcpy(path, dir);

		char *d = path + (strlen(path) - 1);

		if (*d == '/') {
			d++;
		} else {
			if (*d != '\\') {
				d++;
			}

			*d++ = '/';
			*d = '\0';
		}

		for (char *s = arc._names[i]; *s; s++, d++) {
			*d = *s == '/' ? '_' : *s;
		}

		*d = '\0';

		FILE* ofile = fopen(path, "wb");
		fwrite(arc._fileData, 1, arc._fileSize, ofile);
		fclose(ofile);
	}
}

int main(int argc, char *argv[]) {

	if (argc < 3) {
		printf("Usage: %s [--small] <file> <outputdir>", argv[0]);
		exit(1);
	}

	if (!strcmp(argv[1], "--small")) {
		optDump(argv[2], argv[3], true);
	} else {
		optDump(argv[1], argv[2], false);
	}

	return 0;
}
