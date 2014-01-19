/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Extractor for Nippon Safe archives */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "extract_parallaction.h"

Archive::Archive(Tool &tool) : _tool(tool) {
	_fileData = NULL;
	_fileSize = 0;
	_filePos = 0;

	_numFiles = 0;
	_numSlots = 0;
}

Archive::~Archive() {
	closeSubfile();
}

bool Archive::isPackedSubfile(byte *data) {
	return (data[0] == 'P' && data[1] == 'P' && data[2] == '2' && data[3] == '0');
}

uint32 Archive::getSizeOfPackedSubfile(byte *packedData, uint32 packedSize) {
	uint32 size = *(uint32 *)(packedData + packedSize - 4);

	return ((size & 0xFF00)) | ((size & 0xFF0000) >> 16);
}

int32 Archive::findSubfile(const char *filename) {
	for (uint32 i = 0; i < _numFiles; i++) {
		if (!scumm_stricmp(filename, _names[i])) return i;
	}

	return -1;
}

void Archive::unpackSubfile(byte *packedData, uint32 packedSize) {
	ppdepack(packedData, _fileData, packedSize, _fileSize);
}

void Archive::closeSubfile() {
	free(_fileData);
	_fileData = NULL;
	_fileSize = 0;
	_filePos = 0;
}

uint32 Archive::getSizeOfSubfile() {
	return _fileSize;
}

void Archive::openSubfile(uint32 index) {
	if (index >= _numFiles)
		throw ToolException("File index out of bounds.");

	closeSubfile();

	uint32 srcOffset = _offsets[index];
	uint32 srcSize = _sizes[index];

	byte *srcData = (byte *)malloc(srcSize);
	_file.seek(srcOffset, SEEK_SET);
	_file.read_throwsOnError(srcData, srcSize);

	if (isPackedSubfile(srcData)) {
		_fileSize = getSizeOfPackedSubfile(srcData, srcSize);
		_fileData = (byte *)malloc(_fileSize);

		unpackSubfile(srcData, srcSize);

		free(srcData);
	} else {
		_fileSize = srcSize;
		_fileData = srcData;
	}
}

void Archive::openSubfile(const char *filename) {

	int32 index = findSubfile(filename);

	if (index == -1)
		throw ToolException("Could not open subfile.");

	openSubfile(index);

	return;
}

void Archive::readSubfile(byte *buf, uint32 size) {
	assert(size + _filePos <= _fileSize);
	memcpy(buf, _fileData + _filePos, size);
	_filePos += size;
	return;
}

void Archive::open(const char *filename, bool smallArchive) {
	uint16 maxEntries = (smallArchive) ? 180 : 384;

	_file.open(filename, "rb");

	strcpy(_name, filename);

	_file.seek(ARCHIVE_HEADER_SIZE, SEEK_SET);
	_file.read_throwsOnError(_names, (maxEntries + 1) * ARCHIVE_FILENAME_LEN);

	uint32 i;
	for (i = 0; i < maxEntries; i++) {
		if (_names[i][0] == '\0')
			break;
	}

	_numFiles = i;

	if (_numFiles < maxEntries) {
		uint32 *t = (uint32*)_names[i];

		for (; i < (uint32)maxEntries + 1; i++) {
			if (*t != 0)
				break;
			t += 8;
		}
	}

	if (i > (uint32)maxEntries)
		throw ToolException("Could not open archive, index out of range.");

	_numSlots = i;

	_tool.print("%i files found in %i slots (%i empty)", _numFiles, _numSlots, _numSlots - _numFiles);

	_file.seek(_numSlots * ARCHIVE_FILENAME_LEN + ARCHIVE_HEADER_SIZE, SEEK_SET);

	for (i = 0; i < _numSlots; i++) {
		_sizes[i] = _file.readUint32BE();
	}

	if (smallArchive) {
		_offsets[0] = 0x1966;
	} else {
		_offsets[0] = 0x4000;
	}

	_file.pos();

	for (i = 1; i < _numSlots; i++)
		_offsets[i] = _offsets[i-1] + _sizes[i-1];

	return;
}

void Archive::dumpStructs(FILE *dump) {
	char arcName[32];

	char *s = strrchr(_name, '/');
	if (s == NULL) {
		s = strrchr(_name, '\\');
		if (s == NULL) s = _name;
	}

	char *d = arcName;

	for (; *s; ) *d++ = toupper(*s++);
	*d = '\0';

	for (uint32 i = 0; i < _numFiles; i++) {
		fprintf(dump, "{ \"%s\",%*s%5i, kArchive%s, %7i },\n", _names[i], 32-(int)strlen(_names[i]), " ", _sizes[i], arcName+1, _offsets[i]);
	}
}


#define val(p) ((p)[0]<<16 | (p)[1] << 8 | (p)[2])

uint32  depackedlen(byte *packed, uint32 plen) {
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

void ppdepack(byte *packed, byte *depacked, uint32 plen, uint32 unplen) {
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

			for (i = 0; i <= bytes; i++)
				*--dest = get_bits(8);

			if (dest <= depacked)
				return;
		}

		/* decode what to copy from the destination file */
		idx = get_bits(2);
		n_bits = offset_sizes[idx];
		/* bytes to copy */
		bytes = idx + 1;
		if (bytes == 4)	{ /* 4 means >=4 */
			/* and maybe a bigger offset */
			if (get_bits(1) == 0)
				offset = get_bits(7);
			else
				offset = get_bits(n_bits);

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

		if (dest <= depacked)
			return;
	}

}

ExtractParallaction::ExtractParallaction(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {

	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Extract data files from games built on the Parallaction engine.";
	_helptext = "\nUsage: " + _name + " [-o <output dir> = out/] [--small] <file>\n" + _shorthelp + "\n";
}

void ExtractParallaction::parseExtraArguments() {
	if (!_arguments.empty() && _arguments.front() == "--small") {
		_small = true;
		_arguments.pop_front();
	}
}

void ExtractParallaction::execute() {

	Common::Filename inpath(_inputPaths[0].path);
	Common::Filename &outpath = _outputPath;

	if (outpath.empty())
		outpath.setFullPath("out/");

	Archive arc(*this);
	arc.open(inpath.getFullPath().c_str(), _small);

	for (uint32 i = 0; i < arc._numFiles; i++) {

		arc.openSubfile(i);

		char filename[260], * d = filename;

		for (char *s = arc._names[i]; *s; s++, d++)
			*d = *s == '/' ? '_' : *s;
		*d = '\0';

		outpath.setFullName(filename);

		Common::File ofile(outpath, "wb");
		ofile.write(arc._fileData, arc._fileSize);
	}
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractParallaction parallaction(argv[0]);
	return parallaction.run(argc, argv);
}
#endif

