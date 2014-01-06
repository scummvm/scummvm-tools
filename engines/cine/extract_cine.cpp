/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
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
 *
 * This is a utility to unpack Delphine's Cinematique engine's archive files.
 * Should at least work with Future Wars and Operation Stealth.
 * Supports using Operation Stealth's 'vol.cnf' file as input for selecting
 * which archive files to unpack.
 *
 * Note that this isn't polished code so caveat emptor.
 *
 * FIXME: Make this code endian safe.
 * FIXME: Recognize "vol.cnf" also when given something else than simply "vol.cnf"
 *        as the input file (e.g. "./vol.cnf" or "/games/os/vol.cnf").
 */

#include <assert.h>
#include <string.h>
#include <algorithm>

#include "extract_cine.h"
#include "common/endian.h"

////////////////////////////////////////////////////////////////////////////

ExtractCine::ExtractCine(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {

	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Used to unpack Delphine's Cinematique engine's archive files.";
	_helptext =
		"Usage: " + getName() + " [params] [-o outputdir] <archivefile>\n" +
		_shorthelp + "\n" +
		"Supports using Operation Stealth's 'vol.cnf' file as input.\n";
}

////////////////////////////////////////////////////////////////////////////

uint32 CineUnpacker::readSource() {
	if (_src < _srcBegin || _src + 4 > _srcEnd) {
		_error = true;
		return 0; // The source pointer is out of bounds, returning a default value
	}
	uint32 value = READ_BE_UINT32(_src);
	_src -= 4;
	return value;
}

unsigned int CineUnpacker::rcr(bool inputCarry) {
	unsigned int outputCarry = (_chunk32b & 1);
	_chunk32b >>= 1;
	if (inputCarry) {
		_chunk32b |= 0x80000000;
	}
	return outputCarry;
}

unsigned int CineUnpacker::nextBit() {
	unsigned int carry = rcr(false);
	// Normally if the chunk becomes zero then the carry is one as
	// the end of chunk marker is always the last to be shifted out.
	if (_chunk32b == 0) {
		_chunk32b = readSource();
		_crc ^= _chunk32b;
		carry = rcr(true); // Put the end of chunk marker in the most significant bit
	}
	return carry;
}

unsigned int CineUnpacker::getBits(unsigned int numBits) {
	unsigned int c = 0;
	while (numBits--) {
		c <<= 1;
		c |= nextBit();
	}
	return c;
}

void CineUnpacker::unpackRawBytes(unsigned int numBytes) {
	if (_dst >= _dstEnd || _dst - numBytes + 1 < _dstBegin) {
		_error = true;
		return; // Destination pointer is out of bounds for this operation
	}
	while (numBytes--) {
		*_dst = (byte)getBits(8);
		--_dst;
	}
}

void CineUnpacker::copyRelocatedBytes(unsigned int offset, unsigned int numBytes) {
	if (_dst + offset >= _dstEnd || _dst - numBytes + 1 < _dstBegin) {
		_error = true;
		return; // Destination pointer is out of bounds for this operation
	}
	while (numBytes--) {
		*_dst = *(_dst + offset);
		--_dst;
	}
}

bool CineUnpacker::unpack(const byte *src, unsigned int srcLen, byte *dst, unsigned int dstLen) {
	// Initialize variables used for detecting errors during unpacking
	_error    = false;
	_srcBegin = src;
	_srcEnd   = src + srcLen;
	_dstBegin = dst;
	_dstEnd   = dst + dstLen;

	// Initialize other variables
	_src = _srcBegin + srcLen - 4;
	uint32 unpackedLength = readSource(); // Unpacked length in bytes
	_dst = _dstBegin + unpackedLength - 1;
	_crc = readSource();
	_chunk32b = readSource();
	_crc ^= _chunk32b;

	while (_dst >= _dstBegin && !_error) {
		/*
		Bits  => Action:
		0 0   => unpackRawBytes(3 bits + 1)              i.e. unpackRawBytes(1..8)
		1 1 1 => unpackRawBytes(8 bits + 9)              i.e. unpackRawBytes(9..264)
		0 1   => copyRelocatedBytes(8 bits, 2)           i.e. copyRelocatedBytes(0..255, 2)
		1 0 0 => copyRelocatedBytes(9 bits, 3)           i.e. copyRelocatedBytes(0..511, 3)
		1 0 1 => copyRelocatedBytes(10 bits, 4)          i.e. copyRelocatedBytes(0..1023, 4)
		1 1 0 => copyRelocatedBytes(12 bits, 8 bits + 1) i.e. copyRelocatedBytes(0..4095, 1..256)
		*/
		if (!nextBit()) { // 0...
			if (!nextBit()) { // 0 0
				unsigned int numBytes = getBits(3) + 1;
				unpackRawBytes(numBytes);
			} else { // 0 1
				unsigned int numBytes = 2;
				unsigned int offset   = getBits(8);
				copyRelocatedBytes(offset, numBytes);
			}
		} else { // 1...
			unsigned int c = getBits(2);
			if (c == 3) { // 1 1 1
				unsigned int numBytes = getBits(8) + 9;
				unpackRawBytes(numBytes);
			} else if (c < 2) { // 1 0 x
				unsigned int numBytes = c + 3;
				unsigned int offset   = getBits(c + 9);
				copyRelocatedBytes(offset, numBytes);
			} else { // 1 1 0
				unsigned int numBytes = getBits(8) + 1;
				unsigned int offset   = getBits(12);
				copyRelocatedBytes(offset, numBytes);
			}
		}
	}
	return !_error && (_crc == 0);
}

////////////////////////////////////////////////////////////////////////////

void ExtractCine::unpackFile(Common::File &file) {
	char fileName[15];

	unsigned int entryCount = file.readUint16BE(); // How many entries?
	unsigned int entrySize = file.readUint16BE(); // How many bytes per entry?
	assert(entrySize == 0x1e);
	while (entryCount--) {
		file.read_throwsOnError(fileName, 14);
		fileName[14] = '\0';

		Common::Filename outPath(_outputPath);
		outPath.setFullName(fileName);

		uint32 offset = file.readUint32BE();
		unsigned int packedSize = file.readUint32BE();
		unsigned int unpackedSize = file.readUint32BE();
		// Skip one
		file.readUint32BE();
		unsigned int savedPos = file.pos();

		print("unpacking '%s' ... ", outPath.getFullName().c_str());

		Common::File fpOut(outPath, "wb");

		file.seek(offset, SEEK_SET);
		assert(unpackedSize >= packedSize);
		uint8 *data = (uint8 *)calloc(unpackedSize, 1);
		uint8 *packedData = (uint8 *)calloc(packedSize, 1);
		assert(data);
		assert(packedData);
		file.read_throwsOnError(packedData, packedSize);
		bool status = true;
		if (packedSize != unpackedSize) {
			CineUnpacker cineUnpacker;
			status = cineUnpacker.unpack(packedData, packedSize, data, unpackedSize);
		} else {
			memcpy(data, packedData, packedSize);
		}
		free(packedData);
		fpOut.write(data, unpackedSize);
		free(data);

		if (!status) {
			print("CRC ERROR");
		} else {
			print("ok");
		}
		print(", packedSize %u unpackedSize %u", packedSize, unpackedSize);
		file.seek(savedPos, SEEK_SET);
	}
}

void ExtractCine::fixVolCnfFileName(char *dst, const uint8 *src) {
	char *ext, *end;

	memcpy(dst, src, 8);
	src += 8;
	dst[8] = 0;
	ext = strchr(dst, ' ');
	if (!ext) {
		ext = &dst[8];
	}
	if (*src == ' ') {
		*ext = 0;
	} else {
		*ext++ = '.';
		memcpy(ext, src, 3);
		end = strchr(ext, ' ');
		if (!end) {
			end = &ext[3];
		}
		*end = 0;
	}
}

void ExtractCine::unpackAllResourceFiles(const Common::Filename &filename) {
	Common::File f(filename, "rb");

	uint32 unpackedSize, packedSize;
	{
		char header[8];
		f.read_throwsOnError(header, 8);
		if (memcmp(header, "ABASECP", 7) == 0) {
			unpackedSize = f.readUint32BE();
			packedSize = f.readUint32BE();
		} else {
			unpackedSize = packedSize = f.pos(); /* Get file size */
			f.seek(0, SEEK_SET);
		}
	}

	assert(unpackedSize >= packedSize);
	uint8 *buf = (uint8 *)calloc(unpackedSize, 1);
	assert(buf);
	f.read_throwsOnError(buf, packedSize);

	if (packedSize != unpackedSize) {
		CineUnpacker cineUnpacker;
		if (!cineUnpacker.unpack(buf, packedSize, buf, unpackedSize)) {
			error("Failed to unpack 'vol.cnf' data");
		}
	}

	unsigned int resourceFilesCount = READ_BE_UINT16(&buf[0]);
	unsigned int entrySize = READ_BE_UINT16(&buf[2]);
	print("--- Unpacking all %d resource files from 'vol.cnf' (entrySize = %d):", resourceFilesCount, entrySize);
	char resourceFileName[9];
	for (unsigned int i = 0; i < resourceFilesCount; ++i) {
		memcpy(resourceFileName, &buf[4 + i * entrySize], 8);
		resourceFileName[8] = 0;

		Common::File fpResFile(resourceFileName, "rb");
		print("--- Unpacking resource file %s:", resourceFileName);
		unpackFile(fpResFile);
	}

	free(buf);
}

void ExtractCine::execute() {
	Common::Filename infilename(_inputPaths[0].path);

	std::string fname = infilename.getFullName();
	std::transform(fname.begin(), fname.end(), fname.begin(), toupper);

	if (fname == "VOL.CNF") {
		/* Unpack all archive files listed in 'vol.cnf' */
		unpackAllResourceFiles(infilename);
	} else {
		/* Unpack a single archive file */
		Common::File f(infilename, "rb");

		unpackFile(f);
	}
}
