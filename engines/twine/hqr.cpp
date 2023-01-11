/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
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

#include "engines/twine/hqr.h"
#include "common/file.h"
#include "common/util.h"
#include "common/substream.h"
#include "common/memstream.h"

namespace TwinE {

namespace HQR {

/**
 * Decompress entry based in Yaz0r and Zink decompression code
 * @param dst destination pointer where will be the decompressed entry
 * @param compBuf compressed data pointer
 * @param compSize @p compBuf buffer size
 * @param decompsize real file size after decompression
 * @param mode compression mode used
 */
static void decompressEntry(uint8 *dst, const uint8 *compBuf, uint32 compSize, int32 decompsize, int32 mode) {
	Common::MemoryReadStream stream(compBuf, compSize);
	do {
		uint8 b = stream.readByte();
		for (int32 d = 0; d < 8; d++) {
			int32 length;
			if (!(b & (1 << d))) {
				const uint16 offset = stream.readUint16LE();
				length = (offset & 0x0F) + (mode + 1);
				const uint8 *ptr = dst - (offset >> 4) - 1;
				for (int32 i = 0; i < length; i++) {
					*(dst++) = *(ptr++);
				}
			} else {
				length = 1;
				*(dst++) = stream.readByte();
			}
			decompsize -= length;
			if (decompsize <= 0) {
				return;
			}
		}
	} while (decompsize);
}

/**
 * Get a HQR entry pointer
 * @param filename HQR file name
 * @param index entry index to extract
 * @return entry real size
 */
static int voxEntrySize(const Common::Filename &filename, int32 index, int32 hiddenIndex) {
	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen()) {
		warning("HQR: Could not open %s", filename.getFullPath().c_str());
		return 0;
	}

	uint32 headerSize = file.readUint32LE();
	if ((uint32)index >= headerSize / 4) {
		warning("HQR: Invalid entry index");
		return 0;
	}

	file.seek(index * 4, SEEK_SET);
	uint32 offsetToData = file.readUint32LE();

	file.seek(offsetToData, SEEK_SET);
	uint32 realSize = file.readUint32LE();
	uint32 compSize = file.readUint32LE();

	// exist hidden entries
	for (int32 i = 0; i < hiddenIndex; i++) {
		file.seek(offsetToData + compSize + 10, SEEK_SET); // hidden entry
		offsetToData = offsetToData + compSize + 10;  // current hidden offset

		realSize = file.readUint32LE();
		compSize = file.readUint32LE();
	}

	return realSize;
}

int32 getEntry(uint8 *ptr, const Common::Filename &filename, int32 index) {
	if (!ptr) {
		return 0;
	}

	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen()) {
		warning("HQR: Could not open %s", filename.getFullPath().c_str());
		return 0;
	}

	uint32 headerSize = file.readUint32LE();

	if ((uint32)index >= headerSize / 4) {
		warning("HQR: Invalid entry index");
		return 0;
	}

	file.seek(index * 4, SEEK_SET);
	uint32 offsetToData = file.readUint32LE();

	file.seek(offsetToData, SEEK_SET);
	uint32 realSize = file.readUint32LE();
	uint32 compSize = file.readUint32LE();
	uint16 mode = file.readUint16LE();

	// uncompressed
	if (mode == 0) {
		file.read_throwsOnError(ptr, realSize);
	}
	// compressed: modes (1 & 2)
	else if (mode == 1 || mode == 2) {
		uint8 *compDataPtr = (uint8 *)malloc(compSize);
		file.read_throwsOnError(compDataPtr, compSize);
		decompressEntry(ptr, compDataPtr, compSize, realSize, mode);
		free(compDataPtr);
	}

	return realSize;
}

int32 entrySize(const Common::Filename &filename, int32 index) {
	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen()) {
		warning("HQR: Could not open %s", filename.getFullPath().c_str());
		return 0;
	}

	uint32 headerSize = file.readUint32LE();
	if ((uint32)index >= headerSize / 4) {
		warning("HQR: Invalid entry index");
		return 0;
	}

	file.seek(index * 4, SEEK_SET);
	uint32 offsetToData = file.readUint32LE();

	file.seek(offsetToData, SEEK_SET);
	uint32 realSize = file.readUint32LE();

	return realSize;
}

int32 numEntries(const Common::Filename &filename) {
	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen()) {
		warning("HQR: Could not open %s", filename.getFullPath().c_str());
		return 0;
	}

	uint32 headerSize = file.readUint32LE();
	return ((int)headerSize / 4) - 1;
}

Common::SeekableReadStream *makeReadStream(const Common::Filename &filename, int index) {
	uint8 *data = nullptr;
	const int32 size = getAllocEntry(&data, filename, index);
	if (size == 0) {
		return nullptr;
	}
	return new Common::MemoryReadStream(data, size, DisposeAfterUse::YES);
}

int32 getAllocEntry(uint8 **ptr, const Common::Filename &filename, int32 index) {
	if (*ptr) {
		free(*ptr);
	}
	const int32 size = entrySize(filename, index);
	if (size <= 0) {
		*ptr = nullptr;
		warning("HQR: failed to get entry for index %i from file: %s", index, filename.getFullPath().c_str());
		return 0;
	}
	*ptr = (uint8 *)malloc(size * sizeof(uint8));
	if (!*ptr) {
		warning("HQR: unable to allocate entry memory");
		return 0;
	}
	const int32 entrySize = getEntry(*ptr, filename, index);
	assert(entrySize == size);
	return entrySize;
}

int32 getVoxEntry(uint8 *ptr, const Common::Filename &filename, int32 index, int32 hiddenIndex) {
	if (!ptr) {
		return 0;
	}
	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen()) {
		warning("HQR: Could not open %s", filename.getFullPath().c_str());
		return 0;
	}

	uint32 headerSize = file.readUint32LE();

	if ((uint32)index >= headerSize / 4) {
		warning("HQR: Invalid entry index");
		return 0;
	}

	file.seek(index * 4, SEEK_SET);
	uint32 offsetToData = file.readUint32LE();

	file.seek(offsetToData, SEEK_SET);
	uint32 realSize = file.readUint32LE();
	uint32 compSize = file.readUint32LE();
	uint16 mode = file.readSint16LE();

	// exist hidden entries
	for (int32 i = 0; i < hiddenIndex; i++) {
		file.seek(offsetToData + compSize + 10, SEEK_SET); // hidden entry
		offsetToData = offsetToData + compSize + 10;  // current hidden offset

		realSize = file.readUint32LE();
		compSize = file.readUint32LE();
		mode = file.readUint16LE();
	}

	// uncompressed
	if (mode == 0) {
		file.read_throwsOnError(ptr, realSize);
	}
	// compressed: modes (1 & 2)
	else if (mode == 1 || mode == 2) {
		uint8 *compDataPtr = (uint8 *)malloc(compSize);
		file.read_throwsOnError(compDataPtr, compSize);
		decompressEntry(ptr, compDataPtr, compSize, realSize, mode);
		free(compDataPtr);
	}

	return realSize;
}

int32 getAllocVoxEntry(uint8 **ptr, const Common::Filename &filename, int32 index, int32 hiddenIndex) {
	const int32 size = voxEntrySize(filename, index, hiddenIndex);
	if (size == 0) {
		warning("HQR: vox entry with 0 size found for index: %d", index);
		return 0;
	}

	*ptr = (uint8 *)malloc(size * sizeof(uint8));
	if (!*ptr) {
		warning("HQR: unable to allocate entry memory of size %d for index: %d", size, index);
		return 0;
	}
	const int32 entrySize = getVoxEntry(*ptr, filename, index, hiddenIndex);
	assert(entrySize == size);
	return entrySize;
}

} // namespace HQR

} // namespace TwinE
