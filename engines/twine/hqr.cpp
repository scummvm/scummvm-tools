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

#include <string.h>

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
	while (decompsize > 0 && !stream.eos()) {
		uint8 b = stream.readByte();
		for (int32 d = 0; d < 8 && decompsize > 0; d++) {
			int32 length;
			if (!(b & (1 << d))) {
				if (stream.eos())
					return;
				const uint16 offset = stream.readUint16LE();
				length = (offset & 0x0F) + (mode + 1);
				const uint8 *ptr = dst - (offset >> 4) - 1;
				for (int32 i = 0; i < length; i++) {
					*(dst++) = *(ptr++);
				}
			} else {
				if (stream.eos())
					return;
				length = 1;
				*(dst++) = stream.readByte();
			}
			decompsize -= length;
			if (decompsize <= 0) {
				return;
			}
		}
	}
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

	file.seek(4 + index * 4, SEEK_SET);
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

	file.seek(4 + index * 4, SEEK_SET);
	uint32 offsetToData = file.readUint32LE();

	if (offsetToData == 0 || offsetToData >= file.size())
		return 0;

	file.seek(offsetToData, SEEK_SET);
	if ((uint32)file.pos() + 10u > file.size())
		return 0;

	uint32 realSize = file.readUint32LE();
	uint32 compSize = file.readUint32LE();
	uint16 mode = file.readUint16LE();

	// uncompressed
	if (mode == 0) {
		if (file.pos() + realSize > file.size())
			return 0;
		file.read_throwsOnError(ptr, realSize);
	}
	// compressed: modes (1 & 2)
	else if (mode == 1 || mode == 2) {
		if (file.pos() + compSize > file.size())
			return 0;
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

	file.seek(4 + index * 4, SEEK_SET);
	uint32 offsetToData = file.readUint32LE();

	if (offsetToData == 0 || offsetToData >= file.size())
		return 0;

	file.seek(offsetToData, SEEK_SET);
	if ((uint32)file.pos() + 4u > file.size())
		return 0;

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
	const int32 bytesRead = getEntry(*ptr, filename, index);
	assert(bytesRead == size);
	return bytesRead;
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

	file.seek(4 + index * 4, SEEK_SET);
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
	const int32 entryBytes = getVoxEntry(*ptr, filename, index, hiddenIndex);
	assert(entryBytes == size);
	return entryBytes;
}

bool loadHqrEntry(const Common::Filename &filename, int32 index, HqrPackEntry &out) {
	out.blank = true;
	out.data.clear();

	uint8 *data = nullptr;
	const int32 size = getAllocEntry(&data, filename, index);
	if (size <= 0 || !data)
		return true;

	out.blank = false;
	out.data.assign(data, data + size);
	free(data);
	return true;
}

static bool readRawDiskBlock(const Common::Filename &filename, int32 index, std::vector<uint8> &out) {
	out.clear();

	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen())
		return false;

	const uint32 headerSize = file.readUint32LE();
	if ((uint32)index >= headerSize / 4)
		return false;

	file.seek(4 + index * 4, SEEK_SET);
	const uint32 offsetToData = file.readUint32LE();
	if (offsetToData == 0 || offsetToData >= file.size())
		return false;

	file.seek(offsetToData, SEEK_SET);
	if ((uint32)file.pos() + 10u > file.size())
		return false;

	const uint32 realSize = file.readUint32LE();
	const uint32 compSize = file.readUint32LE();
	const uint16 mode = file.readUint16LE();
	const uint32 payloadSize = (mode == 0) ? realSize : compSize;
	if ((uint32)file.pos() + payloadSize > file.size())
		return false;

	out.resize(10 + payloadSize);
	out[0] = (uint8)(realSize);
	out[1] = (uint8)(realSize >> 8);
	out[2] = (uint8)(realSize >> 16);
	out[3] = (uint8)(realSize >> 24);
	out[4] = (uint8)(compSize);
	out[5] = (uint8)(compSize >> 8);
	out[6] = (uint8)(compSize >> 16);
	out[7] = (uint8)(compSize >> 24);
	out[8] = (uint8)(mode);
	out[9] = (uint8)(mode >> 8);
	file.read_throwsOnError(out.data() + 10, payloadSize);
	return true;
}

bool copyHqrFile(const Common::Filename &src, const Common::Filename &dst) {
	Common::File in;
	in.open(src, "r");
	if (!in.isOpen())
		return false;

	const int64 size = in.size();
	if (size <= 0)
		return false;

	std::vector<uint8> buf((size_t)size);
	in.read_throwsOnError(buf.data(), (uint32)size);
	in.close();

	Common::File out;
	out.open(dst, "wb");
	if (!out.isOpen())
		return false;

	out.write(buf.data(), (uint32)size);
	return true;
}

std::vector<uint8> makeUncompressedDiskBlock(const std::vector<uint8> &data) {
	std::vector<uint8> out(10 + data.size());
	const uint32 realSize = (uint32)data.size();
	out[0] = (uint8)(realSize);
	out[1] = (uint8)(realSize >> 8);
	out[2] = (uint8)(realSize >> 16);
	out[3] = (uint8)(realSize >> 24);
	out[4] = out[0];
	out[5] = out[1];
	out[6] = out[2];
	out[7] = out[3];
	out[8] = 0;
	out[9] = 0;
	if (!data.empty())
		memcpy(out.data() + 10, data.data(), data.size());
	return out;
}

bool patchHqrArchive(const Common::Filename &templateFile, const Common::Filename &outFile,
		const std::map<int32_t, std::vector<uint8>> &patches) {
	if (patches.empty())
		return copyHqrFile(templateFile, outFile);

	Common::File in;
	in.open(templateFile, "r");
	if (!in.isOpen())
		return false;

	const uint32 headerSize = in.readUint32LE();
	const int32 numEntries = (int32)(headerSize / 4) - 1;
	if (numEntries <= 0)
		return false;

	std::vector<uint32> oldOffsets((size_t)numEntries + 1, 0);
	for (int32 i = 0; i <= numEntries; i++) {
		in.seek(4 + (uint32)i * 4, SEEK_SET);
		oldOffsets[(size_t)i] = in.readUint32LE();
	}

	std::vector<uint32> newOffsets((size_t)numEntries + 1, 0);
	std::vector<uint8> fileData;
	fileData.reserve(in.size());
	fileData.resize(headerSize, 0);
	fileData[0] = (uint8)(headerSize);
	fileData[1] = (uint8)(headerSize >> 8);
	fileData[2] = (uint8)(headerSize >> 16);
	fileData[3] = (uint8)(headerSize >> 24);

	uint32 dataOffset = headerSize;
	std::vector<uint8> block;

	for (int32 i = 0; i < numEntries; i++) {
		const auto patchIt = patches.find(i);
		if (patchIt != patches.end()) {
			block = patchIt->second;
		} else if (!readRawDiskBlock(templateFile, i, block)) {
			block.clear();
		}

		if (!block.empty()) {
			newOffsets[(size_t)i] = dataOffset;
			fileData.insert(fileData.end(), block.begin(), block.end());
			dataOffset += (uint32)block.size();
		}
	}
	newOffsets[(size_t)numEntries] = dataOffset;

	for (int32 i = 0; i <= numEntries; i++) {
		const uint32 off = newOffsets[(size_t)i];
		const size_t pos = 4 + (size_t)i * 4;
		fileData[pos] = (uint8)(off);
		fileData[pos + 1] = (uint8)(off >> 8);
		fileData[pos + 2] = (uint8)(off >> 16);
		fileData[pos + 3] = (uint8)(off >> 24);
	}

	Common::File out;
	out.open(outFile, "wb");
	if (!out.isOpen())
		return false;

	out.write(fileData.data(), fileData.size());
	return true;
}

} // namespace HQR

} // namespace TwinE
