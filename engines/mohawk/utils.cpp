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

#include "engines/mohawk/utils.h"

#include "common/endian.h"
#include "common/util.h"

#define ATOM_STCO MKID_BE('stco')
#define ATOM_MOOV MKID_BE('moov')
#define ATOM_TRAK MKID_BE('trak')
#define ATOM_MDIA MKID_BE('mdia')
#define ATOM_MINF MKID_BE('minf')
#define ATOM_STBL MKID_BE('stbl')

#define BUF_SIZE 8192

void copyBytes(Common::File *src, Common::File *outputFile, uint32 bytesToCopy) {
	byte tempBuffer[BUF_SIZE];

	if (src->pos() + bytesToCopy > src->size()) {
		error("Unexpected end of stream (pos: %d + bytesToCopy: %d > totalSize: %d",
		      src->pos(), bytesToCopy, src->size());
	}

	while (bytesToCopy > 0) {
		size_t bytesCopied = src->read_noThrow(tempBuffer, MIN<uint32>(bytesToCopy, BUF_SIZE));
		outputFile->write(tempBuffer, bytesCopied);
		bytesToCopy -= bytesCopied;
	}
}

void adjustQuickTimeAtomOffsets(Common::File *src, uint32 parentSize, int32 offset, Common::File *outputFile) {
	static const uint32 kAtomHeaderSize = sizeof(uint32) + sizeof(uint32); // size, type
	uint32 totalSize = 0;

	while (totalSize + kAtomHeaderSize < parentSize) {
		uint32 atomSize = src->readUint32BE();
		uint32 atomType = src->readUint32BE();
		outputFile->writeUint32BE(atomSize);
		outputFile->writeUint32BE(atomType);

		if (atomSize == 1) {
			error("64 bit extended size is not supported in QuickTime");
		}

		if (atomSize < kAtomHeaderSize) {
			error("Invalid atom size %d", atomSize);
		}

		if (atomType == ATOM_MOOV || atomType == ATOM_TRAK
		    || atomType == ATOM_MDIA || atomType == ATOM_MINF
		    || atomType == ATOM_STBL) {
			// Recurse into container atom types
			adjustQuickTimeAtomOffsets(src, atomSize - kAtomHeaderSize, offset, outputFile);

		} else if (atomType == ATOM_STCO) {
			copyBytes(src, outputFile, 4); // Copy flags

			uint32 chunkCount = src->readUint32BE();
			outputFile->writeUint32BE(chunkCount);

			for (uint32 i = 0; i < chunkCount; i++) {
				uint32 chunkOffset = src->readUint32BE() + offset;
				outputFile->writeUint32BE(chunkOffset);
			}

		} else {
			copyBytes(src, outputFile, atomSize - kAtomHeaderSize);
		}

		totalSize += atomSize;
	}

	if (totalSize != parentSize)
		error("Unexpected position in atom %d (expected %d)", totalSize, parentSize);
}