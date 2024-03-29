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

#ifndef SOUND_VOC_H
#define SOUND_VOC_H

#include "common/scummsys.h"

namespace Common {
	class File;
}

namespace Audio {

class AudioStream;

#include "common/pack-start.h"	/* START STRUCT PACKING */

struct VocFileHeader {
	uint8 desc[20];
	uint16 datablock_offset;
	uint16 version;
	uint16 id;
} PACKED_STRUCT;

struct VocBlockHeader {
	uint8 blocktype;
	uint8 size[3];
	uint8 sr;
	uint8 pack;
} PACKED_STRUCT;

#include "common/pack-end.h"	/* END STRUCT PACKING */

/**
 * Take a sample rate parameter as it occurs in a VOC sound header, and
 * return the corresponding sample frequency.
 *
 * This method has special cases for the standard rates of 11025 and 22050 kHz,
 * which due to limitations of the format, cannot be encoded exactly in a VOC
 * file. As a consequence, many game files have sound data sampled with those
 * rates, but the VOC marks them incorrectly as 11111 or 22222 kHz. This code
 * works around that and "unrounds" the sampling rates.
 */
extern int getSampleRateFromVOCRate(int vocSR);

/**
 * Try to load a VOC from the given seekable stream. Returns a pointer to memory
 * containing the PCM sample data (allocated with malloc). It is the callers
 * responsibility to dellocate that data again later on! Currently this
 * function only supports uncompressed raw PCM data.
 */
extern byte *loadVOCFromStream(Common::File &stream, int &size, int &rate);

/**
 * Try to load a VOC from the given seekable stream and create an AudioStream
 * from that data. Currently this function only supports uncompressed raw PCM
 * data. Looping is not supported.
 *
 * This function uses loadVOCFromStream() internally.
 */
AudioStream *makeVOCStream(Common::File &stream);

} // End of namespace Audio

#endif
