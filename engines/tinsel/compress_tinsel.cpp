/* compress_tinsel - .smp compressor
 * Copyright (C) 2009 The ScummVM Team
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

// By Jimi (m [underline] kiewitz [AT] users.sourceforge.net)

#include <stdlib.h>

#include "compress.h"
#include "common/endian.h"

#include "compress_tinsel.h"

// data-format of index-file:
//  [pointer to data file DWORD] [pointer to data file DWORD] [pointer to data file DWORD]
//  we use index[0] to signal the engine what data format it's supposed to expect. It may be 'MP3 ', 'OGG ' or 'FLAC'

// data-format of sample-file:
//  [sample-length DWORD] [sample-data]
//  or
//  [subsamplecount DWORD] [sample-length DWORD] [sample-data]
//   where subsamplecount has upmost bit 31 set (that's how one differentiates sample-length and subsamplecount)
// It seems that data-format 1 is used by DiscWorld 1 and data-format 2 is used by DiscWorld 2. Also DiscWorld 1 uses
//  raw-data as samples and DiscWorld 2 uses ADPCM 6-bit encoded data as samples. We suppose that we will need to do ADPCM
//  decoding on all multi-sample data.

// We also copy over the first 5000 bytes of the .smp file, because otherwise we would trash ScummVM detection.

#define TEMP_IDX "compressed.idx"
#define TEMP_SMP "compressed.smp"
#define TEMP_RAW "tempfile.raw"
#define TEMP_ENC "tempfile.enc"

CompressTinsel::CompressTinsel(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_supportsProgressBar = true;

	ToolInput input1;
	input1.format = "*.smp";
	_inputPaths.push_back(input1);

	ToolInput input2;
	input2.format = "*.idx";
	_inputPaths.push_back(input2);

	_shorthelp = "Used to compress tinsel .smp files.";
	_helptext = "\nUsage: " + getName() + " [mode-params] [-o outputname] <infile.smp> <infile.idx>\n";
}

/* Converts raw-data sample in input_smp of size SampleSize to requested dataformat and writes to output_smp */
void CompressTinsel::convertTinselRawSample (uint32 sampleSize) {
	uint32 copyLeft = 0;
	uint32 doneRead = 0;
	char buffer[2048];
	Common::File curFileHandle;

	print("Assuming DW1 sample being 8-bit raw...\n");

	unlink(TEMP_RAW); unlink(TEMP_ENC);
	curFileHandle.open(TEMP_RAW, "wb");
	copyLeft = sampleSize;
	while (copyLeft > 0) {
		doneRead = _input_smp.read_noThrow(buffer, copyLeft > sizeof(buffer) ? sizeof(buffer) : copyLeft);
		if (doneRead <= 0)
			break;
		copyLeft -= (int)doneRead;
		curFileHandle.write(buffer, doneRead);
	}
	curFileHandle.close();

	// Encode this raw data...
	setRawAudioType(true, false, 8); // LE, mono, 8-bit (??)
	encodeAudio(TEMP_RAW, true, 22050, TEMP_ENC, _format);

	// Append compressed data to output_smp
	curFileHandle.open(TEMP_ENC, "rb");
	curFileHandle.seek(0, SEEK_END);
	copyLeft = curFileHandle.pos();
	curFileHandle.seek(0, SEEK_SET);
	// Write size of compressed data
	_output_smp.writeUint32LE(copyLeft);
	// Write actual data
	while (copyLeft > 0) {
		doneRead = curFileHandle.read_noThrow(buffer, copyLeft > sizeof(buffer) ? sizeof(buffer) : copyLeft);
		if (doneRead <= 0)
			break;
		copyLeft -= (int)doneRead;
		_output_smp.write(buffer, doneRead);
	}
}

static const double TinselFilterTable[4][2] = {
	{0, 0 },
	{0.9375, 0},
	{1.796875, -0.8125},
	{1.53125, -0.859375}
};

template<typename T> inline T CLIP (T v, T amin, T amax) {
	if (v < amin)
		return amin;
	else if (v > amax)
		return amax;
	else
		return v;
}

/* Converts ADPCM-data sample in input_smp of size SampleSize to requested dataformat and writes to output_smp */
/* Quick hack together from adpcm.cpp */
void CompressTinsel::convertTinselADPCMSample (uint32 sampleSize) {
	byte *inBuffer, *inPos;
	int16 *outBuffer, *outPos;
	double predictor = 0;
	double k0 = 0, k1 = 0;
	double d0 = 0, d1 = 0;
	uint32 blockAlign, blockPos;
	uint16 chunkData = 0;
	int16 chunkWord = 0;
	uint8 headerByte, filterVal, chunkPos = 0;
	const double eVal = 1.032226562;
	uint32 decodeLeft = 0, decodedCount = 0;
	uint32 uncompressedSize;
	double sample;

	uint32 copyLeft = 0;
	uint32 doneRead = 0;
	char buffer[2048];
	Common::File curFileHandle;

	print("Assuming DW2 sample using ADPCM 6-bit, decoding to 16-bit raw...\n");

	// Allocate buffer for the ADPCM-compressed sample
	inBuffer = (byte *)malloc(sampleSize);
	if (!inBuffer) {
		print("malloc failed!\n");
		return;
	}

	// Allocate buffer for uncompressed sample data (3 bytes will be uncompressed to 8 bytes)
	uncompressedSize = (sampleSize/3)*4*2+16;
	outBuffer = (int16 *)malloc(uncompressedSize);
	if (!outBuffer) {
		print("malloc failed!\n");
		return;
	}

	_input_smp.read_throwsOnError(inBuffer, sampleSize);

	// 1 channel, 22050 rate, block align 24,
	blockAlign = 24; // Fixed for Tinsel 6-bit
	blockPos = blockAlign; // To make sure first header is read

	inPos = inBuffer; outPos = outBuffer;
	decodeLeft = sampleSize;
	while (decodeLeft > 0) {
		if (blockPos == blockAlign) {
			// read Tinsel header
			headerByte = *inPos; inPos++; decodeLeft--;
			filterVal = (headerByte & 0xC0) >> 6;

			if ((headerByte & 0x20) != 0) {
				//Lower 6 bit are negative
				// Negate
				headerByte = ~(headerByte | 0xC0) + 1;
				predictor = 1 << headerByte;
			} else {
				// Lower 6 bit are positive
				// Truncate
				headerByte &= 0x1F;
				predictor = ((double) 1.0) / (1 << headerByte);
			}
			k0 = TinselFilterTable[filterVal][0];
			k1 = TinselFilterTable[filterVal][1];
			blockPos = 0;
			chunkPos = 0;
		}

		switch (chunkPos) {
		case 0:
			chunkData = *inPos; inPos++; decodeLeft--;
			chunkWord = (chunkData << 8) & 0xFC00;
			break;
		case 1:
			chunkData = (chunkData << 8) | *inPos; inPos++; decodeLeft--;
			blockPos++;
			chunkWord = (chunkData << 6) & 0xFC00;
			break;
		case 2:
			chunkData = (chunkData << 8) | *inPos; inPos++; decodeLeft--;
			blockPos++;
			chunkWord = (chunkData << 4) & 0xFC00;
			break;
		case 3:
			chunkData = chunkData << 8;
			blockPos++;
			chunkWord = (chunkData << 2) & 0xFC00;
			break;
		}
		sample = chunkWord;
		sample *= eVal * predictor;
		sample += (d0 * k0) + (d1 * k1);
		d1 = d0;
		d0 = sample;
		*outPos = (int16) CLIP<double>(sample, -32768.0, 32767.0); outPos++;
		decodedCount++;
		chunkPos = (chunkPos + 1) % 4;
	}

	unlink(TEMP_RAW);
	unlink(TEMP_ENC);

	curFileHandle.open(TEMP_RAW, "wb");
	curFileHandle.write(outBuffer, decodedCount*2);

	free(inBuffer);
	free(outBuffer);

	// Encode this raw data...
	setRawAudioType(true, false, 16); // LE, mono, 16-bit
	encodeAudio(TEMP_RAW, true, 22050, TEMP_ENC, _format);

	// Append compressed data to output_smp
	curFileHandle.open(TEMP_ENC, "rb");
	curFileHandle.seek(0, SEEK_END);
	copyLeft = curFileHandle.pos();
	curFileHandle.seek(0, SEEK_SET);
	// Write size of compressed data
	_output_smp.writeUint32LE(copyLeft);
	// Write actual data
	while (copyLeft > 0) {
		doneRead = curFileHandle.read_noThrow(buffer, copyLeft > sizeof(buffer) ? sizeof(buffer) : copyLeft);
		if (doneRead <= 0)
			break;
		copyLeft -= (int)doneRead;
		_output_smp.write(buffer, doneRead);
	}
}

void CompressTinsel::execute() {
	uint32 indexNo = 0;
	uint32 indexCount = 0;
	uint32 indexOffset = 0;
	uint32 loopCount = 0;
	uint32 sampleSize = 0;
	uint32 sampleCount = 0;

	Common::Filename inpath_smp = _inputPaths[0].path;
	Common::Filename inpath_idx = _inputPaths[1].path;

	_input_idx.open(inpath_idx, "rb");
	_input_smp.open(inpath_smp, "rb");

	unlink(TEMP_IDX);
	_output_idx.open(TEMP_IDX, "wb");

	unlink(TEMP_SMP);
	_output_smp.open(TEMP_SMP, "wb");

	_input_idx.seek(0, SEEK_END);
	indexCount = _input_idx.pos() / sizeof(uint32);
	_input_idx.seek(0, SEEK_SET);

	loopCount = indexCount;
	while (loopCount>0) {
		// Update progress
		updateProgress(indexCount - loopCount, indexCount);

		indexOffset = _input_idx.readUint32LE();
		if (indexOffset) {
			if (indexNo==0) {
				error("The sourcefiles are already compressed, aborting...\n");
			}
			// Got sample(s), so convert...
			print("Converting sample %d of %d\n", indexNo, indexCount);

			// Seek to Sample in input-file and read SampleSize
			_input_smp.seek(indexOffset, SEEK_SET);
			sampleSize = _input_smp.readUint32LE();

			// Write offset of new data to new index file
			_output_idx.writeUint32LE(_output_smp.pos());

			if (sampleSize & 0x80000000) {
				// multiple samples in ADPCM format
				sampleCount = sampleSize & ~0x80000000;
				// Write sample count to new sample file
				_output_smp.writeUint32LE(sampleSize);
				while (sampleCount>0) {
					sampleSize = _input_smp.readUint32LE();
					convertTinselADPCMSample(sampleSize);
					sampleCount--;
				}
			} else {
				// just one sample in raw format
				convertTinselRawSample(sampleSize);
			}
		} else {
			if (indexNo==0) {
				// Write signature as index 0
				switch (_format) {
				case AUDIO_MP3: _output_idx.writeUint32BE(MKID_BE('MP3 ')); break;
				case AUDIO_VORBIS: _output_idx.writeUint32BE(MKID_BE('OGG ')); break;
				case AUDIO_FLAC: _output_idx.writeUint32BE(MKID_BE('FLAC')); break;
				default: throw ToolException("Unknown audio format!");
				}
			} else {
				_output_idx.writeUint32LE(0);
			}
		}
		loopCount--;
		indexNo++;
	}

	/* And some clean-up :-) */
	unlink(TEMP_RAW);
	unlink(TEMP_ENC);
}


#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressTinsel tinsel(argv[0]);
	return tinsel.run(argc, argv);
}
#endif
