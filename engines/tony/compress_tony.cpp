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

#include <stdlib.h>

#include "compress.h"
#include "common/endian.h"
#include "compress_tony.h"

#define TEMP_RAW "tempfile.raw"
#define TEMP_ENC "tempfile.enc"

template<typename T> inline T CLIP (T v, T amin, T amax) {
	if (v < amin)
		return amin;
	else if (v > amax)
		return amax;
	else
		return v;
}

CompressTony::CompressTony(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_supportsProgressBar = true;

	ToolInput input1;
	input1.format = "*.adp";
	_inputPaths.push_back(input1);

	_shorthelp = "Used to compress Tony Tough's .adp files.";
	_helptext = "\nUsage: " + getName() + " [mode-params] [-o outputname] <infile.adp>\n";
}

// This table is used to adjust the step for use on the next sample.
// We could half the table, but since the lookup index used is always
// a 4-bit nibble, it's more efficient to just keep it as it is.
const int16 _stepAdjustTable[16] = {
	-1, -1, -1, -1, 2, 4, 6, 8,
	-1, -1, -1, -1, 2, 4, 6, 8
};

const int16 _imaTable[89] = {
	7,    8,    9,   10,   11,   12,   13,   14,
	16,   17,   19,   21,   23,   25,   28,   31,
	34,   37,   41,   45,   50,   55,   60,   66,
	73,   80,   88,   97,  107,  118,  130,  143,
	157,  173,  190,  209,  230,  253,  279,  307,
	337,  371,  408,  449,  494,  544,  598,  658,
	724,  796,  876,  963, 1060, 1166, 1282, 1411,
	1552, 1707, 1878, 2066, 2272, 2499, 2749, 3024,
	3327, 3660, 4026, 4428, 4871, 5358, 5894, 6484,
	7132, 7845, 8630, 9493,10442,11487,12635,13899,
	15289,16818,18500,20350,22385,24623,27086,29794,
	32767
};

int16 CompressTony::decodeIMA(byte code, int channel) {
	int32 E = (2 * (code & 0x7) + 1) * _imaTable[_status.ima_ch[channel].stepIndex] / 8;
	int32 diff = (code & 0x08) ? -E : E;
	int32 samp = CLIP<int32>(_status.ima_ch[channel].last + diff, -32768, 32767);

	_status.ima_ch[channel].last = samp;
	_status.ima_ch[channel].stepIndex += _stepAdjustTable[code];
	_status.ima_ch[channel].stepIndex = CLIP<int32>(_status.ima_ch[channel].stepIndex, 0, 88);

	return samp;
}

/* Converts ADPCM-data sample in input_adp to requested dataformat and writes to output_smp */
/* Quick hack together from adpcm.cpp */
void CompressTony::convertTonyADPCMSample() {
	byte *inBuffer;
	int16 *outBuffer;

	uint32 doneRead = 0;
	char buffer[2048];
	Common::File curFileHandle;

	uint32 rate = _input_adp.readUint32LE();
	uint32 channels = _input_adp.readUint32LE();

	printf("rate %d channels %d\n", rate, channels);
	printf("original size %d\n", _input_adp.size());

	int sampleSize = _input_adp.size() - 12; // 4 (signature) + 4 (rate) + 4 (channels)

	// Allocate buffer for the ADPCM-compressed sample
	inBuffer = (byte *)malloc(sampleSize);
	if (!inBuffer) {
		print("malloc failed!");
		return;
	}
	_input_adp.read_throwsOnError(inBuffer, sampleSize);

	// Allocate buffer for uncompressed sample data (3 bytes will be uncompressed to 8 bytes)
	uint32 uncompressedSize = sampleSize * 2;
	printf("uncompressed %d bytes\n", uncompressedSize * 2);

	outBuffer = (int16 *)malloc(uncompressedSize * sizeof(int16));
	if (!outBuffer) {
		print("malloc failed!");
		free(inBuffer);
		return;
	}

	int decodedSampleCount = 0;
	int16 decodedSamples[2];
	uint32 samples;
	byte data;
	memset(&_status, 0, sizeof(_status));

	for (samples = 0; samples < uncompressedSize; samples++) {
		if (decodedSampleCount == 0) {
			data = inBuffer[samples >> 1];
			decodedSamples[0] = decodeIMA((data >> 4) & 0x0f, 0);
			decodedSamples[1] = decodeIMA((data >> 0) & 0x0f, channels == 2 ? 1 : 0);
			decodedSampleCount = 2;
		}
		// (1 - (count - 1)) ensures that _decodedSamples acts as a FIFO of depth 2
		outBuffer[samples] = decodedSamples[1 - (decodedSampleCount - 1)];
		decodedSampleCount--;
	}

	Common::removeFile("TEMP.RAW");
	Common::removeFile(TEMP_RAW);
	Common::removeFile(TEMP_ENC);

	curFileHandle.open(TEMP_RAW, "wb");
	for (uint32 i = 0; i < uncompressedSize; i++)
		curFileHandle.writeUint16LE(outBuffer[i]);
	curFileHandle.close();

	free(inBuffer);
	free(outBuffer);

	// Encode this raw data...
	setRawAudioType(true, true, 16); // LE, stereo, 16-bit
	encodeAudio(TEMP_RAW, true, rate, TEMP_ENC, _format);

	// Append compressed data to output_smp
	curFileHandle.open(TEMP_ENC, "rb");
	curFileHandle.seek(0, SEEK_END);
	uint32 copyLeft = curFileHandle.pos();
	curFileHandle.seek(0, SEEK_SET);

	// Write size of compressed data
	// Write actual data
	while (copyLeft > 0) {
		doneRead = curFileHandle.read_noThrow(buffer, copyLeft > sizeof(buffer) ? sizeof(buffer) : copyLeft);
		if (doneRead <= 0)
			break;
		copyLeft -= (int)doneRead;
		_output_enc.write(buffer, doneRead);
	}
}

static const char f_hdr[] = {
	'A', 'D', 'P', 0x10, 0
};

void CompressTony::execute() {
	Common::Filename inpath_adp = _inputPaths[0].path;
	Common::Filename &outpath = _outputPath;

	if (outpath.empty())
		outpath = inpath_adp.getPath();


	char buf[2048];
	_input_adp.open(inpath_adp, "rb");
	_input_adp.read_throwsOnError(buf, 4);
	if (strncmp(buf, f_hdr, 4)) {
		error("Bad ADP");
	}

	Common::Filename outpath_enc = outpath;
	outpath_enc.setFullName(inpath_adp.getFullName());
	switch(_format) {
	case AUDIO_MP3:
		outpath_enc.setExtension(".MP3");
		break;
	case AUDIO_VORBIS:
		outpath_enc.setExtension(".OGG");
		break;
	case AUDIO_FLAC:
		outpath_enc.setExtension(".FLA");
		break;
	default:
		throw ToolException("Unknown audio format");
		break;
	}

	Common::removeFile(outpath_enc.getFullPath().c_str());
	_output_enc.open(outpath_enc, "wb");

	convertTonyADPCMSample();

	/* And some clean-up :-) */
	Common::removeFile(TEMP_RAW);
	Common::removeFile(TEMP_ENC);
}


#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressTony tony(argv[0]);
	return tony.run(argc, argv);
}
#endif
