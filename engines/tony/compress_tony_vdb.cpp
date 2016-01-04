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
#include "common/str.h"
#include "compress_tony_vdb.h"

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

CompressTonyVDB::CompressTonyVDB(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_supportsProgressBar = true;

	ToolInput input1;
	input1.format = "*.vdb";
	_inputPaths.push_back(input1);

	_shorthelp = "Used to compress Tony Tough's .vdb files.";
	_helptext = "\nUsage: " + getName() + " [mode-params] <infile.vdb>\n";
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

int16 CompressTonyVDB::decodeIMA(byte code, int channel) {
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
bool CompressTonyVDB::convertTonyADPCMSample() {
	Common::File curFileHandle;

	int decodedSampleCount = 0;
	int16 decodedSamples[2];
	uint32 samples;
	byte data;
	memset(&_status, 0, sizeof(_status));

	for (samples = 0; samples < _uncompressedSize; samples++) {
		if (decodedSampleCount == 0) {
			data = _inBuffer[samples >> 1];
			decodedSamples[0] = decodeIMA((data >> 4) & 0x0f, 0);
			decodedSamples[1] = decodeIMA((data >> 0) & 0x0f, 0); // 1 channel, hardcoded
			decodedSampleCount = 2;
		}
		// (1 - (count - 1)) ensures that _decodedSamples acts as a FIFO of depth 2
		_outBuffer[samples] = decodedSamples[1 - (decodedSampleCount - 1)];
		decodedSampleCount--;
	}

	Common::removeFile(TEMP_RAW);
	Common::removeFile(TEMP_ENC);

	curFileHandle.open(TEMP_RAW, "wb");
	for (uint32 i = 0; i < _uncompressedSize; i++)
		curFileHandle.writeUint16LE(_outBuffer[i]);
	curFileHandle.close();
	
	// Encode this raw data...
	setRawAudioType(true, false, 16); // LE, stereo, 16-bit
	encodeAudio(TEMP_RAW, true, _rate, TEMP_ENC, _format);

	return true;
}

static const char vdb_hdr[] = {
	'V', 'D', 'B', '1', 0
};

void CompressTonyVDB::execute() {
	Common::Filename inpath_adp = _inputPaths[0].path;
	Common::Filename &outpath = _outputPath;

	if (outpath.empty())
		outpath = inpath_adp.getPath();

	char buf[2048];
	_input_vdb.open(inpath_adp, "rb");
	_input_vdb.seek(-8, SEEK_END);
	int numFiles = _input_vdb.readUint32LE();
	_input_vdb.read_throwsOnError(buf, 4);

	if (strncmp(buf, vdb_hdr, 4)) {
		error("Unexpected file signature\n");
	}

	_input_vdb.seek(-8 - (numFiles * 12), SEEK_END);
	VoiceHeader *vh = new VoiceHeader[numFiles];

	for (int i = 0; i < numFiles; i++) {
		vh[i]._offset = _input_vdb.readUint32LE();
		vh[i]._code = _input_vdb.readUint32LE();
		vh[i]._parts = _input_vdb.readUint32LE();
	}

	Common::Filename outpath_enc = outpath;
	outpath_enc.setFullName(inpath_adp.getFullName());
	switch(_format) {
	case AUDIO_MP3:
		outpath_enc.setExtension(".MDB");
		break;
	case AUDIO_VORBIS:
		outpath_enc.setExtension(".ODB");
		break;
	case AUDIO_FLAC:
		outpath_enc.setExtension(".FDB");
		break;
	default:
		throw ToolException("Unknown audio format");
		break;
	}

	Common::removeFile(outpath_enc.getFullPath().c_str());
	_output_enc.open(outpath_enc, "wb");

	for (int i = 0; i < numFiles; i++) {
		// Update progress
		updateProgress(i, numFiles);

		_input_vdb.seek(vh[i]._offset, SEEK_SET);

		for (int j = 0; j < vh[i]._parts; j++) {
			_sampleSize = _input_vdb.readUint32LE();
			_rate = _input_vdb.readUint32LE();
			_inBuffer = new byte[_sampleSize];
			printf("%d\t%d\t%d/%d\n", _input_vdb.pos() - 8, _sampleSize + 8, j + 1, vh[i]._parts);
			_input_vdb.read_throwsOnError(_inBuffer, _sampleSize);
			_uncompressedSize = _sampleSize * 2;
			_outBuffer = new int16[_uncompressedSize];
			if (convertTonyADPCMSample()) {
				if (j == 0)
					vh[i]._offset = _output_enc.pos();

				Common::File curFileHandle;
				curFileHandle.open(TEMP_ENC, "rb");
				curFileHandle.seek(0, SEEK_END);
				uint32 copyLeft = curFileHandle.pos();
				curFileHandle.seek(0, SEEK_SET);
				_output_enc.writeUint32LE(copyLeft);
				uint32 doneRead = 0;
				char buffer[2048];
				while (copyLeft > 0) {
					doneRead = curFileHandle.read_noThrow(buffer, copyLeft > sizeof(buffer) ? sizeof(buffer) : copyLeft);
					if (doneRead <= 0)
						break;
					copyLeft -= (int)doneRead;
					_output_enc.write(buffer, doneRead);
				}
				curFileHandle.close();
			}
			delete[](_inBuffer);
			delete[](_outBuffer);
		}
	}
	for (int i = 0; i < numFiles; i++) {
		_output_enc.writeUint32LE(vh[i]._offset);
		_output_enc.writeUint32LE(vh[i]._code);
		_output_enc.writeUint32LE(vh[i]._parts);
	}

	_output_enc.writeUint32LE(numFiles);
	switch(_format) {
	case AUDIO_MP3:
		_output_enc.writeByte('M');
		break;
	case AUDIO_VORBIS:
		_output_enc.writeByte('O');
		break;
	case AUDIO_FLAC:
		_output_enc.writeByte('F');
		break;
	default:
		break;
	}

	for (int i = 1; i < 4; i++)
		_output_enc.writeByte(vdb_hdr[i]);

	_output_enc.close();

	/* And some clean-up :-) */
	Common::removeFile(TEMP_RAW);
	Common::removeFile(TEMP_ENC);
}


#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressTonyVDB tony(argv[0]);
	return tony.run(argc, argv);
}
#endif
