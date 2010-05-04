/* compress_sci - resource.aud/resource.sfx compressor
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

// by m_kiewitz

#include <stdlib.h>

#include "compress.h"
#include "common/endian.h"

#include "sound/audiostream.h"
#include "sound/wave.h"

#include "compress_sci.h"

// data-format of resource.aud/resource.sfx:
//  just multiple SOL-Audio/WAVE files appended to each other
//  SCI1.1 also have resource id and headersize PER resource entry (but in WAVE resources)

//  at least kq6 resource.aud contains several raw lipsync resources w/o a real sync resource before that. Identifying those
//   raw lipsync resources isn't possible, because there is no header at all.

//  The resource data is accessed directly by offset referenced inside a map resource. That's why we need to add an additional
//   offset to offset mapping table right at the start of the file. We also include the compression type as first 4 bytes

// This code CURRENTLY DOES NOT SUPPORT SCI2.1+ GAMES! In those games the current code will actually destroy the content of
//  the samples, because SCI32 used a different scheme for decoding. I don't know yet how to detect SCI32 games easily
//  without having resourcemanager.

#define TEMP_RAW "tempfile.raw"
#define TEMP_ENC "tempfile.enc"

CompressSci::CompressSci(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_supportsProgressBar = true;

	ToolInput input1;
	input1.format = "resource.*";
	_inputPaths.push_back(input1);

	_outputToDirectory = false;

	_shorthelp = "Used to compress Sierra resource.aud/resource.sfx files. (NOT SCI32 compatible!)";
	_helptext = "\nUsage: " + getName() + " [mode-params] [-o outputname] <inputname>\n";
}

// header is first 6 bytes read from file
SciResourceDataType CompressSci::detectData(byte *header, bool compressMode) {
	byte buffer[20];
	uint32 dataSize;
	memcpy(&buffer, header, 6);
	if ((memcmp(buffer + 1, "RIFF", 4) == 0) || (memcmp(buffer + 1, "\x8d\x0bSOL", 4) == 0)) {
		// Fixup for pharkas resource.sfx, several WAVE files contain a size thats not right (-1 byte)
		for (int i = 0; i < 5; i++)
			buffer[i] = buffer[i + 1];
		_input.read_throwsOnError(&buffer[5], 1);
		_inputOffset++;
		warning("WAVE resource position adjusted at %lx\n", _inputOffset);
	}
	if (memcmp(buffer, "RIFF", 4) == 0) {
		// WAVE files begin with
		// "RIFF" [size of following data:DWORD]
		_input.read_throwsOnError(&buffer[6], 2);
		dataSize = READ_LE_UINT32(buffer + 4);
		_inputEndOffset = _inputOffset + 8 + dataSize;
		return kSciResourceDataTypeWAVE;
	}
	if (memcmp(buffer, "\x8d\x0bSOL\x00", 6) == 0) {
		// SOL files begin with
		// 0x8D 0x0B/0x0C "SOL" 0x00 [samplerate:WORD] [flags:BYTE] [size of following data after header:DWORD]
		//  0x0C variant have an additional byte inbetween data and [size of]
		_input.read_throwsOnError(&buffer[6], 7);
		dataSize = READ_LE_UINT32(buffer + 9);
		_inputEndOffset = _inputOffset + 13 + dataSize;
		return kSciResourceDataTypeSOL;
	}
	if (memcmp(buffer, "\x8d\x0cSOL\x00", 6) == 0) {
		// see above
		_input.read_throwsOnError(&buffer[6], 8);
		dataSize = READ_LE_UINT32(buffer + 9);
		// HACK: LSL6 resource.aud has a SOL that specifies incorrect dataSize, we fix it here
		if ((_inputOffset == 0x619bf07) && (dataSize == 0x1dd78))
			dataSize--;
		if ((_inputOffset == 0x101DFBC5) && (dataSize == 0x1cfc1))
			dataSize--;
		_inputEndOffset = _inputOffset + 14 + dataSize;
		return kSciResourceDataTypeSOL;
	}
	// No audio data found, if we are at offset 0 of the file -> exit w/ error message
	if (!_inputOffset)
		error("Input file doesn't seem to be a valid sci audio resource file");

	uint32 searchForward = 0;
	bool noSignature = false;
	if (memcmp(buffer, "\x8e\x00", 2) == 0) {
		// sync resource, we expect SOL audio with a resourceid afterwards
		searchForward = 10000;
	}
	// We didn't find anything useful, though this may be a valid file after all - kq6 resource.aud contains "raw-data"
	//  lip-sync data w/o the actual sync data
	if (!searchForward) {
		searchForward = 2048;
		if (!compressMode)
			warning("possibly raw lipsync data found at offset %lx\n", _input.pos());
		noSignature = true;
	}

	int originalOffset = _input.pos();
	byte syncBuffer[10000];
	uint32 syncPos = 0;
	_input.read_throwsOnError(&syncBuffer, searchForward);
	while (true) {
		if (syncPos + 5 == searchForward) {
			if (noSignature)
				error("no SOL audio after possible raw lipsync data at %lx", originalOffset);
			else
				error("no SOL audio after sync resource at %lx", originalOffset);
		}
		if (syncBuffer[syncPos] == 0x8D) {
			if (memcmp(&syncBuffer[syncPos + 1], "\x0cSOL\x00", 5) == 0)
				break;
			if (memcmp(&syncBuffer[syncPos + 1], "\x0bSOL\x00", 5) == 0)
				break;
		}
		syncPos++;
	}
	_inputEndOffset = _inputOffset + 6 + syncPos;
	return kSciResourceTypeTypeSync;
}

static const uint16 tableDPCM16[128] = {
	0x0000, 0x0008, 0x0010, 0x0020, 0x0030, 0x0040, 0x0050, 0x0060, 0x0070, 0x0080,
	0x0090, 0x00A0, 0x00B0, 0x00C0, 0x00D0, 0x00E0, 0x00F0, 0x0100, 0x0110, 0x0120,
	0x0130, 0x0140, 0x0150, 0x0160, 0x0170, 0x0180, 0x0190, 0x01A0, 0x01B0, 0x01C0,
	0x01D0, 0x01E0, 0x01F0, 0x0200, 0x0208, 0x0210, 0x0218, 0x0220, 0x0228, 0x0230,
	0x0238, 0x0240, 0x0248, 0x0250, 0x0258, 0x0260, 0x0268, 0x0270, 0x0278, 0x0280,
	0x0288, 0x0290, 0x0298, 0x02A0, 0x02A8, 0x02B0, 0x02B8, 0x02C0, 0x02C8, 0x02D0,
	0x02D8, 0x02E0, 0x02E8, 0x02F0, 0x02F8, 0x0300, 0x0308, 0x0310, 0x0318, 0x0320,
	0x0328, 0x0330, 0x0338, 0x0340, 0x0348, 0x0350, 0x0358, 0x0360, 0x0368, 0x0370,
	0x0378, 0x0380, 0x0388, 0x0390, 0x0398, 0x03A0, 0x03A8, 0x03B0, 0x03B8, 0x03C0,
	0x03C8, 0x03D0, 0x03D8, 0x03E0, 0x03E8, 0x03F0, 0x03F8, 0x0400, 0x0440, 0x0480,
	0x04C0, 0x0500, 0x0540, 0x0580, 0x05C0, 0x0600, 0x0640, 0x0680, 0x06C0, 0x0700,
	0x0740, 0x0780, 0x07C0, 0x0800, 0x0900, 0x0A00, 0x0B00, 0x0C00, 0x0D00, 0x0E00,
	0x0F00, 0x1000, 0x1400, 0x1800, 0x1C00, 0x2000, 0x3000, 0x4000
};

static const byte tableDPCM8[8] = {0, 1, 2, 3, 6, 10, 15, 21};

template<typename T> inline T CLIP (T v, T amin, T amax) {
	if (v < amin)
		return amin;
	else if (v > amax)
		return amax;
	else
		return v;
}

static void deDPCM16(byte *soundBuf, byte *inputBuf, uint32 n) {
	int16 *out = (int16 *) soundBuf;

	int32 s = 0;
	for (uint32 i = 0; i < n; i++) {
		byte b = *inputBuf++;
		if (b & 0x80)
			s -= tableDPCM16[b & 0x7f];
		else
			s += tableDPCM16[b];

		s = CLIP<int32>(s, -32768, 32767);
		*out++ = (uint16)s;
	}
}

static void deDPCM8Nibble(byte *soundBuf, int32 &s, byte b) {
	if (b & 8) {
// TODO: Can't include it currently because i have yet no idea how to identifiy sci2.1+ easily
// #ifdef ENABLE_SCI32
// 		// SCI2.1 reverses the order of the table values here
// 		if (getSciVersion() >= SCI_VERSION_2_1)
// 			s -= tableDPCM8[b & 7];
// 		else
// #endif
			s -= tableDPCM8[7 - (b & 7)];
	} else
		s += tableDPCM8[b & 7];
	s = CLIP<int32>(s, 0, 255);
	*soundBuf = s;
}

static void deDPCM8(byte *soundBuf, byte *inputBuf, uint32 n) {
	int32 s = 0x80;

	for (uint i = 0; i < n; i++) {
		byte b = *inputBuf++;

		deDPCM8Nibble(soundBuf++, s, b >> 4);
		deDPCM8Nibble(soundBuf++, s, b & 0xf);
	}
}

// Will compress dataType at current offset in inputfile to outputfile using requested codec
void CompressSci::compressData(SciResourceDataType dataType) {
	int orgDataSize = _inputEndOffset - _inputOffset;
	int newDataSize = 0;
	byte *orgData = new byte[orgDataSize];
	byte *newData = 0;

	int sampleRate = 0;
	byte *sampleData = 0;
	int sampleDataSize = 0;
	bool sampleIsStereo = false;
	uint8 sampleBits = 8;
	byte sampleFlags = 0;

	if (!orgData)
		error("malloc error");

	switch (dataType) {
	case kSciResourceDataTypeWAVE:
		print("WAVE found\n");
		if (!Audio::loadWAVFromStream(_input, sampleDataSize, sampleRate, sampleFlags))
			error("Unable to read WAV at offset %lx", _inputOffset);

		sampleData = new byte[sampleDataSize];
		if (!sampleData)
			error("malloc error");
		_input.read_throwsOnError(sampleData, sampleDataSize);
		if (sampleFlags & Audio::Mixer::FLAG_16BITS)
			sampleBits = 16;
		if (sampleFlags & Audio::Mixer::FLAG_STEREO)
			sampleIsStereo = true;
		break;
	case kSciResourceDataTypeSOL: {
		_input.readByte();
		byte headerSize = _input.readByte();
		_input.readUint32LE(); // Skip over "SOL" 0x00
		sampleRate = _input.readUint16LE();
		sampleFlags = _input.readByte();
		sampleDataSize = _input.readUint32LE();
		if (headerSize == 0x0C)
			_input.readByte();
		// Now we read SOL datastream
		sampleData = new byte[sampleDataSize];
		if (!sampleData)
			error("malloc error");
		_input.read_throwsOnError(sampleData, sampleDataSize);

		bool dataUnsigned = false;
		if (sampleFlags & 0x04)
			sampleBits = 16;
		if (sampleFlags & 0x08)
			dataUnsigned = true;
		if (sampleFlags & 0x01) {
			// SOL datastream is compressed, we need to uncompress it
			byte *uncompressedData = new byte[sampleDataSize * 2];
			if (sampleBits == 16)
				deDPCM16(uncompressedData, sampleData, sampleDataSize);
			else
				deDPCM8(uncompressedData, sampleData, sampleDataSize);
			delete[] sampleData;
			sampleData = uncompressedData;
			sampleDataSize *= 2;
		}
		break;
	}
	case kSciResourceTypeTypeSync:
		print("SYNC found at %lx\n", _inputOffset);
		// Simply copy original data over
		_input.read_throwsOnError(orgData, orgDataSize);
		newData = orgData;
		newDataSize = orgDataSize;
		break;
	default:
		error("Unsupported datatype");
	}

	if (sampleData) {
		// Write sample data to temporary raw file
		Common::File tempfileRaw(TEMP_RAW, "wb");
		tempfileRaw.write(sampleData, sampleDataSize);
		tempfileRaw.close();
		delete[] sampleData;
		// Now compress temporary raw file
		setRawAudioType(true, sampleIsStereo, sampleBits);
		encodeAudio(TEMP_RAW, true, sampleRate, TEMP_ENC, _format);
		// And copy encoded file into output-file
		Common::File tempfileEnc(TEMP_ENC, "rb");
		newDataSize = tempfileEnc.size();
		newData = new byte[newDataSize];
		tempfileEnc.read_throwsOnError(newData, newDataSize);
		tempfileEnc.close();
	}

	_output.write(newData, newDataSize);

	if ((newData) && (newData != orgData))
		delete[] newData;
	delete[] orgData;
}

void CompressSci::execute() {
	Common::Filename infile = _inputPaths[0].path;
	Common::Filename outfile = _outputPath;
	SciResourceDataType recognizedDataType;

	_input.open(infile, "rb");
	_inputSize = _input.size();

	// First find out how many samples are in this file
	//  We only support SOL audio and WAVE
	//  We can't support games that use "raw" audio (would have to walk through audio map for those, which would
	//   complicate this code - all talkie games that use this (kq5) don't use much space anyway, so not supporting
	//   those isn't a big issue

	_input.seek(0, SEEK_SET);
	byte header[6];

	_inputOffset = 0;
	_input.read_throwsOnError(&header, 6);
	if (memcmp(header, "MP3 ", 4) == 0)
		error("This resource file is already MP3-compressed, aborting...\n");
	if (memcmp(header, "OGG ", 4) == 0)
		error("This resource file is already OGG-compressed, aborting...\n");
	if (memcmp(header, "FLAC", 4) == 0)
		error("This resource file is already FLAC-compressed, aborting...\n");

	int resourceCount = 0;
	do {
		recognizedDataType = detectData(header, false);
		if (!recognizedDataType)
			error("Unsupported data at offset %lx", _inputOffset);
		_input.seek(_inputEndOffset, SEEK_SET);
		_inputOffset = _inputEndOffset;
		// We abort even, if file position is one below size because of pharkas resource.sfx
		if (_inputOffset >= _inputSize - 1)
			break;

		resourceCount++;
		_input.read_throwsOnError(&header, 6);
	} while (true);

	// This case happens on pharkas resource.sfx
	if (_inputOffset != _inputSize)
		warning("resource file has additional byte before end-of-file\n");

	print("Valid sci audio resource file. Found %d resources", resourceCount);

	// Now write basic output file header
	_output.open(outfile, "wb");
	// Compression ID
	switch (_format) {
	case AUDIO_MP3:		_output.writeUint32BE(MKID_BE('MP3 ')); break;
	case AUDIO_VORBIS:	_output.writeUint32BE(MKID_BE('OGG ')); break;
	case AUDIO_FLAC:	_output.writeUint32BE(MKID_BE('FLAC')); break;
	default:			throw ToolException("Unknown audio format!");
	}
	// Resource count
	_output.writeUint32LE(resourceCount);
	// Offset mapping table
	for (int resourceNo = 0; resourceNo < resourceCount; resourceNo++) {
		_output.writeUint32LE(0); // Original offset
		_output.writeUint32LE(0); // New offset
	}
	// Now actually compress the file
	_input.seek(0, SEEK_SET);
	for (int resourceNo = 0; resourceNo < resourceCount; resourceNo++) {
		_inputOffset = _input.pos();
		_outputOffset = _output.pos();
		_input.read_throwsOnError(&header, 6);
		recognizedDataType = detectData(header, true);

		assert(recognizedDataType);
		_input.seek(_inputOffset, SEEK_SET);
		compressData(recognizedDataType);

		// Seek inputfile to the end of the data
		_input.seek(_inputEndOffset, SEEK_SET);
		// Seek outputfile to mapping table
		_output.seek(8 + resourceNo * 8, SEEK_SET);
		// And write offset translations
		_output.writeUint32LE(_inputOffset);
		_output.writeUint32LE(_outputOffset);
		// Seek to end of file
		_output.seek(0, SEEK_END);

		updateProgress(resourceNo, resourceCount);
	}

	/* And some clean-up :-) */
	Common::removeFile(TEMP_RAW);
	Common::removeFile(TEMP_ENC);
}


#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressSci sci(argv[0]);
	return sci.run(argc, argv);
}
#endif
