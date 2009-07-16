/* compress_saga - Compress SAGA engine digital sound files into
 * MP3 and Ogg Vorbis format
 * Copyright (C) 2004, Marcoen Hirschberg
 * Copyright (C) 2004-2006  The ScummVM Team
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

#include "compress.h"
#include "compress_saga.h"
#include "utils/md5.h"
#include "utils/util.h"
#include "utils/audiostream.h"
#include "utils/file.h"
#include "utils/voc.h"
#include "utils/wave.h"
#include "utils/adpcm.h"

#define FILE_MD5_BYTES 5000
#define RSC_TABLEINFO_SIZE 8
#define RSC_TABLEENTRY_SIZE 8
#define HEADER_SIZE 9

// Known ITE files
static CompressSaga::GameFileDescription ITE_GameFiles[] = {
	//	Filename					swapEndian	md5									resourceType	frequency	stereo
	{"sounds.rsc",					false,		"e2ccb61c325d6d1ead3be0e731fe29fe", kSoundPCM,		22050,		false},	// PC CD/disk
	{"sounds.rsc",					true,		"95863b89a0916941f6c5e1789843ba14", kSoundPCM,		22050,		false},	// Mac
	{"soundsd.rsc",					false,		"95a6c148e22e99a8c243f2978223583c", kSoundPCM,		22050,		false},	// New PC demos
	{"soundsd.rsc",					true,		"b3a831fbed337d1f1300fee1dd474f6c", kSoundPCM,		22050,		false},	// Mac demos
	// Unsupported (8 bit unsigned sound) - used in the early ITE Win32 demo
	//{"soundsd.rsc",				false,		"a741139dd7365a13f463cd896ff9969a", kSoundPCM,		22050,		false},	// Old Win32 demo
	{"ite sounds.bin",				true,		"441426c6bb2a517f65c7e49b57f7a345", kSoundMacPCM,	22050,		false}, // MacBinary

	{"music.rsc",					false,		"d6454756517f042f01210458abe8edd4", kSoundPCM,		11025,		true},	// PC CD/disk with digital music
	{"music.rsc",					true,		"1a91cd60169f367ecb6c6e058d899b2f", kSoundPCM,		11025,		true},	// Mac
	{"musicd.rsc",					true,		"d6454756517f042f01210458abe8edd4", kSoundPCM,		11025,		true},	// New PC demos
	{"musicd.rsc",					true,		"495bdde51fd9f4bea2b9c911091b1ab2", kSoundPCM,		11025,		false},	// New Mac demo
	{"musicd.rsc",					true,		"1a91cd60169f367ecb6c6e058d899b2f", kSoundPCM,		11025,		true},	// Old Mac demo
	{"ite music.bin",				true,		"441426c6bb2a517f65c7e49b57f7a345", kSoundMacPCM,	22050,		false}, // MacBinary

	{"inherit the earth voices",	true,		"c14c4c995e7a0d3828e3812a494301b7", kSoundPCM,		22050,		false},	// Mac
	{"voices.rsc",					false,		"41bb6b95d792dde5196bdb78740895a6", kSoundPCM,		22050,		false},	// CD
	{"voices.rsc",					false,		"2fbad5d10b9b60a3415dc4aebbb11718", kSoundPCM,		22050,		false},	// German CD
	{"voices.rsc",					false,		"c46e4392fcd2e89bc91e5567db33b62d", kSoundVOC,		-1,			false},	// Disk
	{"voices.rsc",					false,		"0c9113e630f97ef0996b8c3114badb08", kSoundVOC,		-1,			false},	// German disk
	{"voices.rsc",					false,		"c58e67c506af4ffa03fd0aac2079deb0", kSoundVOC,		-1,			false},	// Early DOS demo
	{"voicesd.rsc",					false,		"e139d86bab2ee8ba3157337f894a92d4", kSoundVOX,		22050,		false},	// New PC demos and all Mac demos
	// Unsupported (8 bit unsigned sound) - used in the early ITE Win32 demo
	//{"voicesd.rsc",				false,		"0759eaf5b64ae19fd429920a70151ad3", kSoundPCM,		22050,			false},	// Old Win32 demo
	{"ite voices.bin",				true,		"dba92ae7d57e942250fe135609708369", kSoundMacPCM,	22050,		false}	// MacBinary
	// TODO: Add known Amiga files
};

// Known IHNM files
static CompressSaga::GameFileDescription IHNM_GameFiles[] = {
	//	Filename					swapEndian	md5									resourceType	frequency	stereo
	// FIXME: sfx.res is disabled for now, as there are issues when trying to encode it
	//{"sfx.res",					false,		"1c610d543f32ec8b525e3f652536f269", kSoundWAV,		-1,			false},
	{"voicess.res",					false,		"-1", 								kSoundWAV,		-1,			false},
	{"voices1.res",					false,		"-1", 								kSoundWAV,		-1,			false},
	{"voices2.res",					false,		"-1", 								kSoundWAV,		-1,			false},
	{"voices3.res",					false,		"-1", 								kSoundWAV,		-1,			false},
	{"voices4.res",					false,		"-1", 								kSoundWAV,		-1,			false},
	{"voices5.res",					false,		"-1", 								kSoundWAV,		-1,			false},
	{"voices6.res",					false,		"-1", 								kSoundWAV,		-1,			false},
	// Demo
	{"voicesd.res",					false,		"-1",								kSoundWAV,		-1,			false},
};

// --------------------------------------------------------------------------------

static CompressSaga::GameDescription gameDescriptions[] = {
	// Inherit the earth
	{
		GType_ITE,
		ARRAYSIZE(ITE_GameFiles),
		ITE_GameFiles,
	},

	// I Have No Mouth And I Must Scream
	{
		GType_IHNM,
		ARRAYSIZE(IHNM_GameFiles),
		IHNM_GameFiles,
	},

};

typedef struct  {
	uint32 offset;
	uint32 size;
} Record;

// Constructor
CompressSaga::CompressSaga(const std::string &name) : CompressionTool(name) {
	_currentGameDescription = NULL;
	_currentFileDescription = NULL;

	_helptext = "\nUsage: %s [mode] [mode params] [-o outputfile = infile.cmp] <inputfile>\n" kCompressionAudioHelp;
}

bool CompressSaga::inspectInput(const Filename &filename) {
	return filename.hasExtension("rsc") || filename.hasExtension("res") || filename.hasExtension("bin") || filename.getFullName() == "inherit the earth voices";
}

// --------------------------------------------------------------------------------

bool CompressSaga::detectFile(Filename *infile) {
	int gamesCount = ARRAYSIZE(gameDescriptions);
	int i, j;
	uint8 md5sum[16];
	char md5str[32+1];

	Common::md5_file(infile->getFullPath().c_str(), md5sum, FILE_MD5_BYTES);
	print("Input file name: %s\n", infile->getFullPath().c_str());
	for (j = 0; j < 16; j++) {
		sprintf(md5str + j*2, "%02x", (int)md5sum[j]);
	}
	print("md5: %s\n", md5str);

	for (i = 0; i < gamesCount; i++) {
		for (j = 0; j < gameDescriptions[i].filesCount; j++) {
			if (i == 0) {		// ITE
				// MD5 based detection, needed to distinguish the different file encodings
				// of the ITE sound files
				if (strcmp(gameDescriptions[i].filesDescriptions[j].md5, md5str) == 0) {
					_currentGameDescription = &gameDescriptions[i];
					_currentFileDescription = &_currentGameDescription->filesDescriptions[j];

					print("Matched game: Inherit the Earth: Quest for the Orb\n");
					return true;
				}
			} else {			// IHNM
				// Filename based detection, used in IHNM, as all its sound files have the
				// same encoding

				if (scumm_stricmp(gameDescriptions[i].filesDescriptions[j].fileName, infile->getFullName().c_str()) == 0) {
					_currentGameDescription = &gameDescriptions[i];
					_currentFileDescription = &_currentGameDescription->filesDescriptions[j];

					print("Matched game: I have no mouth, and I must scream\n");
					return true;
				}
			}
		}
	}
	print("Unsupported file\n");
	return false;
}

uint32 CompressSaga::copyFile(const char *fromFileName, File &outputFile) {
	uint32 size;
	char fbuf[2048];
	File tempf(fromFileName, "rb");

	if (!tempf.isOpen())
		error("Unable to open %s", fromFileName);

	while ((size = (uint32)tempf.read(fbuf, 1, sizeof(fbuf))) > 0) {
		outputFile.write(fbuf, 1, size);
	}
	size = tempf.pos();
	return size;
}

void CompressSaga::copyFile(File &inputFile, uint32 inputSize, const char *toFileName) {
	uint32 size;
	char fbuf[2048];
	File tempf(toFileName, "wb");

	if (!tempf.isOpen())
		error("Unable to open %s", toFileName);
	while (inputSize > 0) {
		size = (uint32)inputFile.read(fbuf, 1, inputSize > sizeof(fbuf) ? sizeof(fbuf) : inputSize);
		if (size == 0) {
			error("Unable to copy file");
		}
		tempf.write(fbuf, 1, size);
		inputSize -= size;
	}
}

void CompressSaga::writeBufferToFile(uint8 *data, uint32 inputSize, const char *toFileName) {
	File tempf(toFileName, "wb");
	if (!tempf.isOpen())
		error("Unable to open %s", toFileName);
	tempf.write(data, 1, inputSize);
}

void CompressSaga::writeHeader(File &outputFile) {
	outputFile.writeByte(compression_format(_format));
	outputFile.writeUint16LE(_sampleRate);
	outputFile.writeUint32LE(_sampleSize);
	outputFile.writeByte(_sampleBits);
	outputFile.writeByte(_sampleStereo);
}

uint32 CompressSaga::encodeEntry(File &inputFile, uint32 inputSize, File &outputFile) {
	uint8 *inputData = 0;
	byte *buffer = 0;
	Common::File inputFileStream(inputFile);
	int rate, size;
	byte flags;

	if (_currentFileDescription->resourceType == kSoundVOC) {
		inputData = Audio::loadVOCFromStream(inputFileStream, size, rate);

		_sampleSize = size;
		_sampleRate = rate;
		_sampleBits = 8;
		_sampleStereo = 0;
		writeHeader(outputFile);

		writeBufferToFile(inputData, _sampleSize, TEMP_RAW);
		free(inputData);

		setRawAudioType( true, false, 8);
		encodeAudio(TEMP_RAW, true, _sampleRate, tempEncoded, _format);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (_currentFileDescription->resourceType == kSoundPCM) {
		_sampleSize = inputSize;
		_sampleRate = (uint16)_currentFileDescription->frequency;
		_sampleBits = 16;
		_sampleStereo = _currentFileDescription->stereo;
		writeHeader(outputFile);

		copyFile(inputFile, inputSize, TEMP_RAW);

		setRawAudioType( !_currentFileDescription->swapEndian, _sampleStereo != 0, _sampleBits);
		encodeAudio(TEMP_RAW, true, _sampleRate, tempEncoded, _format);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (_currentFileDescription->resourceType == kSoundWAV) {
		if (!Audio::loadWAVFromStream(inputFileStream, size, rate, flags))
			error("Unable to read WAV");

		_sampleSize = size;
		_sampleRate = rate;
		_sampleBits = ((flags & Audio::Mixer::FLAG_16BITS) != 0) ? 16 : 8;
		_sampleStereo = ((flags & Audio::Mixer::FLAG_STEREO) != 0);
		writeHeader(outputFile);

		copyFile(inputFile, size, TEMP_RAW);

		setRawAudioType( true, _sampleStereo != 0, _sampleBits);
		encodeAudio(TEMP_RAW, true, _sampleRate, tempEncoded, _format);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (_currentFileDescription->resourceType == kSoundVOX) {
		_sampleSize = inputSize * 4;
		_sampleRate = (uint16)_currentFileDescription->frequency;
		_sampleBits = 16;
		_sampleStereo = _currentFileDescription->stereo;
		writeHeader(outputFile);

		Audio::AudioStream *voxStream = Audio::makeADPCMStream(&inputFileStream, inputSize, Audio::kADPCMOki);
		buffer = (byte *)malloc(_sampleSize);
		uint32 voxSize = voxStream->readBuffer((int16*)buffer, inputSize * 2);
		if (voxSize != inputSize * 2)
			error("Wrong VOX output size");
		writeBufferToFile((uint8 *)buffer, _sampleSize, TEMP_RAW);
		free(buffer);

		setRawAudioType( !_currentFileDescription->swapEndian, _sampleStereo != 0, _sampleBits);
		encodeAudio(TEMP_RAW, true, _sampleRate, tempEncoded, _format);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (_currentFileDescription->resourceType == kSoundMacPCM) {
		error("MacBinary files are not supported yet");
		// TODO
		// Note: MacBinary files are unsigned. With the pending changes to setRawAudioType, there will need
		// to be some changes here
		/*
		copyFile(inputFile, inputSize, TEMP_RAW);
		_sampleSize = inputSize - 36;
		_sampleRate = (uint16)currentFileDescription->frequency;
		// The MAC CD Guild version has 8 bit sound, whereas the other versions have 16 bit sound
		_sampleBits = 8;
		_sampleStereo = currentFileDescription->stereo;
		writeHeader(outputFile);

		setRawAudioType( !currentFileDescription->swapEndian, currentFileDescription->stereo, _sampleBits);
		encodeAudio(TEMP_RAW, true, currentFileDescription->frequency, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
		*/
	}

	error("Unsupported resourceType %ul\n", _currentFileDescription->resourceType);
	// Never reached
	return 0;
}

void CompressSaga::sagaEncode(Filename *inpath, Filename *outpath) {
	File inputFile;
	File outputFile;
	uint32 inputFileSize;
	uint32 resTableOffset;
	uint32 resTableCount;
	uint32 i;

	Record *inputTable;
	Record *outputTable;

	inputFile.open(*inpath, "rb");
	inputFileSize = inputFile.size();
	print("Filesize: %ul\n", inputFileSize);
	/*
	 * At the end of the resource file there are 2 values: one points to the
	 * beginning of the resource table the other gives the number of
	 * records in the table
	 */
	inputFile.seek(inputFileSize - RSC_TABLEINFO_SIZE, SEEK_SET);

	if (!_currentFileDescription->swapEndian) {
		resTableOffset = inputFile.readUint32LE();
		resTableCount = inputFile.readUint32LE();
	} else {
		resTableOffset = inputFile.readUint32BE();
		resTableCount = inputFile.readUint32BE();
	}

	print("Table offset: %ul\nnumber of records: %ul\n", resTableOffset, resTableCount);
	if (resTableOffset != inputFileSize - RSC_TABLEINFO_SIZE - RSC_TABLEENTRY_SIZE * resTableCount) {
		error("Something's wrong with your resource file");
	}

	// Go to beginning of the table
	inputFile.seek(resTableOffset, SEEK_SET);

	inputTable = (Record*)malloc(resTableCount * sizeof(Record));

	// Put offsets of all the records in a table
	for (i = 0; i < resTableCount; i++) {

		if (!_currentFileDescription->swapEndian) {
			inputTable[i].offset = inputFile.readUint32LE();
			inputTable[i].size = inputFile.readUint32LE();
		} else {
			inputTable[i].offset = inputFile.readUint32BE();
			inputTable[i].size = inputFile.readUint32BE();
		}

		print("Record: %ul, offset: %ul, size: %ul\n", i, inputTable[i].offset, inputTable[i].size);

		if ((inputTable[i].offset > inputFileSize) ||
			(inputTable[i].offset + inputTable[i].size > inputFileSize)) {
			error("The offset points outside the file");
		}

	}
	outputTable = (Record*)malloc(resTableCount * sizeof(Record));

	if (outpath->empty()) {
		*outpath = *inpath;
		outpath->setExtension(".cmp");
	}
	outputFile.open(*outpath, "wb");

	for (i = 0; i < resTableCount; i++) {
		inputFile.seek(inputTable[i].offset, SEEK_SET);
		outputTable[i].offset = outputFile.pos();

		if (inputTable[i].size >= 8) {
			outputTable[i].size = encodeEntry(inputFile, inputTable[i].size, outputFile);
		} else {
			outputTable[i].size = inputTable[i].size;	// Empty sound resource
		}
	}
	inputFile.close();

	resTableOffset = outputFile.pos();
	for (i = 0; i < resTableCount; i++) {
		outputFile.writeUint32LE(outputTable[i].offset);
		outputFile.writeUint32LE(outputTable[i].size);
	}
	outputFile.writeUint32LE(resTableOffset);
	outputFile.writeUint32LE(resTableCount);	// Should be the same number of entries

	outputFile.close();

	free(inputTable);
	free(outputTable);

	// Cleanup
	unlink(TEMP_RAW);
	unlink(tempEncoded);

	print("Done!\n");
}

void CompressSaga::execute() {
	// Check input
	if (_inputPaths.size() != 1)
		error("One input file expected!");
	Filename inpath(_inputPaths[0]);
	Filename &outpath = _outputPath;

	// ITE
	inpath.setExtension(".rsc");
	if (detectFile(&inpath)) {
		sagaEncode(&inpath, &outpath);
	} else {
		// IHNM
		inpath.setExtension(".res");
		if (detectFile(&inpath)) {
			sagaEncode(&inpath, &outpath);
		} else {
			// Check for "inherit the earth voices"
			inpath.setFullName("inherit the earth voices");
			if (detectFile(&inpath)) {
				sagaEncode(&inpath, &outpath);
			} else {
				// Check for MacBinary
				inpath.setExtension(".bin");
				if (detectFile(&inpath)) {
					sagaEncode(&inpath, &outpath);
				} else {
					error("Failed to compress file %s", inpath.getFullPath().c_str());
				}
			}
		}
	}
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressSaga saga(argv[0]);
	return saga.run(argc, argv);
}
#endif

