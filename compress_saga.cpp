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

enum GameSoundTypes {
	kSoundPCM = 0,
	kSoundVOX = 1,
	kSoundVOC = 2,
	kSoundWAV = 3,
	kSoundMacPCM = 4
};

struct GameFileDescription {
	const char *fileName;
	bool swapEndian;
	const char *md5;
	GameSoundTypes resourceType;
	long frequency;
	bool stereo;
};

// Known ITE files
static GameFileDescription ITE_GameFiles[] = {
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
static GameFileDescription IHNM_GameFiles[] = {
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

enum SAGAGameType {
	GType_ITE = 0,
	GType_IHNM = 1
};

struct GameDescription {
	SAGAGameType gameType;
	int filesCount;
	GameFileDescription *filesDescriptions;
};

static GameDescription gameDescriptions[] = {
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

static CompressMode gCompMode = kMP3Mode;

GameDescription *currentGameDescription = NULL;
GameFileDescription *currentFileDescription = NULL;

uint16 sampleRate;
uint32 sampleSize;
uint8 sampleBits;
uint8 sampleStereo;

// --------------------------------------------------------------------------------

bool detectFile(const char *inFileName) {
	int gamesCount = ARRAYSIZE(gameDescriptions);
	int i, j, k;
	uint8 md5sum[16];
	char md5str[32+1];
	char currentFile[256];

	Common::md5_file(inFileName, md5sum, FILE_MD5_BYTES);
	printf("Input file name: %s\n", inFileName);
	for (j = 0; j < 16; j++) {
		sprintf(md5str + j*2, "%02x", (int)md5sum[j]);
	}
	printf("md5: %s\n", md5str);

	for (i = 0; i < gamesCount; i++) {
		for (j = 0; j < gameDescriptions[i].filesCount; j++) {
			if (i == 0) {		// ITE
				// MD5 based detection, needed to distinguish the different file encodings
				// of the ITE sound files
				if (strcmp(gameDescriptions[i].filesDescriptions[j].md5, md5str) == 0) {
					currentGameDescription = &gameDescriptions[i];
					currentFileDescription = &currentGameDescription->filesDescriptions[j];

					printf("Matched game: Inherit the Earth: Quest for the Orb\n");
					return true;
				}
			} else {			// IHNM			
				// Filename based detection, used in IHNM, as all its sound files have the
				// same encoding

				getFilename(inFileName, currentFile);
				for (k = 0; k < strlen(currentFile); k++)
					currentFile[k] = tolower(currentFile[k]);

				if (strcmp(gameDescriptions[i].filesDescriptions[j].fileName, currentFile) == 0) {
					currentGameDescription = &gameDescriptions[i];
					currentFileDescription = &currentGameDescription->filesDescriptions[j];

					printf("Matched game: I have no mouth, and I must scream\n");
					return true;
				}
			}
		}
	}
	printf("Unsupported file\n");
	return false;
}

uint32 copyFile(const char *fromFileName, FILE* outputFile) {
	uint32 size;
	char fbuf[2048];
	FILE * tempf;

	tempf = fopen(fromFileName, "rb");
	if (tempf == NULL)
		error("Unable to open %s", fromFileName);

	while ((size = (uint32)fread(fbuf, 1, sizeof(fbuf), tempf)) > 0) {
		fwrite(fbuf, 1, size, outputFile);
	}
	size = ftell(tempf);
	fclose(tempf);
	return size;
}

void copyFile(FILE* inputFile, uint32 inputSize, const char* toFileName) {
	uint32 size;
	char fbuf[2048];
	FILE * tempf;

	tempf = fopen(toFileName, "wb");
	if (tempf == NULL)
		error("Unable to open %s", toFileName);
	while (inputSize > 0) {
		size = (uint32)fread(fbuf, 1, inputSize > sizeof(fbuf) ? sizeof(fbuf) : inputSize, inputFile);
		if (size == 0) {
			error("Unable to copy file");
		}
		fwrite(fbuf, 1, size, tempf);
		inputSize -= size;
	}
	fclose(tempf);
}

void writeBufferToFile(uint8* data, uint32 inputSize, const char* toFileName) {
	FILE * tempf;

	tempf = fopen(toFileName, "wb");
	if (tempf == NULL)
		error("Unable to open %s", toFileName);
	fwrite(data, 1, inputSize, tempf);
	fclose(tempf);
}

void writeHeader(FILE* outputFile) {
	writeByte(outputFile, gCompMode);
	writeUint16LE(outputFile, sampleRate);
	writeUint32LE(outputFile, sampleSize);
	writeByte(outputFile, sampleBits);
	writeByte(outputFile, sampleStereo);
}

uint32 encodeEntry(FILE* inputFile, uint32 inputSize, FILE* outputFile) {
	uint8 *inputData = 0;
	byte *buffer = 0;
	Common::File inputFileStream(inputFile);
	int rate, size;
	byte flags;

	if (currentFileDescription->resourceType == kSoundVOC) {
		inputData = Audio::loadVOCFromStream(inputFileStream, size, rate);

		sampleSize = size;
		sampleRate = rate;
		sampleBits = 8;
		sampleStereo = 0;
		writeHeader(outputFile);

		writeBufferToFile(inputData, sampleSize, TEMP_RAW);
		free(inputData);

		setRawAudioType( true, false, 8);
		encodeAudio(TEMP_RAW, true, sampleRate, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (currentFileDescription->resourceType == kSoundPCM) {
		sampleSize = inputSize;
		sampleRate = (uint16)currentFileDescription->frequency;
		sampleBits = 16;
		sampleStereo = currentFileDescription->stereo;
		writeHeader(outputFile);

		copyFile(inputFile, inputSize, TEMP_RAW);

		setRawAudioType( !currentFileDescription->swapEndian, sampleStereo != 0, sampleBits);
		encodeAudio(TEMP_RAW, true, sampleRate, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (currentFileDescription->resourceType == kSoundWAV) {
		if (!Audio::loadWAVFromStream(inputFileStream, size, rate, flags))
			error("Unable to read WAV");

		sampleSize = size;
		sampleRate = rate;
		sampleBits = ((flags & Audio::Mixer::FLAG_16BITS) != 0) ? 16 : 8;
		sampleStereo = ((flags & Audio::Mixer::FLAG_STEREO) != 0);
		writeHeader(outputFile);

		copyFile(inputFile, size, TEMP_RAW);

		setRawAudioType( true, sampleStereo != 0, sampleBits);
		encodeAudio(TEMP_RAW, true, sampleRate, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (currentFileDescription->resourceType == kSoundVOX) {
		sampleSize = inputSize * 4;
		sampleRate = (uint16)currentFileDescription->frequency;
		sampleBits = 16;
		sampleStereo = currentFileDescription->stereo;
		writeHeader(outputFile);

		Audio::AudioStream *voxStream = Audio::makeADPCMStream(&inputFileStream, inputSize, Audio::kADPCMOki);
		buffer = (byte *)malloc(sampleSize);
        uint32 voxSize = voxStream->readBuffer((int16*)buffer, inputSize * 2);
        if (voxSize != inputSize * 2)
			error("Wrong VOX output size");
		for (uint32 i = 0; i < sampleSize; i++)
			buffer[i] = TO_LE_16(buffer[i]);
		writeBufferToFile((uint8 *)buffer, sampleSize, TEMP_RAW);
		free(buffer);

		setRawAudioType( !currentFileDescription->swapEndian, sampleStereo != 0, sampleBits);
		encodeAudio(TEMP_RAW, true, sampleRate, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (currentFileDescription->resourceType == kSoundMacPCM) {
		error("MacBinary files are not supported yet");
		// TODO
		// Note: MacBinary files are unsigned. With the pending changes to setRawAudioType, there will need
		// to be some changes here
		/*
		copyFile(inputFile, inputSize, TEMP_RAW);
		sampleSize = inputSize - 36;
		sampleRate = (uint16)currentFileDescription->frequency;
		// The MAC CD Guild version has 8 bit sound, whereas the other versions have 16 bit sound
		sampleBits = 8;
		sampleStereo = currentFileDescription->stereo;
		writeHeader(outputFile);

		setRawAudioType( !currentFileDescription->swapEndian, currentFileDescription->stereo, sampleBits);
		encodeAudio(TEMP_RAW, true, currentFileDescription->frequency, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
		*/
	}

	error("Unsupported resourceType %ul\n", currentFileDescription->resourceType);
}

void sagaEncode(const char *inputPath, const char *inputFileName, const char *inputFileNameExt) {
	char inputFileNameWithExt[256];
	char outputFileNameWithExt[256];
	FILE *inputFile;
	FILE *outputFile;
	uint32 inputFileSize;
	uint32 resTableOffset;
	uint32 resTableCount;
	uint32 i;

	Record *inputTable;
	Record *outputTable;

	sprintf(inputFileNameWithExt, "%s/%s%s", inputPath, inputFileName, inputFileNameExt);
	inputFile = fopen(inputFileNameWithExt, "rb");
	inputFileSize = fileSize(inputFile);
	printf("Filesize: %ul\n", inputFileSize);
	/*
	 * At the end of the resource file there are 2 values: one points to the
	 * beginning of the resource table the other gives the number of
	 * records in the table
	 */
	fseek(inputFile, inputFileSize - RSC_TABLEINFO_SIZE, SEEK_SET);

	if (!currentFileDescription->swapEndian) {
		resTableOffset = readUint32LE(inputFile);
		resTableCount = readUint32LE(inputFile);
	} else {
		resTableOffset = readUint32BE(inputFile);
		resTableCount = readUint32BE(inputFile);
	}

	printf("Table offset: %ul\nnumber of records: %ul\n", resTableOffset, resTableCount);
	if (resTableOffset != inputFileSize - RSC_TABLEINFO_SIZE - RSC_TABLEENTRY_SIZE * resTableCount) {
		error("Something's wrong with your resource file");
	}

	// Go to beginning of the table
	fseek(inputFile, resTableOffset, SEEK_SET);

	inputTable = (Record*)malloc(resTableCount * sizeof(Record));

	// Put offsets of all the records in a table
	for (i = 0; i < resTableCount; i++) {

		if (!currentFileDescription->swapEndian) {
			inputTable[i].offset = readUint32LE(inputFile);
			inputTable[i].size = readUint32LE(inputFile);
		} else {
			inputTable[i].offset = readUint32BE(inputFile);
			inputTable[i].size = readUint32BE(inputFile);
		}

		printf("Record: %ul, offset: %ul, size: %ul\n", i, inputTable[i].offset, inputTable[i].size);

		if ((inputTable[i].offset > inputFileSize) ||
		    (inputTable[i].offset + inputTable[i].size > inputFileSize)) {
			error("The offset points outside the file");
		}

	}
	outputTable = (Record*)malloc(resTableCount * sizeof(Record));

	sprintf(outputFileNameWithExt, "%s/%s.cmp", inputPath, inputFileName);
	outputFile = fopen(outputFileNameWithExt, "wb");

	for (i = 0; i < resTableCount; i++) {
		fseek(inputFile, inputTable[i].offset, SEEK_SET);
		outputTable[i].offset = ftell(outputFile);

		if (inputTable[i].size >= 8) {
			outputTable[i].size = encodeEntry(inputFile, inputTable[i].size, outputFile);
		} else {
			outputTable[i].size = inputTable[i].size;	// Empty sound resource
		}
	}
	fclose(inputFile);

	resTableOffset = ftell(outputFile);
	for (i = 0; i < resTableCount; i++) {
		writeUint32LE(outputFile, outputTable[i].offset);
		writeUint32LE(outputFile, outputTable[i].size);
	}
	writeUint32LE(outputFile, resTableOffset);
	writeUint32LE(outputFile, resTableCount);	// Should be the same number of entries

	fclose(outputFile);

	free(inputTable);
	free(outputTable);

	// Cleanup
	unlink(TEMP_RAW);
	unlink(tempEncoded);

	printf("Done!\n");
}

void showhelp(char *exename) {
	printf("\nUsage: %s [params] <file>\n", exename);

	printf("\nParams:\n");

	printf("--mp3        encode to MP3 format (default)\n");
	printf("--vorbis     encode to Vorbis format\n");
	printf("--flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

	printf("\nMP3 mode params:\n");
	printf("-b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%d)\n", minBitrDef);
	printf("-B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf("--vbr        LAME uses the VBR mode (default)\n");
	printf("--abr        LAME uses the ABR mode\n");
	printf("-V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf("-q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf("--silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf("-b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf("-m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf("-M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf("-q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%d)\n", oggqualDef);
	printf("--silent     the output of oggenc is hidden (default:disabled)\n");

	printf("\nFlac mode params:\n");
 	printf(" --fast       FLAC uses compresion level 0\n");
 	printf(" --best       FLAC uses compresion level 8\n");
 	printf(" -<value>     specifies the value (0 - 8) of compresion (8=best)(default:%d)\n", flacCompressDef);
 	printf(" -b <value>   specifies a blocksize of <value> samples (default:%d)\n", flacBlocksizeDef);
	printf(" --verify     files are encoded and then decoded to check accuracy\n");
 	printf(" --silent     the output of FLAC is hidden (default:disabled)\n");

	printf("\n--help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

int main(int argc, char *argv[]) {
	int	i;
	char inputPath[768];
	char inputFileName[256];
	char inputFileNameWithExt[256];

	if (argc < 2) {
		showhelp(argv[0]);
	}

	/* compression mode */
	gCompMode = kMP3Mode;
	i = 1;

	if (strcmp(argv[1], "--mp3") == 0) {
		gCompMode = kMP3Mode;
		i++;
	} else if (strcmp(argv[1], "--vorbis") == 0) {
		gCompMode = kVorbisMode;
		i++;
	} else if (strcmp(argv[1], "--flac") == 0) {
		gCompMode = kFlacMode;
		i++;
	}

	switch (gCompMode) {
	case kMP3Mode:
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc, argv, i)) {
			showhelp(argv[0]);
		}
		break;
	case kVorbisMode:
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc, argv, i)) {
			showhelp(argv[0]);
		}
		break;
	case kFlacMode:
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc, argv, i)) {
			showhelp(argv[0]);
		}
		break;
	}

	getPath(argv[argc - 1], inputPath);
	getFilename(argv[argc - 1], inputFileName);

	if (strrchr(inputFileName, '.') != NULL) {
		error("Please specify the filename without an extension");
	}

	// ITE
	sprintf(inputFileNameWithExt, "%s/%s.rsc", inputPath, inputFileName);
	if (detectFile(inputFileNameWithExt)) {
		sagaEncode(inputPath, inputFileName, ".rsc");
	} else {
		// IHNM
		sprintf(inputFileNameWithExt, "%s/%s.res", inputPath, inputFileName);
		if (detectFile(inputFileNameWithExt)) {
			sagaEncode(inputPath, inputFileName, ".res");
		} else {
			// Check for "inherit the earth voices"
			if (detectFile("inherit the earth voices")) {
				sagaEncode(inputPath, "inherit the earth voices", "");
			} else {
				// Check for MacBinary
				sprintf(inputFileNameWithExt, "%s/%s.bin", inputPath, inputFileName);
				if (detectFile(inputFileNameWithExt)) {
					sagaEncode(inputPath, inputFileName, ".bin");
				}
			}
		}
	}

	return (0);
}
