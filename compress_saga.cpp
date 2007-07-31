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

#define FILE_MD5_BYTES 5000
#define RSC_TABLEINFO_SIZE 8
#define RSC_TABLEENTRY_SIZE 8
#define HEADER_SIZE 9

enum GameSoundTypes {
	kSoundPCM = 0,
	kSoundVOC = 1,
	kSoundWAV = 2,
	kSoundMacPCM = 3
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
	{"ite sounds.bin",				true,		"441426c6bb2a517f65c7e49b57f7a345", kSoundMacPCM,	22050,		false}, // MacBinary
	{"music.rsc",					false,		"d6454756517f042f01210458abe8edd4", kSoundPCM,		11025,		true},	// PC CD/disk with digital music
	{"music.rsc",					true,		"1a91cd60169f367ecb6c6e058d899b2f", kSoundPCM,		11025,		true},	// Mac
	{"inherit the earth voices",	true,		"c14c4c995e7a0d3828e3812a494301b7", kSoundPCM,		22050,		false},	// Mac
	{"voices.rsc",					false,		"41bb6b95d792dde5196bdb78740895a6", kSoundPCM,		22050,		false},	// CD
	{"voices.rsc",					false,		"2fbad5d10b9b60a3415dc4aebbb11718", kSoundPCM,		22050,		false},	// German CD
	{"voices.rsc",					false,		"c46e4392fcd2e89bc91e5567db33b62d", kSoundVOC,		-1,			false},	// Disk
	{"voices.rsc",					false,		"0c9113e630f97ef0996b8c3114badb08", kSoundVOC,		-1,			false},	// German disk
	{"ite voices.bin",				true,		"dba92ae7d57e942250fe135609708369", kSoundMacPCM,	22050,		false}	// MacBinary
};

#if 0
// Disabled for now
// Known IHNM files
static GameFileDescription IHNM_GameFiles[] = {
	//	Filename					swapEndian	md5									resourceType	frequency	stereo
	// Common
	{"sfx.res",						false,		"1c610d543f32ec8b525e3f652536f269", kSoundWAV,		-1,			false},
	// English
	{"voicess.res",					false,		"54b1f2013a075338ceb0e258d97808bd", kSoundWAV,		-1,			false},
	{"voices1.res",					false,		"fc6440b38025f4b2cc3ff55c3da5c3eb", kSoundWAV,		-1,			false},
	{"voices2.res",					false,		"b37f10fd1696ade7d58704ccaaebceeb", kSoundWAV,		-1,			false},
	{"voices3.res",					false,		"3bbc16a8f741dbb511da506c660a0b54", kSoundWAV,		-1,			false},
	{"voices4.res",					false,		"ebfa160122d2247a676ca39920e5d481", kSoundWAV,		-1,			false},
	{"voices5.res",					false,		"1f501ce4b72392bdd1d9ec38f6eec6da", kSoundWAV,		-1,			false},
	{"voices6.res",					false,		"f580ed7568c7d6ef34e934ba20adf834", kSoundWAV,		-1,			false},
	// Spanish
	{"voicess.res",					false,		"d869de9883c8faea7f687217a9ec7057", kSoundWAV,		-1,			false},
	{"voices1.res",					false,		"dc6a34e3d1668730ea46815a92c7847f", kSoundWAV,		-1,			false},
	{"voices2.res",					false,		"dc6a5fa7a4cdc2ca5a6fd924e969986c", kSoundWAV,		-1,			false},
	{"voices3.res",					false,		"dc6a5fa7a4cdc2ca5a6fd924e969986c", kSoundWAV,		-1,			false},
	{"voices4.res",					false,		"0f87400b804232a58dd22e404420cc45", kSoundWAV,		-1,			false},
	{"voices5.res",					false,		"172668cfc5d8c305cb5b1a9b4d995fc0", kSoundWAV,		-1,			false},
	{"voices6.res",					false,		"96c9bda9a5f41d6bc232ed7bf6d371d9", kSoundWAV,		-1,			false},
	// Russian
	{"voicess.res",					false,		"9df7cd3b18ddaa16b5291b3432567036", kSoundWAV,		-1,			false},
	{"voices1.res",					false,		"d6100d2dc3b2b9f2e1ad247f613dce9b", kSoundWAV,		-1,			false},
	{"voices2.res",					false,		"84f6f48ecc2832841ea6417a9a379430", kSoundWAV,		-1,			false},
	{"voices3.res",					false,		"ebb9501283047f27a0f54e27b3c8ba1e", kSoundWAV,		-1,			false},
	{"voices4.res",					false,		"4c145da5fa6d1306162a7ca8ce5a4f2e", kSoundWAV,		-1,			false},
	{"voices5.res",					false,		"871a559644281917677eca4af1b05620", kSoundWAV,		-1,			false},
	{"voices6.res",					false,		"211be5c24f066d69a2f6cfa953acfba6", kSoundWAV,		-1,			false},
	// German
	{"voicess.res",					false,		"8b09a196a52627cacb4eab13bfe0b2c3", kSoundWAV,		-1,			false},
	{"voices1.res",					false,		"424971e1e2373187c3f5734fe36071a2", kSoundWAV,		-1,			false},
	{"voices2.res",					false,		"c270e0980782af43641a86e4a14e2a32", kSoundWAV,		-1,			false},
	{"voices3.res",					false,		"49e42befea883fd101ec3d0f5d0647b9", kSoundWAV,		-1,			false},
	{"voices5.res",					false,		"c477443c52a0aa56e686ebd8d051e4ab", kSoundWAV,		-1,			false},
	{"voices6.res",					false,		"2b9aea838f74b4eecfb29a8f205a2bd4", kSoundWAV,		-1,			false},
	// French
	{"voicess.res",					false,		"b8642e943bbebf89cef2f48b31cb4305", kSoundWAV,		-1,			false},
	{"voices1.res",					false,		"424971e1e2373187c3f5734fe36071a2", kSoundWAV,		-1,			false},
	{"voices2.res",					false,		"c2d93a35d2c2def9c3d6d242576c794b", kSoundWAV,		-1,			false},
	{"voices3.res",					false,		"49e42befea883fd101ec3d0f5d0647b9", kSoundWAV,		-1,			false},
	{"voices5.res",					false,		"f4c415de7c03de86b73f9a12b8bd632f", kSoundWAV,		-1,			false},
	{"voices6.res",					false,		"3fc5358a5d8eee43bdfab2740276572e", kSoundWAV,		-1,			false}
};
#endif

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

#if 0
	// Disabled for now
	// I Have No Mouth And I Must Scream
	{
		GType_IHNM,
		ARRAYSIZE(IHNM_GameFiles),
		IHNM_GameFiles,
	},
#endif

};

typedef struct  {
	uint32 offset;
	uint32 size;
} Record;

static CompressMode gCompMode = kMP3Mode;

GameDescription *currentGameDescription = NULL;
GameFileDescription *currentFileDescription = NULL;
bool isSigned = true;

uint16 sampleRate;
uint32 sampleSize;
uint8 sampleBits;
uint8 sampleStereo;

// --------------------------------------------------------------------------------

bool detectFile(const char *inFileName) {
	int gamesCount = ARRAYSIZE(gameDescriptions);
	int j,i;
	uint8 md5sum[16];
	char md5str[32+1];

	Common::md5_file(inFileName, md5sum, FILE_MD5_BYTES);
	printf("Input file name: %s\n", inFileName);
	for (j = 0; j < 16; j++) {
		sprintf(md5str + j*2, "%02x", (int)md5sum[j]);
	}
	printf("md5: %s\n", md5str);

	for (i = 0; i < gamesCount; i++) {
		for (j = 0; j < gameDescriptions[i].filesCount; j++) {
			if (strcmp(gameDescriptions[i].filesDescriptions[j].md5, md5str) == 0) {
				currentGameDescription = &gameDescriptions[i];
				currentFileDescription = &currentGameDescription->filesDescriptions[j];

				if (currentGameDescription->gameType == GType_ITE)
					printf("Matched game: Inherit the Earth: Quest for the Orb\n");
				else
					printf("Matched game: I have no mouth, and I must scream\n");
				return true;
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
	uint8 *inputData;
	Common::File inputFileStream(inputFile);
	int rate, size;
	byte flags;

	if (currentFileDescription->resourceType == kSoundVOC) {
		inputData = Audio::loadVOCFromStream(inputFileStream, size, rate);
		sampleSize = size;
		sampleRate = rate;
		sampleBits = 8;
		sampleStereo = 0;
		writeBufferToFile(inputData, sampleSize, TEMP_RAW);
		free(inputData);
		writeHeader(outputFile);

		setRawAudioType( true, false, 8);
		encodeAudio(TEMP_RAW, true, sampleRate, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (currentFileDescription->resourceType == kSoundPCM) {
		copyFile(inputFile, inputSize, TEMP_RAW);
		sampleSize = inputSize;
		sampleRate = (uint16)currentFileDescription->frequency;
		sampleBits = 16;
		sampleStereo = currentFileDescription->stereo;
		writeHeader(outputFile);

		setRawAudioType( !currentFileDescription->swapEndian, currentFileDescription->stereo, sampleBits);
		encodeAudio(TEMP_RAW, true, currentFileDescription->frequency, tempEncoded, gCompMode);
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
	if (currentFileDescription->resourceType == kSoundMacPCM) {
		error("MacBinary files are not supported yet");
		// TODO
		/*
		copyFile(inputFile, inputSize, TEMP_RAW);
		sampleSize = inputSize - 36;
		sampleRate = (uint16)currentFileDescription->frequency;
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

void sagaEncode(const char *inputPath, const char *inputFileName) {
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

	sprintf(inputFileNameWithExt, "%s/%s.rsc", inputPath, inputFileName);
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

		outputTable[i].size = encodeEntry(inputFile, inputTable[i].size, outputFile);
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
	char *p;
	char inputPath[768];
	char *inputFileName = NULL;
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

	/* Find the last occurence of '/' or '\'
	 * Everything before this point is the path
	 * Everything after this point is the filename
	 */
	p = strrchr(argv[argc - 1], '/');
	if (!p) {
		p = strrchr(argv[argc - 1], '\\');

		if (!p) {
			p = argv[argc - 1] - 1;
		}
	}

	/* The path is everything before p, unless the file is in the current directory,
	 * in which case the path is '.'
	 */
	if (p < argv[argc - 1]) {
		strcpy(inputPath, ".");
	} else {
		strncpy(inputPath, argv[argc - 1], p - argv[argc - 1]);
	}

	strcpy(inputFileName, p + 1);

	if (strrchr(inputFileName, '.') != NULL) {
		error("Please specifiy the filename without an extension");
	}

	// ITE
	sprintf(inputFileNameWithExt, "%s/%s.rsc", inputPath, inputFileName);

	if (detectFile(inputFileNameWithExt)) {
		sagaEncode(inputPath, inputFileName);
	} else {
		// IHNM
		sprintf(inputFileNameWithExt, "%s/%s.res", inputPath, inputFileName);
		if (detectFile(inputFileNameWithExt)) {
			sagaEncode(inputPath, inputFileName);
		} else {
			// Check for "inherit the earth voices"
			if (detectFile("inherit the earth voices")) {
				sagaEncode(inputPath, "inherit the earth voices");
			} else {
				// Check for MacBinary
				sprintf(inputFileNameWithExt, "%s/%s.bin", inputPath, inputFileName);
				if (detectFile(inputFileNameWithExt)) {
					isSigned = false;
					sagaEncode(inputPath, inputFileName);
				}
			}
		}
	}

	return (0);
}
