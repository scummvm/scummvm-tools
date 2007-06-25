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

#include <stdio.h>
#include "compress.h"
#include "utils/md5.h"
#include "utils/util.h"
#include "utils/audiostream.h"
#include "utils/file.h"
#include "utils/voc.h"
#include "utils/wave.h"
#include "utils/adpcm.h"


#include "sagagame.h"
#include "sagaresnames.h"
#include "sagagame.cpp"

typedef struct  {
	uint32 offset;
	uint32 size;
} Record;

static CompressMode gCompMode = kMP3Mode;

GameDescription *currentGameDescription = NULL;
int currentFileIndex = -1;
bool isBigEndian = false;

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
				if ((gameDescriptions[i].filesDescriptions[j].fileType & (GAME_VOICEFILE | GAME_SOUNDFILE | GAME_MUSICFILE)) != 0)
				{
					currentGameDescription = &gameDescriptions[i];
					currentFileIndex = j;
					printf("Matched game: %s %s\n", currentGameDescription->name, currentGameDescription->extra);
					return true;
				}
			}
		}
	}
	printf("unsupported file\n");
	return false;
}

uint32 copyFile(const char *fromFileName, FILE* outputFile) {
	uint32 size;
	char fbuf[2048];
	FILE * tempf;
	
	tempf = fopen(fromFileName, "rb");
	if (tempf == NULL)
		error("unable to open %s\n", fromFileName);

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
		error("unable to open %s\n", toFileName);
	while (inputSize > 0) {
		size = (uint32)fread(fbuf, 1, inputSize > sizeof(fbuf) ? sizeof(fbuf) : inputSize, inputFile);
		if (size == 0) {
			error("unable to copy file");
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
		error("unable to open %s\n", toFileName);
	fwrite(data, 1, inputSize, tempf);
	fclose(tempf);
}

#define HEADER_SIZE 9

uint16 sampleRate;
uint32 sampleSize;
uint8 sampleBits;
uint8 sampleStereo;

void writeHeader(FILE* outputFile) {
	writeByte(outputFile, gCompMode);
	writeUint16LE(outputFile, sampleRate);
	writeUint32LE(outputFile, sampleSize);
	writeByte(outputFile, sampleBits);
	writeByte(outputFile, sampleStereo);
}
/*
case kSoundVOX:
        buffer.frequency = soundInfo->frequency;
        buffer.isSigned = soundInfo->isSigned;
        buffer.sampleBits = soundInfo->sampleBits;
        buffer.stereo = soundInfo->stereo;
        buffer.size = soundResourceLength * 4;
        if (onlyHeader) {
                buffer.buffer = NULL;
                free(soundResource);
        } else {
                voxStream = Audio::makeADPCMStream(&readS, soundResourceLength, Audio::kADPCMOki);
                buffer.buffer = (byte *)malloc(buffer.size);
                voxSize = voxStream->readBuffer((int16*)buffer.buffer, soundResourceLength * 2);
                if (voxSize != soundResourceLength * 2) {
                        error("SndRes::load() wrong VOX output size");
                }
                delete voxStream;
        }
        result = true;
        break;
*/
uint32 encodeEntry(GameSoundInfo *soundInfo, FILE* inputFile, uint32 inputSize, FILE* outputFile) {
	Audio::AudioStream *inStream;
	uint8 *inputData;
	Common::File inputFileStream(inputFile);
	int rate, size;
	byte flags;
	
	if (soundInfo->resourceType == kSoundVOX) {
		sampleSize = inputSize * 4;
		sampleRate = (uint16)soundInfo->frequency;
		sampleBits = soundInfo->sampleBits;
		sampleStereo = soundInfo->stereo;
		writeHeader(outputFile);

		inStream = Audio::makeADPCMStream(&inputFileStream, inputSize, Audio::kADPCMOki);
        inputData = (byte *)malloc(sampleSize);
		inStream->readBuffer((int16*)inputData, inputSize * 2);
		delete inStream;
		writeBufferToFile(inputData, sampleSize, TEMP_RAW);
		free(inputData);

		setRawAudioType( true, sampleStereo != 0, sampleBits);
		encodeAudio(TEMP_RAW, true, sampleRate, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (soundInfo->resourceType == kSoundVOC) {
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
	if (soundInfo->resourceType == kSoundPCM) {
		copyFile(inputFile, inputSize, TEMP_RAW);
		sampleSize = inputSize;
		sampleRate = (uint16)soundInfo->frequency;
		sampleBits = soundInfo->sampleBits;
		sampleStereo = soundInfo->stereo;
		writeHeader(outputFile);

		setRawAudioType( !soundInfo->isBigEndian, soundInfo->stereo, soundInfo->sampleBits);
		encodeAudio(TEMP_RAW, true, soundInfo->frequency, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile) + HEADER_SIZE;
	}
	if (soundInfo->resourceType == kSoundWAV) {
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

	error("sorry - unsupported resourceType %ul\n", soundInfo->resourceType);
}

#define RSC_TABLEINFO_SIZE 8
#define RSC_TABLEENTRY_SIZE 8

void sagaEncode(const char *inputFileName) {
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
	GameFileDescription *currentFileDescription;
	GameSoundInfo *soundInfo = NULL;

	currentFileDescription = &currentGameDescription->filesDescriptions[currentFileIndex];
	
	isBigEndian = ((currentGameDescription->features & GF_BIG_ENDIAN_DATA) != 0);

	if (currentFileDescription->fileType & GAME_SWAPENDIAN)
		isBigEndian = !isBigEndian;
	///isBigEndian = false;

	sprintf(inputFileNameWithExt, "%s.rsc", inputFileName);
	inputFile = fopen(inputFileNameWithExt, "rb");
	inputFileSize = fileSize(inputFile);
	printf("filesize: %ul\n", inputFileSize);
	/*
	 * At the end of the resource file there are 2 values: one points to the
	 * beginning of the resource table the other gives the number of
	 * records in the table
	 */
	fseek(inputFile, inputFileSize - RSC_TABLEINFO_SIZE, SEEK_SET);

	if (!isBigEndian) {
		resTableOffset = readUint32LE(inputFile);
		resTableCount = readUint32LE(inputFile);
	} else {
		resTableOffset = readUint32BE(inputFile);
		resTableCount = readUint32BE(inputFile);
	}

	printf("table offset: %ul\nnumber of records: %ul\n", resTableOffset, resTableCount);
	if (resTableOffset != inputFileSize - RSC_TABLEINFO_SIZE - RSC_TABLEENTRY_SIZE * resTableCount) {
		error("Something's wrong with your resource file..\n");
	}

	// Go to beginning of the table 
	fseek(inputFile, resTableOffset, SEEK_SET);

	inputTable = (Record*)malloc(resTableCount * sizeof(Record));

	// Put offsets of all the records in a table 
	for (i = 0; i < resTableCount; i++) {

	if (!isBigEndian) {
		inputTable[i].offset = readUint32LE(inputFile);
		inputTable[i].size = readUint32LE(inputFile);
	} else {
		inputTable[i].offset = readUint32BE(inputFile);
		inputTable[i].size = readUint32BE(inputFile);
	}

		 printf("record: %ul, offset: %ul, size: %ul\n", i, inputTable[i].offset, inputTable[i].size);
	
		if ((inputTable[i].offset > inputFileSize) ||
		    (inputTable[i].offset + inputTable[i].size > inputFileSize)) {
			error("The offset points outside the file!");
		}

	}
	outputTable = (Record*)malloc(resTableCount * sizeof(Record));

	sprintf(outputFileNameWithExt, "%s.cmp", inputFileName);	
	outputFile = fopen(outputFileNameWithExt, "wb");

	for (i = 0; i < resTableCount; i++) {
		fseek(inputFile, inputTable[i].offset, SEEK_SET);
		outputTable[i].offset = ftell(outputFile);
		
		if ((currentFileDescription->fileType & GAME_VOICEFILE) != 0) {
			soundInfo = currentGameDescription->voiceInfo;
		} else {
			if ((currentFileDescription->fileType & GAME_SOUNDFILE) != 0) {
				soundInfo = currentGameDescription->sfxInfo;
			} else {
				if ((currentFileDescription->fileType & GAME_MUSICFILE) != 0) {
					soundInfo = currentGameDescription->musicInfo;
				}
			}
		}

		outputTable[i].size = encodeEntry(soundInfo, inputFile, inputTable[i].size, outputFile);
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
	printf("\nUsage: %s <params> [<file> | mac]\n", exename);

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
	printf("[params]     optional Arguments passed to the Encoder\n");
	printf("             recommended is: --best -b 1152\n");

	printf("\n--help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

int main(int argc, char *argv[]) {
	int		i;
	char *inputFileName;
	char inputFileNameWithExt[256];

	if (argc < 2)
		showhelp(argv[0]);

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
		if (!process_mp3_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	}

	i = argc - 1;
	inputFileName = argv[i];

	sprintf(inputFileNameWithExt, "%s.rsc", inputFileName);
	if (detectFile(inputFileNameWithExt))
		sagaEncode(inputFileName);

	return (0);
}
