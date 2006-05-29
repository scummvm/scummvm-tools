/* compress_sword1 - Compress Broken Sword I sound clusters into MP3/Ogg Vorbis
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

#define READ_BE_UINT32(x) \
	((((uint8*)(x))[0] << 24) | (((uint8*)(x))[1] << 16) | (((uint8*)(x))[2] << 8) | (((uint8*)(x))[3] << 0))

#define TOTAL_TUNES 269

char tempOutName[16];

typedef struct {
	char fileName[8];
	bool missing; /* Some of the music files seem to have been removed from the game. */
		      /* Try and look for them, but don't warn if they are missing. */
} MusicFile;

MusicFile musicNames[TOTAL_TUNES] = {
	{ "1M2", false },
	{ "1M3", false },
	{ "1M4", false },
	{ "1M6", false },
	{ "1M7", false },
	{ "1M8", false },
	{ "1M9", false },
	{ "1M10", false },
	{ "1M11", false },
	{ "1M12", false },
	{ "1M13", false },
	{ "1M14", false },
	{ "1M15", false },
	{ "1M16", false },
	{ "1M17", false },
	{ "1M18", false },
	{ "1M19", false },
	{ "1M20", false },
	{ "1M21", false },
	{ "1M22", false },
	{ "1M23", false },
	{ "1M24", false },
	{ "1M25", false },
	{ "1M26", false },
	{ "1M27", false },
	{ "1M28", false },
	{ "1M29", false },
	{ "1M28A", false },
	{ "1M30", false },
	{ "1M31", false },
	{ "1M32", false },
	{ "1M34", false },
	{ "1M35", false },
	{ "2M1", false },
	{ "2M2", false },
	{ "2M4", false },
	{ "2M5", false },
	{ "2M6", false },
	{ "2M7", false },
	{ "2M8", false },
	{ "2M9", false },
	{ "2M10", false },
	{ "2M11", false },
	{ "2M12", false },
	{ "2M13", false },
	{ "2M14", false },
	{ "2M15", false },
	{ "2M16", false },
	{ "2M17", false },
	{ "2M18", false },
	{ "2M19", false },
	{ "2M20", false },
	{ "2M21", false },
	{ "2M22", false },
	{ "2M23", false },
	{ "2M24", false },
	{ "2M25", false },
	{ "2M26", false },
	{ "2M27", false },
	{ "2M28", false },
	{ "2M29", false },
	{ "2M30", false },
	{ "2M31", false },
	{ "2M32", false },
	{ "2M33", false },
	{ "1M28", false },
	{ "2M24", false },
	{ "2M6", false },
	{ "1M25", false },
	{ "2M38", false },
	{ "2M39", false },
	{ "2M40", false },
	{ "3M1", false },
	{ "3M2", false },
	{ "3M3", false },
	{ "3M4", false },
	{ "1M28", false },
	{ "2M26", false },
	{ "3M7", false },
	{ "3M8", false },
	{ "3M9", true }, 
	{ "3M10", false },
	{ "2M13", false },
	{ "3M12", false },
	{ "3M13", false },
	{ "3M14", false },
	{ "2M9", false },
	{ "3M17", false },
	{ "3M18", false },
	{ "3M19", false },
	{ "3M20", false },
	{ "3M21", false },
	{ "3M22", false },
	{ "3M24", false },
	{ "3M26", false },
	{ "3M27", false },
	{ "2M26", false },
	{ "3M29", false },
	{ "3M30", false },
	{ "3M32", false },
	{ "3M33", false },
	{ "2M13", false },
	{ "4M3", false },
	{ "4M4", false },
	{ "4M5", false },
	{ "4M6", false },
	{ "4M8", false },
	{ "4M9", false },
	{ "4M10", false },
	{ "4M11", false },
	{ "4M12", false },
	{ "4M13", false },
	{ "4M14", false },
	{ "4M15", false },
	{ "4M17", false },
	{ "4M18", false },
	{ "4M19", false },
	{ "4M20", false },
	{ "4M21", false },
	{ "4M22", false },
	{ "4M24", false },
	{ "4M25", false },
	{ "4M28", false },
	{ "4M29", false },
	{ "4M31", false },
	{ "4M32", false },
	{ "5M1", false },
	{ "5M2", true }, 
	{ "5M3", false },
	{ "5M4", false },
	{ "5M5", false },
	{ "5M6", false },
	{ "5M8", false },
	{ "5M9", false },
	{ "5M10", false },
	{ "5M11", false },
	{ "5M12", false },
	{ "5M13", false },
	{ "5M14", false },
	{ "5M17", false },
	{ "5M22", true },
	{ "5M23", false },
	{ "5M24", false },
	{ "2M3", false },
	{ "6M1", false },
	{ "6M2", false },
	{ "6M3", false },
	{ "6M4", false },
	{ "6M5", false },
	{ "6M6", false },
	{ "6M7", false },
	{ "6M8", false },
	{ "6M12", true },
	{ "2M6", false },
	{ "5M1", false },
	{ "6M15", false },
	{ "7M1", false },
	{ "7M2", false },
	{ "7M4", false },
	{ "7M5", false },
	{ "7M6", false },
	{ "7M7", false },
	{ "7M8", false },
	{ "7M11", false },
	{ "7M14", false },
	{ "7M15", false },
	{ "5M18", false },
	{ "6M11", false },
	{ "7M17", false },
	{ "7M18", false },
	{ "7M19", false },
	{ "7M20", false },
	{ "7M21", false },
	{ "7M22", false },
	{ "7M23", false },
	{ "7M28", false },
	{ "7M30", false },
	{ "7M31", false },
	{ "7M32", false },
	{ "7M33", false },
	{ "7M34", false },
	{ "8M1", false },
	{ "8M2", false },
	{ "8M4", false },
	{ "8M7", false },
	{ "8M10", false },
	{ "8M11", false },
	{ "8M12", false },
	{ "8M13", false },
	{ "8M14", false },
	{ "8M15", false },
	{ "8M16", false },
	{ "8M18", false },
	{ "8M19", false },
	{ "8M20", false },
	{ "8M21", false },
	{ "8M22", false },
	{ "8M24", false },
	{ "8M26", false },
	{ "8M28", false },
	{ "8M29", false },
	{ "8M30", false },
	{ "8M31", false },
	{ "8M37", true },
	{ "8M38", false },
	{ "8M39", false },
	{ "8M40", false },
	{ "8M41", false },
	{ "9M1", false },
	{ "9M2", false },
	{ "9M3", false },
	{ "9M5", false },
	{ "9M6", false },
	{ "9M7", false },
	{ "9M8", false },
	{ "9M9", false },
	{ "9M10", false },
	{ "9M11", false },
	{ "9M13", false },
	{ "9M14", false },
	{ "9M15", false },
	{ "9M17", false },
	{ "9M18", false },
	{ "9M19", false },
	{ "9M20", false },
	{ "9M21", false },
	{ "9M22", false },
	{ "9M23", false },
	{ "9M24", false },
	{ "9M25", false },
	{ "10M1", false },
	{ "10M2", false },
	{ "10M3", false },
	{ "10M4", false },
	{ "11M1", false },
	{ "11M3", false },
	{ "11M4", false },
	{ "11M7", false },
	{ "11M8", false },
	{ "11M9", true }, 
	{ "12M1", false },
	{ "11M2", false },
	{ "SPM2", false },
	{ "SPM3", false },
	{ "SPM4", false },
	{ "SPM5", false },
	{ "SPM6", false },
	{ "SCM1", false },
	{ "SCM2", false },
	{ "SCM3", false },
	{ "SCM4", false },
	{ "SCM5", false },
	{ "SCM6", false },
	{ "SCM7", false },
	{ "SCM8", false },
	{ "SCM11", false },
	{ "RM3A", false },
	{ "RM3B", false },
	{ "SCM16", false },
	{ "SCM1B", false },
	{ "SPM6B", false },
	{ "MARQUET", false },
	{ "RM4", false },
	{ "RM5", false },
	{ "RM6", false },
	{ "RM7", false },
	{ "RM8", false },
	{ "RM3C", false },
	{ "RM3D", false }
};

void showhelp(char *exename) {
	printf("\nUsage: %s <params>\n", exename);

	printf("\nParams:\n");
	printf(" --mp3          encode to MP3 format (default)\n");
	printf(" --vorbis       encode to Vorbis format\n");
	printf(" --speech-only  only encode speech clusters\n");
	printf(" --music-only   only encode music files\n");
	printf("                (default: encode both)\n");
	printf("(The above parameters have to be specified first)\n");

	printf("\nMP3 mode params:\n");
	printf(" -b <rate>      <rate> is the target bitrate(ABR)/minimal bitrate(VBR)\n");
	printf("                (default:%d)\n", minBitrDef);
	printf(" -B <rate>      <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf(" --vbr          LAME uses the VBR mode (default)\n");
	printf(" --abr          LAME uses the ABR mode\n");
	printf(" -V <value>     specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf(" -q <value>     specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf(" --silent       the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf(" -b <rate>      <rate> is the nominal bitrate (default:unset)\n");
	printf(" -m <rate>      <rate> is the minimum bitrate (default:unset)\n");
	printf(" -M <rate>      <rate> is the maximum bitrate (default:unset)\n");
	printf(" -q <value>     specifies the value (0 - 10) of VBR quality (10=best)\n");
	printf("                (default:%d)\n", oggqualDef);
	printf(" --silent       the output of oggenc is hidden (default:disabled)\n");

	printf("\n --help         this help message\n");

	printf("\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	printf("\nPlease run this tool from your BS1 main directory.\n");
	printf("(The one with the subdirectories \"MUSIC\" and \"SPEECH\")\n");
	exit(2);
}

int16 *uncompressSpeech(FILE *clu, uint32 idx, uint32 cSize, uint32 *returnSize) {
	uint32 resSize, srcPos;
	int16 *srcData, *dstData, *dstPos;
	uint32 headerPos = 0;
	int16 length, cnt;
	uint8 *fBuf = (uint8 *)malloc(cSize);
	fseek(clu, idx, SEEK_SET);
	assert(fread(fBuf, 1, cSize, clu) == cSize);
	while ((READ_BE_UINT32(fBuf + headerPos) != 'data') && (headerPos < 100))
		headerPos++;
	if (headerPos < 100) {
		resSize = READ_LE_UINT32(fBuf + headerPos + 4) >> 1;
		srcData = (int16 *)(fBuf + headerPos + 8);
		dstData = (int16 *)malloc(resSize * 2);
		srcPos = 0;
		dstPos = dstData;
		cSize = (cSize - (headerPos + 8)) / 2;
		while (srcPos < cSize) {
			length = (int16)READ_LE_UINT16(srcData + srcPos);
			srcPos++;
			if (length < 0) {
				length = -length;
				for (cnt = 0; cnt < (uint16)length; cnt++)
					*dstPos++ = srcData[srcPos];
				srcPos++;
			} else {
				memcpy(dstPos, srcData + srcPos, length * 2);
				dstPos += length;
				srcPos += length;
			}
		}
		free(fBuf);
		*returnSize = resSize * 2;
		return dstData;
	} else {
		free(fBuf);
		error("Sound::uncompressSpeech(): DATA tag not found in wave header");
		*returnSize = 0;
		return NULL;
	}
}

uint8 *convertData(uint8 *rawData, uint32 rawSize, CompressMode compMode, uint32 *resSize) {
	FILE *temp;
	uint8 *resBuf;
	uint32 size;
	temp = fopen(TEMP_RAW, "wb");
	assert(fwrite(rawData, 1, rawSize, temp) == rawSize);
	fclose(temp);
	encodeAudio(TEMP_RAW, true, 11025, tempOutName, compMode);
	temp = fopen(tempOutName, "rb");
	fseek(temp, 0, SEEK_END);
	*resSize = size = ftell(temp);
	resBuf = (uint8*)malloc(size);
	fseek(temp, 0, SEEK_SET);
	fread(resBuf, 1, size, temp);
	fclose(temp);
	return resBuf;
}

void convertClu(FILE *clu, FILE *cl3, CompressMode compMode) {
	uint32 *cowHeader;
	uint32 numRooms;
	uint32 numSamples;
	uint32 cnt;
	uint32 *cl3Index, *sampleIndex;
	uint32 smpSize, mp3Size;
	uint8 *smpData, *mp3Data;

	uint32 headerSize = readUint32LE(clu);

	assert(!(headerSize & 3));
	cowHeader = (uint32*)malloc(headerSize);
	for (cnt = 0; cnt < (headerSize / 4) - 1; cnt++)
		cowHeader[cnt] = readUint32LE(clu);
	assert(!(cowHeader[0] & 3));
	numRooms = cowHeader[0] / 4;
	assert(cowHeader[numRooms] == 0);	/* This dword should be unused. */
	/* The samples are divided into rooms and samples. We don't care about the room indexes at all. */
	/* We simply copy them and go to the sample-index data. */
	writeUint32LE(cl3, headerSize);
	for (cnt = 0; cnt < numRooms; cnt++)
		writeUint32LE(cl3, cowHeader[cnt]);
	writeUint32LE(cl3, cowHeader[numRooms]);

	numSamples = (((headerSize / 4) - numRooms) / 2) - 1;
	for (cnt = 0; cnt < numSamples * 2; cnt++) {
		/* This is where we'll put the sample index data later. */
		writeUint32BE(cl3, 0xdeadbeefL);
	}
	cl3Index = (uint32*)malloc(numSamples * 8);
	memset(cl3Index, 0, numSamples * 8);
	
	sampleIndex = cowHeader + numRooms + 1;
	/* This points to the sample index table. 8 bytes each (4 bytes size and then 4 bytes file index) */

	printf("converting %d samples\n", numSamples);

	for (cnt = 0; cnt < numSamples; cnt++) {
		if (sampleIndex[cnt << 1] | sampleIndex[(cnt << 1) | 1]) {
			printf("sample %5d: ", cnt);
			smpData = (uint8*)uncompressSpeech(clu, sampleIndex[cnt << 1] + headerSize, sampleIndex[(cnt << 1) | 1], &smpSize);
			if ((!smpData) || (!smpSize))
				error("unable to handle speech sample %d!\n", cnt);

			mp3Data = convertData(smpData, smpSize, compMode, &mp3Size);
			cl3Index[cnt << 1] = ftell(cl3);
			cl3Index[(cnt << 1) | 1] = mp3Size;
			assert(fwrite(mp3Data, 1, mp3Size, cl3) == mp3Size);

			free(smpData);
			free(mp3Data);
		} else {
			cl3Index[cnt << 1] = cl3Index[(cnt << 1) | 1] = 0;
			printf("sample %5d: skipped\n", cnt);
		}
	}
	fseek(cl3, (numRooms + 2) * 4, SEEK_SET);	/* Now write the sample index into the CL3 file */
	for (cnt = 0; cnt < numSamples * 2; cnt++)
		writeUint32LE(cl3, cl3Index[cnt]);
	free(cl3Index);
	free(cowHeader);
}

void compressSpeech(CompressMode compMode) {
	FILE *clu, *cl3 = NULL;
	int i;
	char cluName[256], outName[256];

	setRawAudioType(true, false, 16);
	for (i = 1; i <= 2; i++) {
		sprintf(cluName, "SPEECH/SPEECH%d.CLU", i);
		clu = fopen(cluName, "rb");
		if (!clu) {
			printf("Unable to open \"SPEECH%d.CLU\".\n", i);
			printf("Please copy the \"SPEECH.CLU\" from CD %d\nand rename it to \"SPEECH%d.CLU\".\n", i, i);
		} else {
			sprintf(outName, "SPEECH/SPEECH%d.%s", i, (compMode == kMP3Mode) ? ("CL3") : ("CLV"));

			cl3 = fopen(outName, "wb");
			if (!cl3) {
				printf("Unable to create file \"%s\".\n", outName);
				printf("Please make sure you've got write permission in this directory.\n");
			} else {
				printf("Converting CD %d...\n", i);
				convertClu(clu, cl3, compMode);
			}
		}
		if (clu)
			fclose(clu);
		if (cl3)
			fclose(cl3);
	}
	unlink(TEMP_RAW);
	unlink(tempOutName);
}

void compressMusic(CompressMode compMode) {
	int i;
	FILE *inf;
	char fNameIn[256], fNameOut[256];
	for (i = 0; i < TOTAL_TUNES; i++) {
		sprintf(fNameIn, "MUSIC/%s.WAV", musicNames[i].fileName);
		inf = fopen(fNameIn, "rb");
		if (!inf) {
			if (!musicNames[i].missing) {
				printf("unable to open file \"%s\"\n", fNameIn);
			}
		} else {
			fclose(inf);
			sprintf(fNameOut, "MUSIC/%s.%s", musicNames[i].fileName, (compMode == kMP3Mode) ? "MP3" : "OGG");
			
			printf("encoding file (%3d/%d) %s -> %s.%s\n", i + 1, TOTAL_TUNES, musicNames[i].fileName, musicNames[i].fileName, (compMode == kMP3Mode) ? "MP3" : "OGG");
			encodeAudio(fNameIn, false, -1, fNameOut, compMode);
		}
	}
}

void processArgs(int argc, char *argv[], int i, CompressMode mode) {
	/* HACK: the functions in compress.c expect the last argument to be a filename. */
	/*       As we don't expect one, we simply add a dummy argument to the list. */
	char **args;
	int cnt;
	int result;
	char dummyName[] = "dummy";
	args = (char **)malloc((argc + 1) * sizeof(char *));
	for (cnt = 0; cnt < argc; cnt++)
		args[cnt] = argv[cnt];
	args[argc] = dummyName;
	if (mode == kMP3Mode)
		result = process_mp3_parms(argc + 1, args, i);
	else if (mode == kVorbisMode)
		result = process_ogg_parms(argc + 1, args, i);
	else
		error("Unknown encoding method");
	if (!result)
		showhelp(argv[0]);
	free(args);
}

void checkFilesExist(bool checkSpeech, bool checkMusic) {
	int i;
	FILE *testFile;
	char fileName[256];
	bool speechFound = false, musicFound = false;
	if (checkSpeech) {
		for (i = 1; i <= 2; i++) {
			sprintf(fileName, "SPEECH/SPEECH%d.CLU", i);
			testFile = fopen(fileName, "rb");
			if (testFile){
				speechFound = true;
				fclose(testFile);
			}
		}
		if (!speechFound) {
			printf("Unable to find speech files.\n");
			printf("Please copy the SPEECH.CLU files from Broken Sword CD1 and CD2\n");
			printf("into the \"SPEECH\" subdirectory and rename them to\n");
			printf("SPEECH1.CLU and SPEECH2.CLU\n\n");
			printf("If your OS is case-sensitive, make sure the filenames\n");
			printf("and directorynames are all upper-case.\n\n");
		}
	}
	if (checkMusic) {
		for (i = 0; i < 20; i++) { /* Check the first 20 music files */
			sprintf(fileName, "MUSIC/%s.WAV", musicNames[i].fileName);
			testFile = fopen(fileName, "rb");
			if (testFile) {
				musicFound = true;
				fclose(testFile);
			}
		}
		if (!musicFound) {
			printf("Unable to find music files.\n");
			printf("Please copy the music files from Broken Sword CD1 and CD2\n");
			printf("into the \"MUSIC\" subdirectory.\n");
			printf("If your OS is case-sensitive, make sure the filenames\n");
			printf("and directorynames are all upper-case.\n");
		}
	}
	if ((checkSpeech && (!speechFound)) || (checkMusic && (!musicFound))) {
		printf("\nUse --help for more information\n");
		exit(2);
	}
}

int main(int argc, char *argv[]) {
	CompressMode compMode = kMP3Mode;
	int i = 1;
	bool compMusic = true, compSpeech = true;

	while (i < argc) {
		if (!strcmp(argv[i], "--mp3"))
			compMode = kMP3Mode;
		else if (!strcmp(argv[i], "--vorbis"))
			compMode = kVorbisMode;
		else if (!strcmp(argv[i], "--speech-only"))
			compMusic = false;
		else if (!strcmp(argv[i], "--music-only"))
			compSpeech = false;
		else
			break;
		i++;
	}

	if (compMode == kMP3Mode)
		strcpy(tempOutName, TEMP_MP3);
	else
		strcpy(tempOutName, TEMP_OGG);
	
	processArgs(argc, argv, i, compMode);

	/* Do a quick check to see if we can open any files at all */
	checkFilesExist(compSpeech, compMusic);

	if (compSpeech)
		compressSpeech(compMode);
	if (compMusic)
		compressMusic(compMode);
	
	return EXIT_SUCCESS;
}

