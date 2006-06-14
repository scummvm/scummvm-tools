/* compress_kyra_bun - compressor for kyra sound file packages
 * Copyright (C) 2006  The ScummVM Team
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

struct PakFileEntry {
	char filename[25];
	uint8 *fileData;
	uint32 size;
};

static void showhelp(const char *exename);
static void process(FILE *file, const char *output);
static void decompress(uint8 *data, uint32 size, PakFileEntry &output);
static void processFile(const char *file, PakFileEntry &output);

#define OUTPUT_MP3 ".VO3"
#define OUTPUT_OGG ".VOG"
#define OUTPUT_FLAC ".VOF"

#define TEMPFILE "TEMP.VOC"

const char *outputName = 0;
static CompressMode gCompMode = kMP3Mode;

int main(int argc, char *argv[]) {
	if (argc < 3)
		showhelp(argv[0]);

	int i = 0;
	/* Compression mode */
	gCompMode = kMP3Mode;
	i = 1;
	if (strcmp(argv[1], "--mp3") == 0) {
		gCompMode = kMP3Mode;
		i++;
	}
	else if (strcmp(argv[1], "--vorbis") == 0) {
		gCompMode = kVorbisMode;
		i++;
	}
	else if (strcmp(argv[1], "--flac") == 0) {
		gCompMode = kFlacMode;
		i++;
	}

	switch (gCompMode) {
	case kMP3Mode:
		outputName = OUTPUT_MP3;
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc - 1, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		outputName = OUTPUT_OGG;
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc - 1, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		outputName = OUTPUT_FLAC;
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc - 1, argv, i))
			showhelp(argv[0]);
		break;
	}
	
	i = argc - 2;
	
	FILE *input = fopen(argv[i], "rb");
	if (!input) {
		printf("Cannot open file: %s\n", argv[i]);
		exit(-1);
	}
	
	process(input, argv[argc - 1]);
	fclose(input);
}

static void showhelp(const char *exename) {
	printf("\nUsage: %s <params> infile outfile\n", exename);

	printf("\nParams:\n");
	printf(" --mp3        encode to MP3 format (default)\n");
	printf(" --vorbis     encode to Vorbis format\n");
	printf(" --flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

	printf("\nMP3 mode params:\n");
	printf(" -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%d)\n", minBitrDef);
	printf(" -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf(" --vbr        LAME uses the VBR mode (default)\n");
	printf(" --abr        LAME uses the ABR mode\n");
	printf(" -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf(" -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf(" --silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf(" -b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf(" -m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf(" -M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf(" -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%d)\n", oggqualDef);
	printf(" --silent     the output of oggenc is hidden (default:disabled)\n");

	printf("\nFlac mode params:\n");
	printf(" [params]     optional arguments passed directly to the encoder\n");
	printf("              recommended is: --best -b 1152\n");

	printf("\n --help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");

	exit(2);
}

static void process(FILE *file, const char *output) {
	FILE *outputFile = fopen(output, "wb");
	if (!outputFile) {
		printf("Cannot open file: %s\n", output);
		exit(-1);
	}

	uint32 file_size = fileSize(file);
	fseek(file, 0, SEEK_SET);
	int filesInPak = 0;
	readUint32LE(file);
	while (1) {
		++filesInPak;
		PakFileEntry tempPak;
		int namePos = 0;
		while (1) {
			tempPak.filename[namePos++] = readByte(file);
			if (!tempPak.filename[namePos-1])
				break;
		}
		uint32 temp = readUint32LE(file);
		if (temp == file_size)
			break;
	}
	if (filesInPak == 0) {
		printf("ERROR: Empty or unknown PAK file format\n");
		exit(-1);
	}
	printf("Found %d files in package\n", filesInPak);
	
	PakFileEntry *pakEntries = new PakFileEntry[filesInPak];
	assert(pakEntries);
	memset(pakEntries, 0, sizeof(PakFileEntry)*filesInPak);
	
	fseek(file, 0, SEEK_SET);
	
	uint32 startOffset = readUint32LE(file);
	uint32 endOffset = 0;
	int pakPos = 0;
	while (!feof(file)) {
		PakFileEntry tempPak;
		
		int namePos = 0;
		while (1) {
			tempPak.filename[namePos++] = readByte(file);
			if (!tempPak.filename[namePos-1])
				break;
		}		
		endOffset = readUint32LE(file);
		
		if (strstr(tempPak.filename, ".VOC") == NULL) {
			if (endOffset == file_size)
				break;
			startOffset = endOffset;
			continue;
		}
		
		if (!endOffset)
			endOffset = file_size;
		
		long position = ftell(file);
		
		fseek(file, startOffset, SEEK_SET);
		fseek(file, 26, SEEK_CUR);

		if (fgetc(file) != 1) {
			warning("broken VOC file '%s' skipping it...", tempPak.filename);
			startOffset = endOffset;
			fseek(file, position, SEEK_SET);
			continue;
		}

		fseek(file, -1, SEEK_CUR);
		
		uint8 *temp = new uint8[endOffset - startOffset];
		assert(temp);
		fread(temp, sizeof(uint8), endOffset - startOffset, file);

		char *vocStart = strstr(tempPak.filename, ".VOC");
		for (unsigned int i = 0; i < strlen(outputName); ++i) {
			vocStart[i] = outputName[i];
		}
		strcpy(pakEntries[pakPos].filename, tempPak.filename);
		
		decompress(temp, endOffset - startOffset, pakEntries[pakPos++]);
		
		delete [] temp;
		temp = 0;
		
		fseek(file, position, SEEK_SET);
		
		if (endOffset == file_size)
			break;
		startOffset = endOffset;
	}
	
	// writes the new pack file
	uint32 startAddr = 0x0;
	static const char *zeroName = "\0\0\0\0\0";
	
	for (int i = 0; i < filesInPak; ++i) {
		if (strcmp(pakEntries[i].filename, "") == 0)
			continue;
		startAddr += strlen(pakEntries[i].filename) + 1 + 4;
	}
	startAddr += 5 + 4;
	
	// writes the filenames
	uint32 curAddr = startAddr;
	for (int i = 0; i < filesInPak; ++i) {
		if (strcmp(pakEntries[i].filename, "") == 0)
			continue;
		writeUint32LE(outputFile, curAddr);
		fwrite(pakEntries[i].filename, sizeof(uint8), strlen(pakEntries[i].filename) + 1, outputFile);
		curAddr += pakEntries[i].size;
	}
	
	writeUint32LE(outputFile, curAddr);
	fwrite(zeroName, sizeof(uint8), 5, outputFile);
	
	for (int i = 0; i < filesInPak; ++i) {
		if (strcmp(pakEntries[i].filename, "") == 0)
			continue;
		fwrite(pakEntries[i].fileData, sizeof(uint8), pakEntries[i].size, outputFile);
		delete [] pakEntries[i].fileData;
		pakEntries[i].fileData = 0;
	}
	
	fclose(outputFile);
	unlink(TEMPFILE);
}

static void decompress(uint8 *data, uint32 size, PakFileEntry &output) {
	FILE *tempFile = fopen(TEMPFILE, "wb");
	if (!tempFile) {
		printf("Cannot open file: %s\n", TEMPFILE);
		exit(-1);
	}
	fwrite(data, sizeof(uint8), size, tempFile);
	fclose(tempFile);
	
	tempFile = fopen(TEMPFILE, "rb");
	if (!tempFile) {
		printf("Cannot open file: %s\n", TEMPFILE);
		exit(-1);
	}
	
	extractAndEncodeVOC(TEMP_RAW, tempFile, gCompMode);
	processFile(tempEncoded, output);
	fclose(tempFile);

	unlink(TEMPFILE);
	unlink(TEMP_RAW);
	unlink(tempEncoded);
}

static void processFile(const char *file, PakFileEntry &output) {
	FILE *tempFile = fopen(file, "rb");
	if (!tempFile) {
		printf("Cannot open file: %s\n", file);
		exit(-1);
	}
	
	fseek(tempFile, 0, SEEK_END);
	output.size = ftell(tempFile);
	
	fseek(tempFile, 0, SEEK_SET);
	
	output.fileData = new uint8[output.size];
	assert(output.fileData);
	
	fread(output.fileData, sizeof(uint8), output.size, tempFile); 
	
	fclose(tempFile);
}
