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
#include "kyra_pak.h"

static void showhelp(const char *exename);
static void process(const char *infile, const char *output);
static void processKyra3(const char *infile, const char *output);
static bool detectKyra3File(const char *infile);

#define OUTPUT_MP3 ".VO3"
#define OUTPUT_OGG ".VOG"
#define OUTPUT_FLAC ".VOF"

#define TEMPFILE "TEMP.VOC"

const char *outputExt = 0;
static CompressMode gCompMode = kMP3Mode;

int main(int argc, char *argv[]) {
	if (argc < 3)
		showhelp(argv[0]);

	char inputFile[1024];
	char outputFile[1024];
	int i = 0;

	/* Compression mode */
	gCompMode = kMP3Mode;
	i = 1;

	for (; i < argc - 2; ++i) {
		if (strcmp(argv[i], "--mp3") == 0)
			gCompMode = kMP3Mode;
		else if (strcmp(argv[i], "--vorbis") == 0)
			gCompMode = kVorbisMode;
		else if (strcmp(argv[i], "--flac") == 0)
			gCompMode = kFlacMode;
		else
			break;
	}

	switch (gCompMode) {
	case kMP3Mode:
		outputExt = OUTPUT_MP3;
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc - 2, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		outputExt = OUTPUT_OGG;
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc - 2, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		outputExt = OUTPUT_FLAC;
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc - 2, argv, i))
			showhelp(argv[0]);
		break;
	}

	sprintf(inputFile, "%s/%s", argv[argc - 2], argv[argc - 3]);
	sprintf(outputFile, "%s/%s", argv[argc - 1], argv[argc - 3]);

	if (scumm_stricmp(inputFile, outputFile) == 0)
		error("infile and outfile are the same file");

	bool isKyra3 = detectKyra3File(inputFile); 
	if (!isKyra3)
		process(inputFile, outputFile);
	else
		processKyra3(inputFile, outputFile);

	return 0;
}

static void showhelp(const char *exename) {
	printf("\nUsage: %s [params] [mode params] <file> <inputdir> <outputdir>\n", exename);

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
 	printf(" --fast       FLAC uses compression level 0\n");
 	printf(" --best       FLAC uses compression level 8\n");
 	printf(" -<value>     specifies the value (0 - 8) of compression (8=best)(default:%d)\n", flacCompressDef);
 	printf(" -b <value>   specifies a blocksize of <value> samples (default:%d)\n", flacBlocksizeDef);
	printf(" --verify     files are encoded and then decoded to check accuracy\n");
	printf(" --silent     the output of FLAC is hidden (default:disabled)\n");

	printf("\n --help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");

	exit(2);
}

static bool hasSuffix(const char *str, const char *suf) {
	const int sufSize = strlen(suf);

	int off = strlen(str);
	if (off < sufSize)
		return false;

	off -= sufSize;

	return (scumm_stricmp(&str[off], suf) == 0);
}

static void process(const char *infile, const char *outfile) {
	PAKFile input, output;

	if (!input.loadFile(infile, false))
		return;

	if (!output.loadFile(0, false))
		return;

	PAKFile::cFileList *list = input.getFileList();
	char outputName[32];

	for (; list; list = list->next) {
		if (!hasSuffix(list->filename, ".VOC"))
			continue;

		if (list->data[26] != 1) {
			warning("'%s' contains broken VOC file '%s' skipping it...", infile, list->filename);
			continue;
		}

		input.outputFileAs(list->filename, TEMPFILE);
		strncpy(outputName, list->filename, 32);

		FILE *tempFile = fopen(TEMPFILE, "rb");
		fseek(tempFile, 26, SEEK_CUR);
		extractAndEncodeVOC(TEMP_RAW, tempFile, gCompMode);
		fclose(tempFile);

		char *vocStart = strstr(outputName, ".VOC");
		for (unsigned int i = 0; i < strlen(outputExt); ++i)
			vocStart[i] = outputExt[i];

		output.addFile(outputName, tempEncoded);

		unlink(TEMPFILE);
		unlink(TEMP_RAW);
		unlink(tempEncoded);
	}

	if (output.getFileList())
		output.saveFile(outfile);
	else
		printf("file '%s' doesn't contain any .voc files\n", infile);
}

// Kyra3 specifc code

static uint16 clip8BitSample(int16 sample) {
	if (sample > 255)
		return 255;
	if (sample < 0)
		return 0;
	return sample;
}

static int decodeChunk(FILE *in, FILE *out) {
	uint16 size = readUint16LE(in);
	uint16 outSize = readUint16LE(in);
	uint32 id = readUint32LE(in);
	byte *inputBuffer, *outputBuffer;
	int bytesRead = 0;

	int16 curSample;
	uint8 code;
	int8 count;
	uint16 input;
	int i, j;

	uint16 remaining;

	const int8 WSTable2Bit[] = { -2, -1, 0, 1 };
	const int8 WSTable4Bit[] = {
		-9, -8, -6, -5, -4, -3, -2, -1,
		 0,  1,  2,  3,  4,  5,  6,  8
	};

	assert(id == 0x0000DEAF);

	bytesRead += (8 + size);

	outputBuffer = (byte *)malloc(outSize);
	assert(outputBuffer);

	if (size == outSize) {
		int readSize = size;
		while (readSize > 0) {
			int read = fread(outputBuffer, 1, readSize, in);
			if (read <= 0)
				error("[1] Couldn't read data");
			readSize -= read;
		}
		while (size > 0)  {
			int written = fwrite(outputBuffer, 1, size, out);
			if (written <= 0)
				error("[1] Couldn't write data");
			size -= written;
		}
		free(outputBuffer);
		return bytesRead;
	}

	inputBuffer = (byte *)malloc(size);
	assert(inputBuffer);

	int readSize = size;
	while (readSize > 0) {
		int read = fread(inputBuffer, 1, readSize, in);
		if (read <= 0)
			error("[2] Couldn't read data");
		readSize -= read;
	}

	curSample = 0x80;
	i = 0;
	j = 0;

	remaining = outSize;

	while (remaining > 0) {
		input = inputBuffer[i++] << 2;
		code = (input >> 8) & 0xff;
		count = (input & 0xff) >> 2;

		switch (code) {
		case 2:
			if (count & 0x20) {
				/* NOTE: count is signed! */
				count <<= 3;
				curSample += (count >> 3);
				outputBuffer[j++] = curSample;
				remaining--;
			} else {
				for (; count >= 0; count--) {
					outputBuffer[j++] = inputBuffer[i++];
					remaining--;
				}
				curSample = inputBuffer[i - 1];
			}
			break;
		case 1:
			for (; count >= 0; count--) {
				code = inputBuffer[i++];

				curSample += WSTable4Bit[code & 0x0f];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = curSample;

				curSample += WSTable4Bit[code >> 4];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = curSample;

				remaining -= 2;
			}
			break;
		case 0:
			for (; count >= 0; count--) {
				code = inputBuffer[i++];

				curSample += WSTable2Bit[code & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = curSample;

				curSample += WSTable2Bit[(code >> 2) & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = curSample;

				curSample += WSTable2Bit[(code >> 4) & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = curSample;

				curSample += WSTable2Bit[(code >> 6) & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = curSample;

				remaining -= 4;
			}
			break;
		default:
			for (; count >= 0; count--) {
				outputBuffer[j++] = curSample;
				remaining--;
			}
		}
	}

	while (outSize > 0)  {
		int written = fwrite(outputBuffer, 1, outSize, out);
		if (written <= 0)
			error("[2] Couldn't write data");
		outSize -= written;
	}

	free(inputBuffer);
	free(outputBuffer);

	return bytesRead;
}

typedef struct {
	uint16 freq;
	uint32 size;
	byte flags;
	byte type;
} AUDHeader;

static void compressAUDFile(FILE *input, const char *outfile) {
	AUDHeader header;

	header.freq = readUint16LE(input);
	header.size = readUint32LE(input);
	header.flags = readByte(input);
	header.type = readByte(input);
	//printf("%d Hz, %d bytes, type %d (%08X)\n", header.freq, header.size, header.type, header.flags);

	FILE *output = fopen(TEMP_RAW, "wb");

	if (!output)
		error("Couldn't create temporary file '%s'", TEMP_RAW);

	uint32 remaining = header.size;
	while (remaining > 0)
		remaining -= decodeChunk(input, output);

	fclose(output);

	encodeAudio(TEMP_RAW, true, header.freq, outfile, gCompMode);

	unlink(TEMP_RAW);
}

static void changeFileExt(char *filename) {
	char *str = filename + strlen(filename) - 4;

	if (*str != '.')
		error("Invalid filename '%s'", filename);

	++str;

	switch (gCompMode) {
	case kMP3Mode:
		*str++ = 'm';
		*str++ = 'p';
		*str++ = '3';
		break;

	case kVorbisMode:
		*str++ = 'o';
		*str++ = 'g';
		*str++ = 'g';
		break;

	case kFlacMode:
		*str++ = 'f';
		*str++ = 'l';
		*str++ = 'a';
		break;

	default:
		error("Unknown compression mode");
	}

	*str = 0;
}

struct DuplicatedFile {
	uint32 resFilename;
	uint32 resOffset;
};

static const DuplicatedFile *findDuplicatedFile(uint32 resOffset, const DuplicatedFile *list, const uint32 maxEntries) {
	for (uint32 i = 0; i < maxEntries; ++i) {
		if (list[i].resOffset == resOffset && list[i].resOffset != 0)
			return &list[i];
	}

	return 0;
}

static void processKyra3(const char *infile, const char *outfile) {
	if (hasSuffix(infile, ".AUD")) {
		char outname[1024];

		strncpy(outname, outfile, sizeof(outname));
		changeFileExt(outname);

		FILE *input = fopen(infile, "rb");
		if (!input)
			error("Couldn't open file '%s'", infile);

		compressAUDFile(input, outname);

		fclose(input);
	} else if (hasSuffix(infile, ".TLK")) {
		PAKFile output;

		FILE *input = fopen(infile, "rb");
		if (!input)
			error("Couldn't open file '%s'", infile);

		if (!output.loadFile(0, false))
			return;

		uint16 files = readUint16LE(input);
		DuplicatedFile *red = new DuplicatedFile[files];
		memset(red, 0, sizeof(DuplicatedFile)*files);

		for (uint16 i = 0; i < files; ++i) {
			uint32 resFilename = readUint32LE(input);
			uint32 resOffset = readUint32LE(input);

			char outname[16];
			snprintf(outname, 16, "%.08u.AUD", resFilename);
			changeFileExt(outname);

			const DuplicatedFile *file = findDuplicatedFile(resOffset, red, files);
			if (file) {
				char linkname[16];
				snprintf(linkname, 16, "%.08u.AUD", file->resFilename);
				changeFileExt(linkname);

				output.linkFiles(outname, linkname);
			} else {
				red[i].resFilename = resFilename;
				red[i].resOffset = resOffset;

				uint32 pos = (uint32)ftell(input);
				fseek(input, resOffset + 4, SEEK_SET);

				compressAUDFile(input, outname);

				output.addFile(outname, outname);

				unlink(outname);

				fseek(input, pos, SEEK_SET);
			}
		}

		delete[] red;
		fclose(input);

		if (output.getFileList())
			output.saveFile(outfile);
	} else {
		error("Unsupported file '%s'", infile);
	}
}

bool detectKyra3File(const char *infile) {
	if (hasSuffix(infile, ".AUD")) {
		return true;
	} else if (hasSuffix(infile, ".VRM") || hasSuffix(infile, ".PAK")) {
		if (!PAKFile::isPakFile(infile))
			error("Unknown filetype of file: '%s'", infile);
		return false;
	} else if (hasSuffix(infile, ".TLK")) {
		if (PAKFile::isPakFile(infile))
			return false;

		FILE *f = fopen(infile, "rb");
		if (!f)
			error("Couldn't open file '%s'", infile);

		uint16 entries = readUint16LE(f);
		uint32 entryTableSize = (entries * 8);
		const uint32 filesize = fileSize(f);

		if (entryTableSize + 2 > filesize) {
			fclose(f);
			error("Unknown filetype of file: '%s'", infile);
		}

		uint32 offset = 0;
		for (uint16 i = 0; i < entries; ++i) {
			readUint32LE(f);
			offset = readUint32LE(f);

			if (offset > filesize)
				error("Unknown filetype of file: '%s'", infile);
		}

		return true;
	}

	error("Unknown filetype of file: '%s'", infile);
}

