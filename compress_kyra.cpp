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

static void process(Filename *infile, Filename *output);
static void processKyra3(Filename *infile, Filename *output);
static bool detectKyra3File(Filename *infile);

#define TEMPFILE "TEMP.VOC"

static CompressMode gCompMode = kMP3Mode;

const char *helptext = "\nUsage: %s [params] [mode params] [-o out = ] <infile>\n" kCompressionAudioHelp;

int main(int argc, char *argv[]) {
	Filename inpath, outpath;
	int first_arg = 1;
	int last_arg = argc - 1;

	parseHelpArguments(argv, argc, helptext);

	// Compression mode
	gCompMode = process_audio_params(argc, argv, &first_arg);

	if(gCompMode == kNoAudioMode) {
		// Unknown mode (failed to parse arguments), display help and exit
		printf(helptext, argv[0]);
		exit(2);
	}
	
	// Now we try to find the proper output file
	// also make sure we skip those arguments
	if (parseOutputFileArguments(&outpath, argv, argc, first_arg))
		first_arg += 2;
	else if (parseOutputFileArguments(&outpath, argv, argc, last_arg - 2))
		last_arg -= 2;
	else
		// Standard output file is 'out'
		outpath.setFullPath("out");
	
	inpath.setFullName(argv[first_arg]);
	outpath.setFullName(argv[first_arg]);

	if (inpath.equals(&outpath))
		error("Infile and outfile cannot be the same file");

	bool isKyra3 = detectKyra3File(&inpath);
	if (!isKyra3)
		process(&inpath, &outpath);
	else
		processKyra3(&inpath, &outpath);

	return 0;
}

static bool hasSuffix(const char *str, const char *suf) {
	const int sufSize = strlen(suf);

	int off = strlen(str);
	if (off < sufSize)
		return false;

	off -= sufSize;

	return (scumm_stricmp(&str[off], suf) == 0);
}

static void process(Filename *infile, Filename *outfile) {
	PAKFile input, output;

	if (!input.loadFile(infile->getFullPath(), false))
		return;

	if (!output.loadFile(NULL, false))
		return;

	PAKFile::cFileList *list = input.getFileList();

	for (; list; list = list->next) {
		// Detect VOC file from content instead of extension. This is needed for Lands of Lore TLK files.
		if (memcmp(list->data, "Creative Voice File", 19) != 0)
			continue;

		if (list->data[26] != 1) {
			warning("'%s' contains broken VOC file '%s' skipping it...", infile->getFullPath(), list->filename);
			continue;
		}

		Filename outputName;
		input.outputFileAs(list->filename, TEMPFILE);
		strncpy(outputName._path, list->filename, 27);
		outputName._path[27] = 0;

		FILE *tempFile = fopen(TEMPFILE, "rb");
		fseek(tempFile, 26, SEEK_CUR);
		extractAndEncodeVOC(TEMP_RAW, tempFile, gCompMode);
		fclose(tempFile);

		outputName.setExtension(audio_extensions[gCompMode]);

		output.addFile(outputName.getFullPath(), tempEncoded);

		unlink(TEMPFILE);
		unlink(TEMP_RAW);
		unlink(tempEncoded);
	}

	if (output.getFileList())
		output.saveFile(outfile->getFullPath());
	else
		printf("file '%s' doesn't contain any .voc files\n", infile->getFullPath());
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
				outputBuffer[j++] = (byte)curSample;
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
				outputBuffer[j++] = (byte)curSample;

				curSample += WSTable4Bit[code >> 4];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				remaining -= 2;
			}
			break;
		case 0:
			for (; count >= 0; count--) {
				code = inputBuffer[i++];

				curSample += WSTable2Bit[code & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				curSample += WSTable2Bit[(code >> 2) & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				curSample += WSTable2Bit[(code >> 4) & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				curSample += WSTable2Bit[(code >> 6) & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				remaining -= 4;
			}
			break;
		default:
			for (; count >= 0; count--) {
				outputBuffer[j++] = (byte)curSample;
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

static void processKyra3(Filename *infile, Filename *outfile) {
	if (infile->hasExtension("AUD")) {
		outfile->setExtension(audio_extensions[gCompMode]);

		FILE *input = fopen(infile->getFullPath(), "rb");
		if (!input)
			error("Couldn't open file '%s'", infile->getFullPath());

		compressAUDFile(input, outfile->getFullPath());

		fclose(input);
	} else if (infile->hasExtension("TLK")) {
		PAKFile output;

		FILE *input = fopen(infile->getFullPath(), "rb");
		if (!input)
			error("Couldn't open file '%s'", infile->getFullPath());

		if (!output.loadFile(NULL, false))
			return;

		uint16 files = readUint16LE(input);
		DuplicatedFile *red = new DuplicatedFile[files];
		memset(red, 0, sizeof(DuplicatedFile)*files);

		for (uint16 i = 0; i < files; ++i) {
			uint32 resFilename = readUint32LE(input);
			uint32 resOffset = readUint32LE(input);

			char outname[16];
			snprintf(outname, 16, "%.08u.%s", resFilename, audio_extensions[gCompMode]);

			const DuplicatedFile *file = findDuplicatedFile(resOffset, red, files);
			if (file) {
				char linkname[16];
				snprintf(linkname, 16, "%.08u.%s", file->resFilename, audio_extensions[gCompMode]);

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
			output.saveFile(outfile->getFullPath());
	} else {
		error("Unsupported file '%s'", infile->getFullPath());
	}
}

bool detectKyra3File(Filename *infile) {
	if (infile->hasExtension("AUD")) {
		return true;
	} else if (infile->hasExtension("VRM") || infile->hasExtension("PAK")) {
		if (!PAKFile::isPakFile(infile->getFullPath()))
			error("Unknown filetype of file: '%s'", infile->getFullPath());
		return false;
	} else if (infile->hasExtension("TLK")) {
		if (PAKFile::isPakFile(infile->getFullPath()))
			return false;

		FILE *f = fopen(infile->getFullPath(), "rb");
		if (!f)
			error("Couldn't open file '%s'", infile->getFullPath());

		uint16 entries = readUint16LE(f);
		uint32 entryTableSize = (entries * 8);
		const uint32 filesize = fileSize(f);

		if (entryTableSize + 2 > filesize) {
			fclose(f);
			error("Unknown filetype of file: '%s'", infile->getFullPath());
		}

		uint32 offset = 0;
		for (uint16 i = 0; i < entries; ++i) {
			readUint32LE(f);
			offset = readUint32LE(f);

			if (offset > filesize)
				error("Unknown filetype of file: '%s'", infile->getFullPath());
		}

		return true;
	}

	error("Unknown filetype of file: '%s'", infile->getFullPath());
}

