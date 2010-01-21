/* extract_mohawk - Mohawk file extractor
 * Copyright (C) 2009 The ScummVM project
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

#include "mohawk_file.h"
#include "util.h"
#include "utils/file.h"

#include <assert.h>

// Have a maximum buffer size
#define MAX_BUF_SIZE 16384

static byte *outputBuffer = NULL;

bool fileExists(const char *filename) {
	FILE *outputFile = fopen(filename, "rb");
	if (outputFile != NULL) {
		fclose(outputFile);
		return true;
	}
	return false;
}

void dumpRawResource(MohawkOutputStream output) {
	// Change the extension to bin
	output.name += ".bin";

	const char* outputCStr = output.name.c_str();

	printf ("Extracting \'%s\'...\n", outputCStr);

	if(fileExists(outputCStr)) {
		printf ("File \'%s\' already exists!\n", outputCStr);
		return;
	}
	FILE *outputFile = fopen(outputCStr, "wb");
	if (!outputFile) {
		printf ("Could not open file for output!\n");
		return;
	}

	assert(outputBuffer);

	while (output.stream->pos() < output.stream->size()) {
		uint32 size = output.stream->read(outputBuffer, MAX_BUF_SIZE);
		fwrite(outputBuffer, 1, size, outputFile);
	}

	fclose(outputFile);
}

void convertSoundResource(MohawkOutputStream output) {
	printf ("Converting sounds not yet supported. Dumping instead...\n");
	dumpRawResource(output);
}

void convertMovieResource(MohawkOutputStream output) {
	printf ("Converting movies not yet supported. Dumping instead...\n");
	dumpRawResource(output);
}

void convertMIDIResource(MohawkOutputStream output) {
	// Change the extension to midi
	output.name += ".mid";

	const char* outputCStr = output.name.c_str();

	printf ("Extracting \'%s\'...\n", outputCStr);

	if(fileExists(outputCStr)) {
		printf ("File \'%s\' already exists!\n", outputCStr);
		return;
	}
	FILE *outputFile = fopen(outputCStr, "wb");
	if (!outputFile) {
		printf ("Could not open file for output!\n");
		return;
	}

	// Read the Mohawk MIDI header
	assert(output.stream->readUint32BE() == ID_MHWK);
	output.stream->readUint32BE(); // Skip size
	assert(output.stream->readUint32BE() == ID_MIDI);

	uint32 size = output.stream->size() - 12; // Skip MHWK header's size

	byte *midiData = (byte *)malloc(size);

	// Read the MThd Data
	output.stream->read(midiData, 14);

	// Skip the unknown Prg# section
	assert(output.stream->readUint32BE() == ID_PRG);
	output.stream->skip(output.stream->readUint32BE());

	// Read the MTrk Data
	uint32 mtrkSize = output.stream->size() - output.stream->pos();
	output.stream->read(midiData + 14, mtrkSize);

	// Output the data to the file.
	fwrite(midiData, 1, 14 + mtrkSize, outputFile);
	free(midiData);

	fclose(outputFile);
}

void outputMohawkStream(MohawkOutputStream output, bool doConversion) {
	// File output naming format preserves all archive information...
	char *strBuf = (char *)malloc(256);
	sprintf(strBuf, "%04d_%s_%d", output.index, tag2str(output.tag), output.id);
	if (!output.name.empty())
		sprintf(strBuf + strlen(strBuf), "_%s", output.name.c_str());
	output.name = strBuf;

	if (doConversion) {
		// Intercept the sound tags
		if (output.tag == ID_TWAV || output.tag == ID_MSND || output.tag == ID_SND) {
			convertSoundResource(output);
			return;
		}

		// Intercept the movie tag (need to change the offsets)
		if (output.tag == ID_TMOV) {
			convertMovieResource(output);
			return;
		}

		// Intercept the MIDI tag (strip out Mohawk header/Prg# stuff)
		if (output.tag == ID_TMID) {
			convertMIDIResource(output);
			return;
		}

		// TODO: Convert other resources? PICT/WDIB/tBMP?
	}

	// Default to dump raw binary...
	dumpRawResource(output);
}

void printUsage(const char *appName) {
	printf("Usage: %s [options] <mohawk archive> [tag id]\n", appName);
	printf("Options : --raw       Dump Resources as raw binary dump (default)\n");
	printf("          --convert   Dump Resources as converted files\n");
}

int main(int argc, char *argv[]) {
	bool doConversion = false;
	int archiveArg;

	// Parse parameters
	for (archiveArg = 1; archiveArg < argc; archiveArg++) {
		Common::String current = Common::String(argv[archiveArg]);

		if(!current.hasPrefix("--"))
			break;

		// Decode options
		if (current.equals("--raw"))
			doConversion = false;
		else if (current.equals("--convert"))
			doConversion = true;
		else {
			printf("Unknown argument : \"%s\"\n", argv[archiveArg]);
			printUsage(argv[0]);
			return 1;
		}
	}

	if (archiveArg != argc - 1 && archiveArg != argc - 2 - 1) { // No tag and id or tag and id present
		printUsage(argv[0]);
		return 1;
	}

	FILE *file = fopen(argv[archiveArg], "rb");
	if (!file) {
		printf ("Could not open \'%s\'\n", argv[archiveArg]);
		return 1;
	}

	// Open the file as a Mohawk archive
	MohawkFile *mohawkFile = MohawkFile::createMohawkFile(new Common::File(file));
	
	if (!mohawkFile) {
		printf("\'%s\' is not a valid Mohawk archive\n", argv[archiveArg]);
		fclose(file);
		return 1;
	}

	// Allocate a buffer for the output
	outputBuffer = (byte *)malloc(MAX_BUF_SIZE);

	if (argc == archiveArg - 2 - 1) {
		uint32 tag = READ_BE_UINT32(argv[archiveArg + 1]);
		uint16 id = (uint16)atoi(argv[archiveArg + 2]);

		MohawkOutputStream output = mohawkFile->getRawData(tag, id);

		if (output.stream) {
			outputMohawkStream(output, doConversion);
			delete output.stream;
		} else {
			printf ("Could not find specified data!\n");
		}
	} else {
		MohawkOutputStream output = mohawkFile->getNextFile();
		while (output.stream) {
			outputMohawkStream(output, doConversion);
			delete output.stream;
			output = mohawkFile->getNextFile();
		}
	}

	printf("Done!\n");
	free(outputBuffer);
	mohawkFile->close();
	fclose(file);
	return 0;
}
