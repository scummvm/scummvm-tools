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
#include "utils/file.h"

// Have a maximum buffer size
#define MAX_BUF_SIZE 16384

void printUsage(const char *appName) {
	printf("Usage: %s <mohawk archive> [tag id]\n", appName);
}

static byte *outputBuffer = NULL;

void convertSoundResource(MohawkOutputStream output) {
	printf ("Converting sounds not yet supported.\n");
}

void convertMovieResource(MohawkOutputStream output) {
	printf ("Converting movies not yet supported. Dumping instead...\n");
}

void convertMIDIResource(MohawkOutputStream output) {
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

	// Change the extension to midi
	output.name += ".mid";

	printf ("Extracting \'%s\'...\n", output.name.c_str());

	FILE *outputFile = fopen(output.name.c_str(), "wb");
	if (!outputFile) {
		printf ("Could not open file for output!\n");
		free(midiData);
		return;
	}

	// Output the data to the file.
	fwrite(midiData, 1, 14 + mtrkSize, outputFile);
	free(midiData);

	fflush(outputFile);
	fclose(outputFile);
}

void outputMohawkStream(MohawkOutputStream output) {
	// No specified name, prepare our own
	if (output.name.empty()) {
		char *strBuf = (char *)malloc(256);
		sprintf(strBuf, "%s_%d", tag2str(output.tag), output.id);
		output.name = strBuf;
	}

	// Intercept the sound tags
	if (output.tag == ID_TWAV || output.tag == ID_MSND || output.tag == ID_SND) {
		convertSoundResource(output);
		return;
	}

	// Intercept the movie tag (need to change the offsets)
	// TODO: Actually convert. Just dump for now.
	if (output.tag == ID_TMOV) {
		convertMovieResource(output);
		//return;
	}

	// Intercept the MIDI tag (strip out Mohawk header/Prg# stuff)
	if (output.tag == ID_TMID) {
		convertMIDIResource(output);
		return;
	}

	// TODO: Convert other resources? PICT/WDIB/tBMP?

	assert(outputBuffer);

	printf ("Extracting \'%s\'...\n", output.name.c_str());

	FILE *outputFile = fopen(output.name.c_str(), "wb");
	if (!outputFile) {
		printf ("Could not open file for output!\n");
		return;
	}

	while (output.stream->pos() < output.stream->size()) {
		uint32 size = output.stream->read(outputBuffer, MAX_BUF_SIZE);
		fwrite(outputBuffer, 1, size, outputFile);
	}

	fflush(outputFile);
	fclose(outputFile);
}

int main(int argc, char *argv[]) {
	if (argc < 2 || argc == 3) {
		printUsage(argv[0]);
		return 1;
	}

	FILE *file = fopen(argv[1], "rb");
	if (!file) {
		printf ("Could not open \'%s\'\n", argv[1]);
		return 1;
	}

	// Open the file as a Mohawk archive
	MohawkFile *mohawkFile;
	if(Common::String(argv[0]).hasSuffix("old"))
		mohawkFile = new OldMohawkFile();
	else
		mohawkFile = new MohawkFile();
	mohawkFile->open(new Common::File(file));

	// Allocate a buffer for the output
	outputBuffer = (byte *)malloc(MAX_BUF_SIZE);

	if (argc > 3) {
		uint32 tag = READ_BE_UINT32(argv[2]);
		uint16 id = (uint16)atoi(argv[3]);

		MohawkOutputStream output = mohawkFile->getRawData(tag, id);

		if (output.stream) {
			outputMohawkStream(output);
			delete output.stream;
		} else {
			printf ("Could not find specified data!\n");
		}
	} else {
		MohawkOutputStream output = mohawkFile->getNextFile();
		while (output.stream) {
			outputMohawkStream(output);
			delete output.stream;
			output = mohawkFile->getNextFile();
		}
	}

	printf ("Done!\n");

	free(outputBuffer);

	return 0;
}
