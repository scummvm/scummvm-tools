/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

/* Mohawk file extractor */

#include "common/file.h"
#include "common/util.h"

#include "engines/mohawk/archive.h"
#include "engines/mohawk/utils.h"

#include <assert.h>

void dumpRawResource(MohawkOutputStream output) {
	// Change the extension to bin
	Common::Filename filename(output.name + ".bin");

	printf("Extracting \'%s\'...\n", filename.getName().c_str());

	if (filename.exists()) {
		printf ("File \'%s\' already exists!\n", filename.getName().c_str());
		return;
	}

	Common::File outputFile(filename, "wb");
	if (!outputFile.isOpen()) {
		printf ("Could not open file for output!\n");
		return;
	}

	copyBytes(output.stream, &outputFile, output.size);
}

void convertSoundResource(MohawkOutputStream output) {
	printf ("Converting sounds not yet supported. Dumping instead...\n");
	dumpRawResource(output);
}

void convertMovieResource(MohawkOutputStream output) {
	Common::Filename filename(output.name + ".bin");

	printf("Extracting '%s'...\n", filename.getName().c_str());

	if (filename.exists()) {
		printf ("File '%s' already exists!\n", filename.getName().c_str());
		return;
	}

	Common::File outputFile(filename, "wb");
	if (!outputFile.isOpen()) {
		printf ("Could not open file '%s' for output!\n", filename.getName().c_str());
		return;
	}

	adjustQuickTimeAtomOffsets(output.stream, output.size, -output.offset, &outputFile);
}

void convertMIDIResource(MohawkOutputStream output) {
	// Change the extension to midi
	Common::Filename filename(output.name + ".mid");

	printf("Extracting '%s'...\n", filename.getName().c_str());

	if (filename.exists()) {
		printf ("File '%s' already exists!\n", filename.getName().c_str());
		return;
	}

	Common::File outputFile(filename, "wb");
	if (!outputFile.isOpen()) {
		printf ("Could not open file '%s' for output!\n", filename.getName().c_str());
		return;
	}

	// Read the Mohawk MIDI header
	assert(output.stream->readUint32BE() == ID_MHWK);
	output.stream->readUint32BE(); // Skip size
	assert(output.stream->readUint32BE() == ID_MIDI);

	uint32 size = output.stream->size() - 12; // Skip MHWK header's size

	byte *midiData = (byte *)malloc(size);

	// Read the MThd Data
	output.stream->read_noThrow(midiData, 14);

	// Skip the unknown Prg# section
	assert(output.stream->readUint32BE() == ID_PRG);
	output.stream->seek(output.stream->readUint32BE(), SEEK_CUR);

	// Read the MTrk Data
	uint32 mtrkSize = output.stream->size() - output.stream->pos();
	output.stream->read_noThrow(midiData + 14, mtrkSize);

	// Output the data to the file.
	outputFile.write(midiData, 14 + mtrkSize);
	free(midiData);
}

void outputMohawkStream(MohawkOutputStream output, bool doConversion, bool fileTableIndex, bool fileTableFlags) {
	// File output naming format preserves all archive information...
	char *strBuf = (char *)malloc(256);
	strBuf[0] = '\0';
	if (fileTableIndex)
		sprintf(strBuf + strlen(strBuf), "%04d_", output.index);
	if (fileTableFlags)
		sprintf(strBuf + strlen(strBuf), "%02x_", output.flags);
	sprintf(strBuf + strlen(strBuf), "%s_%d", tag2str(output.tag), output.id);
	if (!output.name.empty()) {
		for (uint i = 0; i < output.name.size(); i++) {
			//printf("DEBUG: i: %d output.name[i]: %c\n", i, output.name[i]);
			if (output.name[i] == '\\') {
				//printf("\tPadded \ at %d with %c\n", i, '\\');
				output.name.insertChar('\\', i);
				i++;
			}
			if (output.name[i] == '/') {
				//printf("\tReplaced / at %d with %c\n", i, '\\');
				output.name.setChar('\\', i);
			}
		}
		sprintf(strBuf + strlen(strBuf), "_%s", output.name.c_str());
	}
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
	printf("\n");
	printf("Options : --raw        Dump Resources as raw binary dump (default)\n");
	printf("          --convert    Dump Resources as converted files\n");
	printf("\n");
	printf("          --ftindex    Prepend File Table Index to dumped resource names (default)\n");
	printf("          --no-ftindex Omit File Table Index from dumped resource names\n");
	printf("          --ftflags    Prepend File Table Flags to dumped resource names (default)\n");
	printf("          --no-ftflags Omit File Table Flags from dumped resource names\n");
}

int main(int argc, char *argv[]) {
	// Defaults for options
	bool doConversion = false;
	bool fileTableIndex = true;
	bool fileTableFlags = true;

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
		else if (current.equals("--no-ftindex"))
			fileTableIndex = false;
		else if (current.equals("--ftindex"))
			fileTableIndex = true;
		else if (current.equals("--no-ftflags"))
			fileTableFlags = false;
		else if (current.equals("--no-ftflags"))
			fileTableFlags = true;
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

	Common::File file(argv[archiveArg], "rb");
	if (!file.isOpen()) {
		printf ("Could not open \'%s\'\n", argv[archiveArg]);
		return 1;
	}

	// Open the file as a Mohawk archive
	MohawkArchive *mohawkArchive = MohawkArchive::createMohawkArchive(&file);

	if (!mohawkArchive) {
		printf("\'%s\' is not a valid Mohawk archive\n", argv[archiveArg]);
		return 1;
	}

	if (argc == archiveArg - 2 - 1) {
		uint32 tag = READ_BE_UINT32(argv[archiveArg + 1]);
		uint16 id = (uint16)atoi(argv[archiveArg + 2]);

		MohawkOutputStream output = mohawkArchive->getRawData(tag, id);

		if (output.stream) {
			outputMohawkStream(output, doConversion, fileTableIndex, fileTableFlags);
		} else {
			printf ("Could not find specified data!\n");
		}
	} else {
		MohawkOutputStream output = mohawkArchive->getNextFile();
		while (output.stream) {
			outputMohawkStream(output, doConversion, fileTableIndex, fileTableFlags);
			output = mohawkArchive->getNextFile();
		}
	}

	printf("Done!\n");
	mohawkArchive->close();
	delete mohawkArchive;
	return 0;
}
