/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pack_cge.h"
#include "cge_structs.h"

#define SEED        0xA5
#define MAX_FILES   5000

// TODO:
//   - Use output directory instead of current directory
//   - Use input directory instead of current directory

PackCge::PackCge(const std::string &name) : Tool(name, TOOLTYPE_UNKNOWN) {
	ToolInput input;
	input.format = "/";
	input.file = false;
	_inputPaths.push_back(input);
	
	_outputToDirectory = true;

	_shorthelp = "Used to repackage Soltys data files.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] /path/to/one_game_file\n";
}

void PackCge::execute() {
	pack();
}


void PackCge::writeData(Common::File &f, byte *buff, int size) {
	for (int i = 0; i < size; ++i)
		buff[i] ^= SEED;
	f.write(buff, size);
}

void PackCge::pack() {
	BtPage btPage;
	print("Packing...\n");
	
	Common::Filename inPath(_inputPaths[0].path);

	// Load in the list of files to recompress
	char files[MAX_FILES][kBtKeySize];
	int fileCount = 0;
	inPath.setFullName("files.txt");
	Common::File fIn(inPath, "r");
	if (!fIn.isOpen()) {
		error("Unable to open %s\n", inPath.getFullPath().c_str());
	}

	while (!fIn.eos()) {
		fIn.scanString(&files[fileCount++][0]);
		if (fileCount == MAX_FILES) {
			error("Max files reached");
		}
	}
	fIn.close();

	// Open vol cat and dat files for writing
	if (_outputPath.empty())
		_outputPath.setFullPath("./");
	_outputPath.setFullName("vol.cat");
	Common::File volCat(_outputPath, "wb");
	if (!volCat.isOpen()) {
		error("Unable to create %s\n", _outputPath.getFullPath().c_str());
	}
	_outputPath.setFullName("vol.dat");
	Common::File volDat(_outputPath, "wb");
	if (!volDat.isOpen()) {
		error("Unable to create %s\n", _outputPath.getFullPath().c_str());
	}

	/* Build the index page */
	// Header
	memset(&btPage, 0, sizeof(BtPage));
	int pageCount = fileCount / CGE_LEA_SIZE;
	btPage._hea._count = pageCount;
	btPage._hea._down = 1;

	// Innert file list - lists the first file of the next page
	for (int pageNum = 0; pageNum < pageCount; ++pageNum) {
		int nextFile = (pageNum + 1) * CGE_LEA_SIZE;

		btPage._inn[pageNum]._down = pageNum + 2;
		strcpy((char *)&btPage._inn[pageNum]._key[0], files[nextFile]);
	}

	// Write out the index page
	writeData(volCat, (byte *)&btPage, sizeof(BtPage));

	// Loop through processing each page and the dat file
	pageCount = (fileCount + CGE_LEA_SIZE - 1) / CGE_LEA_SIZE;
	int fileIndex = 0;
	for (int pageNum = 0; pageNum < pageCount; ++pageNum) {
		int startFile = pageNum * CGE_LEA_SIZE;
		int lastFile = (pageNum + 1) * CGE_LEA_SIZE - 1;
		if (lastFile >= fileCount)
			lastFile = fileCount - 1;

		// Header
		memset(&btPage, 0, sizeof(BtPage));
		btPage._hea._count = lastFile - startFile + 1;
		btPage._hea._down = 0xffff;

		for (int fileNum = 0; fileNum < btPage._hea._count; ++fileNum, ++fileIndex) {
			// Set filename and offset in dat file
			strcpy(btPage._lea[fileNum]._key, &files[fileIndex][0]);
			btPage._lea[fileNum]._mark = volDat.pos();

			// Load the given file and write it into the dat file
			char fname[32];
			strcpy(fname, files[fileIndex]);

			// Open the file and get the size
			inPath.setFullName(fname);
			fIn.open(inPath, "rb");
			if (!fIn.isOpen()) {
				error("Error opening %s\n", inPath.getFullPath().c_str());
			}
			int fileSize = fIn.size();
			fIn.seek(0, SEEK_SET);
			btPage._lea[fileNum]._size = fileSize;

			// Allocate buffer space for the file
			byte *buffer = (byte *)malloc(fileSize);

			// Read it in, encrypt it, and write it out
			fIn.read_noThrow(buffer, fileSize);
			writeData(volDat, buffer, fileSize);

			free(buffer);
			fIn.close();
		}

		// Write out the page
		writeData(volCat, (byte *)&btPage, sizeof(BtPage));
	}

	volCat.close();
	volDat.close();
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	PackCge cge(argv[0]);
	return cge.run(argc, argv);
}
#endif


