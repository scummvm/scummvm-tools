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
#include "extract_cge.h"
#include "cge_structs.h"

#define BUFFER_SIZE 8192
#define SEED        0xA5

ExtractCge::ExtractCge(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input1;
	input1.format = "vol.cat";
	_inputPaths.push_back(input1);
	
	ToolInput input2;
	input2.format = "vol.dat";
	_inputPaths.push_back(input2);
		
	_outputToDirectory = true;

	_shorthelp = "Used to extract Soltys data files.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] /path/to/vol.dat /path/to/vol.cat\n";
}

void ExtractCge::execute() {
	unpack();
}

void ExtractCge::readData(Common::File &f, byte *buff, int size) {
	int bytesRead = f.read_noThrow(buff, size);
	for (int i = 0; i < bytesRead; ++i)
		buff[i] ^= SEED;
}

void ExtractCge::unpack() {
	print("Unpacking...\n");

	BtPage btPage;

	Common::File volCat, volDat;
	Common::Filename inPath1(_inputPaths[0].path);
	Common::Filename inPath2(_inputPaths[1].path);
	if (scumm_stricmp(inPath1.getName().c_str(), "vol") != 0 || scumm_stricmp(inPath2.getName().c_str(), "vol") != 0) {
		error("Wrong input file names.\n");
	}
	if (inPath1.hasExtension("cat") && inPath2.hasExtension("dat")) {
		volCat.open(inPath1, "rb");
		volDat.open(inPath2, "rb");
	} else if (inPath1.hasExtension("dat") && inPath2.hasExtension("cat")) {
		volCat.open(inPath2, "rb");
		volDat.open(inPath1, "rb");
	} else {
		error("Wrong input file names.\n");
	}

	if (!volCat.isOpen()) {
		error("Unable to open vol.cat\n");
	}

	if (!volDat.isOpen()) {
		error("Unable to open vol.dat\n");
	}

	// We always need to setup default output path, since there is no obligation to specify it
	if (_outputPath.empty())
		_outputPath.setFullPath("./");

	_outputPath.setFullName("files.txt");
	Common::File fFiles(_outputPath, "w");
	if (!fFiles.isOpen()) {
		error("Unable to create files.txt\n");
	}

	// Get in a list of pages individual files will be on
	readData(volCat, (byte *)&btPage, sizeof(BtPage));

	int pageList[1000];
	int pageCount = btPage._hea._count;
	pageList[0] = btPage._hea._down;
	for (int i = 0; i < pageCount; ++i)
		pageList[i + 1] = btPage._inn[i]._down;
	
	bool first = true;
	Common::File fOut;
	// Loop through the pages of individual files
	for (int i = 0; i <= pageCount; ++i) {
		// Move to correct page and read it
		volCat.seek(pageList[i] * sizeof(BtPage), SEEK_SET);
		readData(volCat, (byte *)&btPage, sizeof(BtPage));
		
		// Process the files
		for (unsigned int fileNum = 0; fileNum < btPage._hea._count; ++fileNum) {
			char fname[256];
			strcpy(fname, btPage._lea[fileNum]._key);
			
			// Add filename to files list
			if (!first)
				fFiles.print("\n%s", btPage._lea[fileNum]._key);
			else {
				fFiles.print("%s", btPage._lea[fileNum]._key);
				first = false;
			}

			_outputPath.setFullName(fname);
			fOut.open(_outputPath, "wb");
			byte *buffer = (byte *)malloc(btPage._lea[fileNum]._size);

			volDat.seek(btPage._lea[fileNum]._mark, SEEK_SET);
			readData(volDat, buffer, btPage._lea[fileNum]._size);
			fOut.write(buffer, btPage._lea[fileNum]._size);

			fOut.close();
			free(buffer);
		}
	}

	volCat.close();
	volDat.close();
	fFiles.close();
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractCge cge(argv[0]);
	return cge.run(argc, argv);
}
#endif


