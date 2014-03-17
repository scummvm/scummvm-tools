/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extract_cge.h"
#include "cge_structs.h"

#define SEED 0xA5

ExtractCge::ExtractCge(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "vol.*";
	_inputPaths.push_back(input);

	_outputToDirectory = true;

	_shorthelp = "Used to extract Soltys and Sfinx data files.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] <inputfile>\n";
}

void ExtractCge::execute() {
	Common::Filename filename = _inputPaths[0].path;

	filename.setFullName("vol.cat");
	_volCat.open(filename, "rb");
	if (!_volCat.isOpen()) {
		error("Unable to open vol.cat");
	}

	filename.setFullName("vol.dat");
	_volDat.open(filename, "rb");
	if (!_volDat.isOpen()) {
		_volCat.close();
		error("Unable to open vol.dat");
	}

	// We always need to setup default output path, since there is no obligation to specify it
	if (_outputPath.empty())
		_outputPath.setFullPath("./");

	_outputPath.setFullName("files.txt");
	_fFiles.open(_outputPath, "w");
	if (!_fFiles.isOpen()) {
		_volCat.close();
		_volDat.close();
		error("Unable to create files.txt");
	}

	if (!unpack()) {
		_volCat.seek(0, SEEK_SET);
		_volDat.seek(0, SEEK_SET);
		unpack2();
	}

	_volCat.close();
	_volDat.close();
	_fFiles.close();
}

InspectionMatch ExtractCge::inspectInput(const Common::Filename &filename) {
	// Accept either 'vol.cat' or 'vol.dat'
	std::string file = filename.getFullName();
	if (
		scumm_stricmp(file.c_str(), "vol.dat") == 0 ||
		scumm_stricmp(file.c_str(), "vol.cat") == 0
	)
		return IMATCH_PERFECT;
	return IMATCH_AWFUL;
}

void ExtractCge::readData(Common::File &f, byte *buff, int size) {
	int bytesRead = f.read_noThrow(buff, size);
	for (int i = 0; i < bytesRead; ++i)
		buff[i] ^= SEED;
}

bool ExtractCge::unpack() {
	BtPage btPage;

	// Get in a list of pages individual files will be on
	readData(_volCat, (byte *)&btPage, sizeof(BtPage));

	int pageList[1000];
	int pageCount = btPage._hea._count;
	if (pageCount > 998) {
		print("CAT/DAT file not detected as CGE1...");
		return false; // If it's a mess, we stop trying with CGE1.
	}
	pageList[0] = btPage._hea._down;
	for (int i = 0; i < pageCount; ++i)
		pageList[i + 1] = btPage._inn[i]._down;

	for (int i = 0; i <= pageCount; ++i) {
		// Move to correct page and read it
		_volCat.seek(pageList[i] * sizeof(BtPage), SEEK_SET);
		readData(_volCat, (byte *)&btPage, sizeof(BtPage));

		// Process the files
		for (unsigned int fileNum = 0; fileNum < btPage._hea._count; ++fileNum) {
			// If it's bigger than the upper limit of the array...
			if (fileNum > ((kBtPageSize - sizeof(CgeHea)) / sizeof(CgeBtKeypack) - 1)) {
				print("CAT/DAT file not detected as CGE1...");
				return false; // ...we stop trying with CGE1.
			}
		}
	}

 	// Otherwise it's all ok:
	print("Unpacking as CGE1...");
	_fFiles.print("CGE1");

	Common::File fOut;
	// Loop through the pages of individual files
	for (int i = 0; i <= pageCount; ++i) {
		// Move to correct page and read it
		_volCat.seek(pageList[i] * sizeof(BtPage), SEEK_SET);
		readData(_volCat, (byte *)&btPage, sizeof(BtPage));

		// Process the files
		for (unsigned int fileNum = 0; fileNum < btPage._hea._count; ++fileNum) {
			char fname[256];
			strcpy(fname, btPage._lea[fileNum]._key);

			// Add filename to files list
			_fFiles.print("\n%s", btPage._lea[fileNum]._key);

			_outputPath.setFullName(fname);
			fOut.open(_outputPath, "wb");
			byte *buffer = (byte *)malloc(btPage._lea[fileNum]._size);

			_volDat.seek(btPage._lea[fileNum]._mark, SEEK_SET);
			readData(_volDat, buffer, btPage._lea[fileNum]._size);
			fOut.write(buffer, btPage._lea[fileNum]._size);

			fOut.close();
			free(buffer);
		}
	}

	return true;
}

void ExtractCge::unpack2() {
	BtPage2 btPage2;

	print("Unpacking as CGE2...");
	// Get in a list of pages individual files will be on
	readData(_volCat, (byte *)&btPage2, sizeof(BtPage2));

	int pageList[1000];
	int pageCount = btPage2._hea._count;
	pageList[0] = btPage2._hea._down;
	for (int i = 0; i < pageCount; ++i)
		pageList[i + 1] = btPage2._inn[i]._down;

	Common::File fOut;
	_fFiles.print("CGE2");

	// Loop through the pages of individual files
	for (int i = 0; i <= pageCount; ++i) {
		// Move to correct page and read it
		_volCat.seek(pageList[i] * sizeof(BtPage2), SEEK_SET);
		readData(_volCat, (byte *)&btPage2, sizeof(BtPage2));

		// Process the files
		for (unsigned int fileNum = 0; fileNum < btPage2._hea._count; ++fileNum) {
			char fname[256];
			strcpy(fname, btPage2._lea[fileNum]._key);

			// Add filename to files list
			_fFiles.print("\n%s", fname);

			_outputPath.setFullName(fname);
			fOut.open(_outputPath, "wb");
			byte *buffer = (byte *)malloc(btPage2._lea[fileNum]._size);

			_volDat.seek(btPage2._lea[fileNum]._mark, SEEK_SET);
			readData(_volDat, buffer, btPage2._lea[fileNum]._size);
			fOut.write(buffer, btPage2._lea[fileNum]._size);

			fOut.close();
			free(buffer);
		}
	}
}
 
#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractCge cge(argv[0]);
	return cge.run(argc, argv);
}
#endif
