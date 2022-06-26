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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pack_cge.h"

#define SEED 0xA5

PackCge::PackCge(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION/*TOOLTYPE_UNKNOWN*/) {
	ToolInput input;
	input.format = "/";
	input.file = false;
	_inputPaths.push_back(input);

	_outputToDirectory = true;

	_shorthelp = "Used to repackage Soltys and Sfinx data files.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] /path/to/one_game_file\n";

	for (uint i = 0; i < MAX_FILES; i++)
		memset(_files[i], 0, kBtKeySize);
}

void PackCge::execute() {
	// Load in the list of _files to recompress
	inPath.setFullPath(_inputPaths[0].path);
	inPath.setFullName("files.txt");
	_fIn.open(inPath, "r");
	if (!_fIn.isOpen()) {
		error("Unable to open %s", inPath.getFullPath().c_str());
	}

	// Define the version and the depending size of a leaf in the tree.
	_leaSize = 0;
	int version = 0;
	char versionLine[kBtKeySize];
	_fIn.scanString(&versionLine[0]);
	std::string sversion(versionLine);
	std::string verHeader("CGE2");
	if (sversion.compare(verHeader) == 0) {
		version = 2;
		_leaSize = (kBtPageSize2 - sizeof(CgeHea)) / sizeof(CgeBtKeypack2);
	} else {
		version = 1;
		_leaSize = (kBtPageSize - sizeof(CgeHea)) / sizeof(CgeBtKeypack);

		// Handle the old CGE1 format:
		verHeader = std::string("CGE1");
		if (sversion.compare(verHeader) != 0)
			_fIn.seek(0, SEEK_SET); // So we have to rewind the file, since there's no header.
	}

	_fileCount = 0;
	while (!_fIn.eos()) {
		_fIn.scanString(&_files[_fileCount++][0]);
		if (_fileCount == MAX_FILES) {
			_fIn.close();
			error("Max files reached");
		}
	}
	_fIn.close();

	// Open vol cat and dat files for writing
	if (_outputPath.empty())
		_outputPath.setFullPath("./");
	_outputPath.setFullName("vol.cat");
	_volCat.open(_outputPath, "wb");
	if (!_volCat.isOpen()) {
		error("Unable to create %s", _outputPath.getFullPath().c_str());
	}
	_outputPath.setFullName("vol.dat");
	_volDat.open(_outputPath, "wb");
	if (!_volDat.isOpen()) {
		_volCat.close();
		error("Unable to create %s", _outputPath.getFullPath().c_str());
	}

	switch (version) {
	case 1:
		pack();
		break;
	case 2:
		pack2();
		break;
	default:
		break;
	}

	_volCat.close();
	_volDat.close();
}

InspectionMatch PackCge::inspectInput(const Common::Filename &filename) {
	// Check that this is a directory
	if (!filename.directory())
		return IMATCH_AWFUL;

	// Check that it contains a files.txt file
	Common::Filename file = filename;
	file.setFullName("files.txt");
	if (file.exists())
		return IMATCH_PERFECT;
	return IMATCH_AWFUL;
}

void PackCge::writeData(Common::File &f, byte *buff, int size) {
	for (int i = 0; i < size; ++i)
		buff[i] ^= SEED;
	f.write(buff, size);
}

void PackCge::pack() {
	BtPage btPage;
	print("Packing as CGE1...");

	/* Build the index page */
	// Header
	memset(&btPage, 0, sizeof(BtPage));
	int pageCount = _fileCount / _leaSize;
	btPage._hea._count = pageCount;
	btPage._hea._down = 1;

	// Innert file list - lists the first file of the next page
	for (int pageNum = 0; pageNum < pageCount; ++pageNum) {
		int nextFile = (pageNum + 1) * _leaSize;

		btPage._inn[pageNum]._down = pageNum + 2;
		strcpy((char *)&btPage._inn[pageNum]._key[0], _files[nextFile]);
	}

	// Write out the index page
	writeData(_volCat, (byte *)&btPage, sizeof(BtPage));

	// Loop through processing each page and the dat file
	pageCount = (_fileCount + _leaSize - 1) / _leaSize;
	int fileIndex = 0;
	for (int pageNum = 0; pageNum < pageCount; ++pageNum) {
		int startFile = pageNum * _leaSize;
		int lastFile = (pageNum + 1) * _leaSize - 1;
		if (lastFile >= _fileCount)
			lastFile = _fileCount - 1;

		// Header
		memset(&btPage, 0, sizeof(BtPage));
		btPage._hea._count = lastFile - startFile + 1;
		btPage._hea._down = 0xffff;

		for (int fileNum = 0; fileNum < btPage._hea._count; ++fileNum, ++fileIndex) {
			// Set filename and offset in dat file
			strcpy(btPage._lea[fileNum]._key, &_files[fileIndex][0]);
			btPage._lea[fileNum]._mark = _volDat.pos();

			// Load the given file and write it into the dat file
			char fname[32];
			strcpy(fname, _files[fileIndex]);

			// Open the file and get the size
			inPath.setFullName(fname);
			_fIn.open(inPath, "rb");
			if (!_fIn.isOpen()) {
				error("Error opening %s", inPath.getFullPath().c_str());
			}
			int _filesize = _fIn.size();
			_fIn.seek(0, SEEK_SET);
			btPage._lea[fileNum]._size = _filesize;

			// Allocate buffer space for the file
			byte *buffer = (byte *)malloc(_filesize);

			// Read it in, encrypt it, and write it out
			_fIn.read_noThrow(buffer, _filesize);
			writeData(_volDat, buffer, _filesize);

			free(buffer);
			_fIn.close();
		}

		// Write out the page
		writeData(_volCat, (byte *)&btPage, sizeof(BtPage));
	}
}

void PackCge::pack2() {
	BtPage2 btPage2;
	print("Packing as CGE2...");

	/* Build the index page */
	// Header
	memset(&btPage2, 0, sizeof(BtPage2));
	int pageCount = _fileCount / _leaSize;
	btPage2._hea._count = pageCount;
	btPage2._hea._down = 1;

	// Innert file list - lists the first file of the next page
	for (int pageNum = 0; pageNum < pageCount; ++pageNum) {
		int nextFile = (pageNum + 1) * _leaSize;

		btPage2._inn[pageNum]._down = pageNum + 2;
		strcpy((char *)&btPage2._inn[pageNum]._key[0], _files[nextFile]);
	}

	// Write out the index page
	writeData(_volCat, (byte *)&btPage2, sizeof(BtPage2));

	// Loop through processing each page and the dat file
	pageCount = (_fileCount + _leaSize - 1) / _leaSize;
	int fileIndex = 0;
	for (int pageNum = 0; pageNum < pageCount; ++pageNum) {
		int startFile = pageNum * _leaSize;
		int lastFile = (pageNum + 1) * _leaSize - 1;
		if (lastFile >= _fileCount)
			lastFile = _fileCount - 1;

		// Header
		memset(&btPage2, 0, sizeof(BtPage2));
		btPage2._hea._count = lastFile - startFile + 1;
		btPage2._hea._down = 0xffff;

		for (int fileNum = 0; fileNum < btPage2._hea._count; ++fileNum, ++fileIndex) {
			// Set filename and offset in dat file
			strcpy(btPage2._lea[fileNum]._key, &_files[fileIndex][0]);
			btPage2._lea[fileNum]._mark = _volDat.pos();

			// Load the given file and write it into the dat file
			char fname[32];
			strcpy(fname, _files[fileIndex]);

			// Open the file and get the size
			inPath.setFullName(fname);
			_fIn.open(inPath, "rb");
			if (!_fIn.isOpen()) {
				error("Error opening %s", inPath.getFullPath().c_str());
			}
			int _filesize = _fIn.size();
			_fIn.seek(0, SEEK_SET);
			btPage2._lea[fileNum]._size = _filesize;

			// Allocate buffer space for the file
			byte *buffer = (byte *)malloc(_filesize);

			// Read it in, encrypt it, and write it out
			_fIn.read_noThrow(buffer, _filesize);
			writeData(_volDat, buffer, _filesize);

			free(buffer);
			_fIn.close();
		}

		// Write out the page
		writeData(_volCat, (byte *)&btPage2, sizeof(BtPage2));
	}
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	PackCge cge(argv[0]);
	return cge.run(argc, argv);
}
#endif
