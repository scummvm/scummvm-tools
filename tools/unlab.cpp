/* ResidualVM - A 3D game interpreter
 *
 * ResidualVM is the legal property of its developers, whose names
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

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <fstream>
#include "lab.h"

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#define GT_GRIM 1
#define GT_EMI 2

static void createDirectoryStructure(std::string name) {
#ifdef WIN32
	int pos = name.find_last_of("\\");
	if (pos != name.npos) {
		name.erase(pos);
		createDirectoryStructure(name);
		CreateDirectory(name.c_str(), NULL);
	}
#endif
}

std::string fixFilename(std::string filename) {
	int len = filename.size();
	for (int i = 0; i < len; i++) {
		if (filename[i] == '\\') {
			filename[i] = '/';
		}
		filename[i] = tolower(filename[i]);

	}

	return filename;
}

int main(int argc, char **argv) {
	if (argc < 2) {
		printf("No file specified\n");
		exit(1);
	}
	const char *filename = argv[1];

	Lab lab(filename);

	for (int i = 0; i < lab.getNumEntries(); i++) {
		std::string filename = lab.getFileName(i);
		std::istream *infile = lab.getFile(filename);
		filename = fixFilename(filename);
		createDirectoryStructure(filename.c_str());
		std::fstream outfile(filename.c_str(), std::ios::out | std::ios::binary);
		if (!outfile.is_open()) {
			printf("Could not open %s for writing\n", filename.c_str());
			createDirectoryStructure(filename.c_str());
			continue;
		}

		printf("Extracting file %s\n", filename.c_str());

		outfile << infile->rdbuf();

		delete infile;

	}

	return 0;
}
