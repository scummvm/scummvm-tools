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

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <fstream>
#include "lab.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/stat.h>
#endif

#define GT_GRIM 1
#define GT_EMI 2

static void createDirectoryStructure(std::string name) {
#ifdef _WIN32
	size_t pos = name.find_last_of("\\");
	if (pos != name.npos) {
		name.erase(pos);
		createDirectoryStructure(name);
		CreateDirectory(name.c_str(), NULL);
	}
#else
	size_t pos = name.find_last_of("/");
	if (pos != name.npos) {
		name.erase(pos);
		createDirectoryStructure(name);
		mkdir(name.c_str(), 0755);
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
		std::string fname = lab.getFileName(i);
		std::istream *infile = lab.getFile(fname);
		fname = fixFilename(fname);
		createDirectoryStructure(fname.c_str());
		std::fstream outfile(fname.c_str(), std::ios::out | std::ios::binary);
		if (!outfile.is_open()) {
			printf("Could not open %s for writing\n", fname.c_str());
			createDirectoryStructure(fname.c_str());
			continue;
		}

		printf("Extracting file %s\n", fname.c_str());

		outfile << infile->rdbuf();

		delete infile;

	}

	return 0;
}
