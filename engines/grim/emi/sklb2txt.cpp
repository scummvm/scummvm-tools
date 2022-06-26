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

// Based on Benjamin Haischs work on sklb-files.

#include <fstream>
#include <string>
#include <iostream>
#include "filetools.h"
#include "engines/grim/lab.h"

using namespace std;

int main(int argc, char **argv) {
	if (argc < 2) {
		std::cout << "Error: filename not specified" << std::endl;
		return 0;
	}

	Lab *lab = NULL;
	std::string filename;

	if (argc > 2) {
		lab = new Lab(argv[1]);
		filename = argv[2];
	} else {
		filename = argv[1];
	}

	std::istream *file = getFile(filename, lab);

	if (!file) {
		std::cout << "Unable to open file " << filename << std::endl;
		return 0;
	}
	int numBones = readInt(*file);

	char boneString[32];
	char parentString[32];

	float angle = 0;
	// Bones are listed in the same order as in the meshb.
	Vector3d *vec = 0;
	for (int i = 0; i < numBones; i++) {
		file->read((char *)&boneString, 32);
		file->read((char *)&parentString, 32);

		std::cout << "# BoneName " << boneString << "\twith parent: " << parentString << "\t";
		std::cout << " position: ";
		vec = readVector3d(*file);
		std::cout << vec->toString();
		delete vec;
		std::cout << " rotation: ";
		vec = readVector3d(*file);
		std::cout << vec->toString();
		delete vec;
		angle = readFloat(*file);
		std::cout << angle << std::endl;

	}
}
