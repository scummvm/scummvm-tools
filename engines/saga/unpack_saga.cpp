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
#include <string.h>
#include <stdio.h>

#include "common/file.h"
#include "common/str.h"
#include "common/util.h"

#define RSC_TABLEINFO_SIZE 8
#define RSC_TABLEENTRY_SIZE 8

typedef struct  {
	uint32 offset;
	uint32 size;
} Record;

void unpack(Common::String inputName, Common::String outDirName, bool isBe = false) {
	Common::File inputFile;
	Common::File outputFile;
	uint32 inputFileSize;
	uint32 resTableOffset;
	uint32 resTableCount;
	uint32 i;

	Record *inputTable;

	inputFile.open(inputName, "rb");
	inputFileSize = inputFile.size();
	fprintf(stderr, "Filesize: %ul\n", inputFileSize);
	/*
	 * At the end of the resource file there are 2 values: one points to the
	 * beginning of the resource table the other gives the number of
	 * records in the table
	 */
	inputFile.seek(inputFileSize - RSC_TABLEINFO_SIZE, SEEK_SET);

	if (!isBe) {
		resTableOffset = inputFile.readUint32LE();
		resTableCount = inputFile.readUint32LE();
	} else {
		resTableOffset = inputFile.readUint32BE();
		resTableCount = inputFile.readUint32BE();
	}

	fprintf(stderr, "Table offset: %ul\nnumber of records: %ul\n", resTableOffset, resTableCount);
	if (resTableOffset != inputFileSize - RSC_TABLEINFO_SIZE - RSC_TABLEENTRY_SIZE * resTableCount) {
		error("Something's wrong with your resource file");
	}

	// Go to beginning of the table
	inputFile.seek(resTableOffset, SEEK_SET);

	inputTable = (Record*)malloc(resTableCount * sizeof(Record));

	// Put offsets of all the records in a table
	for (i = 0; i < resTableCount; i++) {

		if (!isBe) {
			inputTable[i].offset = inputFile.readUint32LE();
			inputTable[i].size = inputFile.readUint32LE();
		} else {
			inputTable[i].offset = inputFile.readUint32BE();
			inputTable[i].size = inputFile.readUint32BE();
		}

		fprintf(stderr, "Record: %ul, offset: %ul, size: %ul\n", i, inputTable[i].offset, inputTable[i].size);

		if ((inputTable[i].offset > inputFileSize) ||
			(inputTable[i].offset + inputTable[i].size > inputFileSize)) {
			error("The offset points outside the file");
		}

	}

	for (i = 0; i < resTableCount; i++) {
		outputFile.open(Common::String::format("%s/%d.bin", outDirName.c_str(), i), "wb");
		inputFile.seek(inputTable[i].offset, SEEK_SET);
		byte *buf = new byte[inputTable[i].size];
		inputFile.read_throwsOnError(buf, inputTable[i].size);
		outputFile.write(buf, inputTable[i].size);
		delete[]buf;
	}
	inputFile.close();

	fprintf(stderr, "Done!\n");
}

int main(int argc, char *argv[]) {
	unpack(argv[1], argv[2]);
}


