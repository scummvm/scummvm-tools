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

/* add creator and file type attributes to pegasus save files */
#include "common/scummsys.h"

#include <stdio.h>
#include <CoreServices/CoreServices.h>

#include "common/endian.h"
#include "common/file.h"
#include "common/str.h"

static const uint32 kPegasusCreator = MKID_BE('JPPP');
static const uint32 kPegasusDisc1 = MKID_BE('PPG1');
static const uint32 kPegasusDisc2 = MKID_BE('PPG2');
static const uint32 kPegasusDisc3 = MKID_BE('PPG3');
static const uint32 kPegasusDisc4 = MKID_BE('PPG4');

int main(int argc, char **argv) {
	if (argc < 2) {
		printf("Usage: %s <save file>\n", argv[0]);
		printf("Applies the creator and type codes so the save can be used\n");
		printf("by the original interpreter.\n");
		return 0;
	}

	uint32 creator, type;

	// First, let's open our file to make sure it's a pegasus save
	// Also, we're going to extract what type we need to set (for the correct disc)
	try {
		Common::File file(argv[1], "rb");

		// Let's make sure the user already gunzip'd the file
		uint16 gzipTest = file.readUint16BE();
		if (gzipTest == 0x1F8B) {
			printf("Please gunzip the save file first\n");
			return 1;
		}

		creator = (gzipTest << 16) | file.readUint16BE(); // Too lazy to seek back :P
		type = file.readUint32BE();

		if (creator != kPegasusCreator) {
			printf("Failed to find pegasus creator in save file\n");
			return 1;
		}

		if (type != kPegasusDisc1 && type != kPegasusDisc2 && type != kPegasusDisc3 && type != kPegasusDisc4) {
			printf("Invalid pegasus save type in save file\n");
			return 1;
		}
	} catch (Common::FileException e) {
		printf("File error: '%s'\n", e.what());
		return 1;
	}

	FSCatalogInfo catInfo;
	FSRef ref;

	if (!FSPathMakeRef((const UInt8 *)argv[1], &ref, 0)) {
		FSGetCatalogInfo(&ref, kFSCatInfoFinderInfo, &catInfo, 0, 0, 0);
		FileInfo *info = (FileInfo *)catInfo.finderInfo;
		info->fileCreator = creator;
		info->fileType = type;
		FSSetCatalogInfo(&ref, kFSCatInfoFinderInfo, &catInfo);
	} else {
		printf("Failed to use File Manager to open the file\n");
		return 1;
	}

	printf("Success!\n");
	return 0;
}
