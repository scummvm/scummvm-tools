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

/* TwinE Script disassembler */

#include <stdio.h>
#include <string.h>

#include "common/util.h"
#include "common/memstream.h"
#include "engines/twine/hqr.h"
#include "engines/twine/lba1.h"
#include "engines/twine/lba2.h"

static void printHelp(const char *bin) {
	printf("Usage: %s <variant> <index> <scene.hqr>\n\n", bin);
	printf("The disassembled script will be written to stdout.\n\n");
	printf("Supported variants:\n");
	printf("	lba1      - Little Big Adventure 1\n");
	printf("	lba2      - Little Big Adventure 2\n");
	printf("\n");
}

static int getVariant(const char *verStr) {
	if (!scumm_stricmp(verStr, "lba1")) {
		return 1;
	} else if (!scumm_stricmp(verStr, "lba2")) {
		return 2;
	}
	return -1;
}

int main(int argc, char **argv) {
	if ((argc < 3) || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {
		printHelp(argv[0]);
		return -1;
	}

	int variant = getVariant(argv[1]);
	if (variant == -1) {
		printHelp(argv[0]);
		return -1;
	}

	int index = atoi(argv[2]);
	const char *sceneHqr = "scene.hqr";
	if (argc >= 4) {
		sceneHqr = argv[3];
	}
	const Common::Filename fn(sceneHqr);
	uint8 *data = nullptr;
	int size = 0;
	if (fn.exists()) {
		size = TwinE::HQR::getAllocEntry(&data, fn, index);
	}
	if (data == nullptr || size == 0) {
		fprintf(stderr, "Failed to load index %i from %s", index, fn.getFullName().c_str());
		return 127;
	}

	printf("Scene %i\n", index);
	if (variant == 1) {
		return decompileLBA1(data, size);
	}
	return decompileLBA2(data, size);
}
