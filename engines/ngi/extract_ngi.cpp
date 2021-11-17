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
 * This is a utility to unpack Nikita engine's archive files.
 */

#include "common/scummsys.h"
#include "common/endian.h"
#include "common/file.h"

static void unpackFile(Common::File &fp, const char *outDir) {
	char filePath[512];

	fp.seek(4, SEEK_SET);

	unsigned int count = fp.readUint16LE(); // How many entries?

	fp.seek(20, SEEK_SET);

	unsigned int key = fp.readUint16LE();

	byte key1, key2;

	key1 = key & 0xff;
	key2 = (key >> 8) & 0xff;

	printf("count: %d\n", count);

	int fatSize = count * 32;

	fp.seek(32, SEEK_SET);

	byte *o = (byte *)calloc(fatSize, 1);

	fp.read_noThrow(o, fatSize);

	for (int i = 0; i < fatSize; i++) {
		key1 = (key1 << 1) ^ key2;
		key2 = (key2 >> 1) ^ key1;

		o[i] ^= key1;
	}

	char fname[13];
	int flags, extVal, offset, size;
	for (uint i = 0; i < count; i++) {
		memcpy(fname, &o[i * 32], 12);
		fname[12] = 0;
		flags = READ_LE_UINT32(&o[i * 32 + 16]);
		extVal = READ_LE_UINT32(&o[i * 32 + 20]);
		offset = READ_LE_UINT32(&o[i * 32 + 24]);
		size = READ_LE_UINT32(&o[i * 32 + 28]);
		printf("[%s]: %.8x %.8x %.8x %.8x\n", fname, flags, extVal, offset, size);

		if (flags & 0x1e0) {
			printf("File has flags: %.8x\n", flags & 0x1e0);
		}

		sprintf(filePath, "%s/%s", outDir, fname);
		Common::File out;

		out.open(filePath, "wb");

		byte *dest = (byte *)malloc(size);

		fp.seek(offset, SEEK_SET);
		fp.read_noThrow(dest, size);
		out.write(dest, size);
		free(dest);

		out.close();
	}
}

int showUsage() {
	printf("USAGE: extract_ngi [input file] [output directory]\n");
	return -1;
}

int main(int argc, char *argv[]) {
	int i;
	char tmp[512];

	if (argc == 3) {
		strcpy(tmp, argv[1]);
		for (i = 0; tmp[i] != 0; i++) {
			tmp[i] = toupper(tmp[i]);
		}

		Common::File fp;

		fp.open(argv[1], "rb");
		if (fp.isOpen()) {
			unpackFile(fp, argv[2]);
			fp.close();
		} else {
			printf("Couldn't open input file '%s'\n", argv[1]);
			return -1;
		}
	} else {
		return showUsage();
	}
	return 0;
}
