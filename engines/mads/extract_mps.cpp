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

#include "common/endian.h"
#include "common/memstream.h"
#include "common/str.h"
#include "common/util.h"
#include "common/dcl.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

struct FileDescriptorBin {
	char name[0x52]; // zero-terminated, rest is filled with what looks like garbage
	uint16 compression;
	uint16 volume;
	uint32 offset_in_volume;
	uint32 compressedSize;
	uint32 uncompressedSize;
} __attribute__ ((packed));

int main (int argc, char **argv) {
	unsigned char * buf;
	size_t indexSize;
	FILE *fin;

	if (argc < 3) {
		fprintf (stderr, "USAGE: %s BASENAME OUTDIR\n", argv[0]);
		return -1;
	}

	Common::String base = argv[1];

	fin = fopen ((base + ".IDX").c_str(), "rb");
	if (fin == NULL) {
		fprintf (stderr, "Unable to open %s: %s\n", argv[1], strerror(errno));
		return -2;
	}
	fseek (fin, 0, SEEK_END);
	indexSize = ftell (fin);
	fseek (fin, 0, SEEK_SET);

	buf = (byte *) malloc (indexSize);

	fread (buf, 1, indexSize, fin);
	fclose (fin);

	int filecnt = READ_LE_UINT16(buf);

	byte *ptr;
	int i;

	hexdump(buf + 2, 10);

	if (filecnt > (indexSize - 12) / sizeof(FileDescriptorBin))
		filecnt = (indexSize - 12) / sizeof(FileDescriptorBin);

	for (ptr = buf + 12, i = 0; i < filecnt; ptr += sizeof(FileDescriptorBin), i++) {
		FileDescriptorBin *descBin = (FileDescriptorBin *) ptr;
		uint32 compressedSize = FROM_LE_32(descBin->compressedSize);
		uint32 uncompressedSize = FROM_LE_32(descBin->uncompressedSize);
		uint16 compressionAlgo = FROM_LE_16(descBin->compression);
		printf("name: %s, compressed=%d, uncompressed=%d, compression=%d, offset=%x, volume = %03d\n",
		       descBin->name, compressedSize,
		       uncompressedSize, compressionAlgo, FROM_LE_32(descBin->offset_in_volume), FROM_LE_16(descBin->volume));
		if (compressionAlgo != 0 && compressionAlgo != 1) {
			fprintf (stderr, "Unsupported compression alorithm for %s, skipping\n", descBin->name);
			continue;
		}
		Common::String fn = Common::String::format("%s/%s", argv[2], descBin->name);
		uint32 off = FROM_LE_32(descBin->offset_in_volume);
		int vol = FROM_LE_16(descBin->volume);
		byte *compressedBuf = new byte[compressedSize];
		byte *outptr = compressedBuf;
		uint32 rem = compressedSize;
		while (rem > 0) {
			FILE *fvol = fopen((base + Common::String::format(".%03d", vol)).c_str(), "rb");
			fseek(fvol, off, SEEK_SET);
			int actual = fread(outptr, 1, rem, fvol);
			fclose(fvol);
			rem -= actual;
			outptr += actual;
			if (actual == 0)
				break;
			vol++;
			off = 0;
		}
		byte *uncompressedBuf;
		switch (compressionAlgo) {
		case 0:
			uncompressedBuf = compressedBuf;
			uncompressedSize = compressedSize;
			break;
		case 1:
			Common::MemoryReadStream compressedReadStream(compressedBuf, compressedSize);
			uncompressedBuf = new byte[uncompressedSize];
			if (!Common::decompressDCL(&compressedReadStream, uncompressedBuf, compressedSize, uncompressedSize)) {
				fprintf (stderr, "Unable to decompress %s\n", descBin->name);
				continue;
			}
				
			break;
		}
		FILE *fout;
		fout = fopen(fn.c_str(), "wb");
		if (fout == NULL) {
			fprintf (stderr, "Unable to open %s: %s\n", fn.c_str(), strerror(errno));
			return -3;
		}
		fwrite(uncompressedBuf, 1, uncompressedSize, fout);
		fclose(fout);
		if (uncompressedBuf != compressedBuf)
			delete[] uncompressedBuf;
		delete[]compressedBuf;
	}

	return 0;
}
