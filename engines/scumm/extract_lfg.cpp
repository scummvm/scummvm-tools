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
#include "common/file.h"
#include "common/str.h"
#include "common/util.h"
#include "common/dcl.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main (int argc, char **argv) {
	unsigned char *buf = nullptr;
	size_t inSize = 0;

	if (argc < 3) {
		fprintf (stderr, "USAGE: %s OUTDIR VOLUMES\n", argv[0]);
		return -1;
	}

	for (int i = 2; i < argc; i++) {
		FILE *fin = fopen (argv[i], "rb");
		byte head[8];
		if (fin == NULL) {
			fprintf (stderr, "Unable to open %s: %s\n", argv[i], strerror(errno));
			free (buf);
			return -2;
		}
		fread (head, 1, 8, fin);
		if (READ_BE_UINT32(head) != MKTAG('L', 'F', 'G', '!')) {
			fprintf (stderr, "%s doesn't contain magic\n", argv[i]);
			free (buf);
			return -2;
		}
		uint32 volumeSize = READ_LE_UINT32(head + 4);
		uint32 oldInSize = inSize;
		inSize += volumeSize;

		buf = (byte *) realloc (buf, inSize);
		fread (buf + oldInSize, 1, volumeSize, fin);
		fclose (fin);
	}

	printf("Archive name: %s\n", buf);
	printf("Value A=0x%x\n", buf[13]);
	printf("Number of volumes=%d\n", READ_LE_UINT16(buf+14));
	printf("Total number of bytes=%d\n", READ_LE_UINT32(buf+16));

	for (byte *ptr = buf + 0x14; ptr < buf + inSize;) {
		uint32 magic = READ_BE_UINT32(ptr);
		if (magic != MKTAG('F', 'I', 'L', 'E')) {
			printf("Wrong magic: %x @ %x\n", magic, (int)(ptr - buf));
			free (buf);
			return 0;
		}
		uint32 blocklen = READ_LE_UINT32(ptr + 4);
		byte *blockend = ptr + 8 + blocklen;
		ptr += 8;
		Common::String name((char *)ptr, 13);
		ptr += 13;
		Common::File fout(Common::String::format("%s/%s", argv[1], name.c_str()), "wb");
		uint32 uncompressedSize = READ_LE_UINT32(ptr + 1);
		printf("name: %s\n", name.c_str());
		printf("value B=0x%x\n", ptr[0]);
		// ptr[0] seems to be const in the same archive. So far:
		// 0x13: indy4 demo
		// 0x17: indy4 full
		// ptr + 5 is always 02 00 01 00 00 00 so far
		printf("value C:\n");
		hexdump(ptr + 5, 6);
		// Most likely it contains compression somewhere
		ptr += 11;

		uint32 compressedSize = blockend - ptr;
		byte *uncompressedBuf = nullptr;

		// DCL. TODO: Support uncompressed if needed
		Common::MemoryReadStream compressedReadStream(ptr, compressedSize);
		uncompressedBuf = new byte[uncompressedSize];
		if (!Common::decompressDCL(&compressedReadStream, uncompressedBuf, compressedSize, uncompressedSize)) {
			fprintf (stderr, "Unable to decompress %s\n", name.c_str());
			delete[] uncompressedBuf;
			continue;
		}

		fout.write(uncompressedBuf, uncompressedSize);
		ptr = blockend;

		delete[] uncompressedBuf;
	}

	free (buf);

	return 0;
}
