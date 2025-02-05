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
#include <sys/stat.h>
#include <sys/types.h>

int main (int argc, char **argv) {
	FILE *fin;

	if (argc < 3) {
		fprintf (stderr, "USAGE: %s FILENAME OUTDIR\n", argv[0]);
		return -1;
	}

	Common::String base = argv[1];
	Common::String outdir = argv[2];

	fin = fopen (base.c_str(), "rb");
	if (fin == NULL) {
		fprintf (stderr, "Unable to open %s: %s\n", argv[1], strerror(errno));
		return -2;
	}

	byte buf[1024];

	fread (buf, 1, 5, fin);
	byte version = buf[0];
	printf("version = %d\n", version);
	if (version != 1) {
		fprintf (stderr, "Unsupported version (%d)", version);
		return -5;
	}
	uint32 numfiles = READ_LE_UINT32(buf + 1);

	for (uint i = 0; i < numfiles; i++) {
	  	fread (buf, 1, 4, fin);
		uint32 nameLen = READ_LE_UINT32(buf);
		if (nameLen > sizeof(buf) - 1) {
			fprintf (stderr, "Name too long (%d)", nameLen);
			return -3;
		}
	  	fread (buf, 1, nameLen, fin);
		buf[nameLen] = 0;
		if (strlen((char *)buf) != nameLen) {
			fprintf (stderr, "Name contains NULs");
			hexdump(buf, nameLen);
			return -4;
		}
		Common::String name((char *)buf);
	  	fread (buf, 1, 8, fin);
		uint32 offset = READ_LE_UINT32(buf);		    
		uint32 sz = READ_LE_UINT32(buf + 4);
		printf("%08x %08x %s\n", offset, sz, name.c_str());
		Common::String fullpath = outdir + "/" + name;
		memset(buf, 0, sizeof(buf));
		strncpy((char *)buf, fullpath.c_str(), sizeof(buf) - 2);
		for (uint j = 0; buf[j]; j++) {
			if (j != 0 && buf[j] == '/') {
				buf[j] = 0;
				mkdir((char *)buf, 0755);
				buf[j] = '/';
			}
		}
		FILE *fout = fopen(fullpath.c_str(), "wb");
		if (fout == NULL) {
			fprintf (stderr, "Unable to open %s: %s\n", fullpath.c_str(), strerror(errno));
			return -6;
		}
		off_t oldoff = ftello(fin);
		fseeko(fin, offset, SEEK_SET);
		for (uint32 j = 0; j < sz; ) {
			uint32 chunk = sizeof(buf);
			if (chunk > sz - j)
				chunk = sz - j;
			fread(buf, 1, chunk, fin);
			fwrite(buf, 1, chunk, fout);
			j += chunk;
		}
		fclose(fout);
		fseeko(fin, oldoff, SEEK_SET);
	}
	fclose(fin);

	return 0;
}
