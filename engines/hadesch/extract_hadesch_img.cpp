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
#include "common/array.h"
#include "common/str.h"
#include "common/util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

struct palette_element {
	int idx;
	int r, g, b;
	palette_element() = default;
	palette_element(int idx_in, int r_in, int g_in, int b_in) : idx(idx_in), r(r_in), g(g_in), b(b_in) {}
};

struct image {
	int w;
	int h;
	int alignw;

	unsigned char *buf;

	~image() {
		free(buf);
	}
};

int decode_image (const unsigned char *buf, int sz, image* iout) {
	if (memcmp (buf, "CEL ", 4) != 0) {
		fprintf (stderr, "Bad signature\n");
		return 1;
	}

	int cur;
	int ssz;

	const unsigned char *dataptr = NULL;
	int datasize = 0;

	for (cur = 8; cur < sz; cur +=ssz) {
		const unsigned char *ptr = buf + cur;
		ssz = READ_BE_UINT32(ptr + 4);

		if (memcmp (ptr, "INFO", 4) == 0) {
			iout->w = READ_BE_UINT32(ptr + 0x1c);
			iout->h = READ_BE_UINT32(ptr + 0x20);
			printf ("Dimensions: %d x %d\n", iout->w, iout->h);
			printf ("Off: %d x %d\n", -READ_BE_UINT32(ptr + 0x14), -READ_BE_UINT32(ptr + 0x18));
      
			continue;
		}

		if (memcmp (ptr, "DATA", 4) == 0) {
			dataptr = ptr + 8;
			datasize = ssz - 8;
			continue;
		}

		if (memcmp (ptr, "BLTM", 4) == 0
		    || memcmp (ptr, "LINZ", 4) == 0) {
			continue;
		}

		fprintf (stderr, "Unknown section: %s\n", ptr);
	}


	iout->alignw = iout->w;
	if (iout->w % 4)
		iout->alignw += 4 - (iout->w % 4);

	iout->buf = (unsigned char *) malloc(iout->alignw * iout->h);
	memset(iout->buf, 0, iout->alignw * iout->h);
	int line = iout->h-1;
	int linerem = iout->w;

	unsigned char *outptr = iout->buf + line * iout->alignw;

	for (int i = 0; i < datasize && line >= 0;) {
		if (dataptr[i] != 0) {
			int len = MIN(static_cast<int>(dataptr[i]), linerem);
			memset (outptr, dataptr[i + 1], len);
			outptr += len;			
			linerem -= dataptr[i];
			i += 2;
			continue;
		}

		if (dataptr[i + 1] != 0) {
			int len = MIN(static_cast<int>(dataptr[i + 1]), linerem);
			memcpy(outptr, dataptr + i + 2, len);
			outptr += len;
			linerem -= dataptr[i+1];
			i += 2 + dataptr[i+1];
			continue;
		}

		// End of line
		line--;
		outptr = iout->buf + line * iout->alignw;
		linerem = iout->w;
		i += 2;
	}

	return 0;
}

void write_bmp(const Common::Array<palette_element>& palette, const image& img, FILE *fout) {
	fwrite("BM", 2, 1, fout);
	unsigned char outbuf[256];
	unsigned char bmppal[256][4];
	const int numcols = 256;
	const int infohdrsize = 40;

	memset(outbuf, 0, sizeof(outbuf));
	WRITE_LE_UINT32(outbuf, 14 + infohdrsize + numcols * 4 + img.alignw * img.h);
	WRITE_LE_UINT32(outbuf + 8, 14 + infohdrsize + numcols * 4);
	WRITE_LE_UINT32(outbuf + 12, infohdrsize);
	WRITE_LE_UINT32(outbuf + 16, img.w);
	WRITE_LE_UINT32(outbuf + 20, img.h);
	WRITE_LE_UINT16(outbuf + 24, 1); // planes
	WRITE_LE_UINT16(outbuf + 26, 8); // bpp
	WRITE_LE_UINT32(outbuf + 44, numcols);

	fwrite(outbuf, infohdrsize + 12, 1, fout);

	memset(bmppal, 0, sizeof(bmppal));
	for (Common::Array<palette_element>::const_iterator it = palette.begin(); it != palette.end(); it++)
	{
		bmppal[it->idx][0] = it->b;
		bmppal[it->idx][1] = it->g;
		bmppal[it->idx][2] = it->r;
	}

	fwrite(bmppal, 4 * numcols, 1, fout);

	fwrite(img.buf, img.alignw * img.h, 1, fout);
}

int main (int argc, char **argv) {
	unsigned char * buf;
	size_t sz;
	FILE *fin;

	if (argc < 3) {
		fprintf (stderr, "USAGE: %s INFILE OUTDIR\n", argv[0]);
		return -1;
	}

	fin = fopen (argv[1], "rb");
	if (fin == NULL) {
		fprintf (stderr, "Unable to open %s: %s\n", argv[1], strerror(errno));
		return -2;
	}
	fseek (fin, 0, SEEK_END);
	sz = ftell (fin);
	fseek (fin, 0, SEEK_SET);

	buf = (unsigned char *) malloc (sz);

	fread (buf, 1, sz, fin);
	fclose (fin);

	if (memcmp (buf, "Pod\0file\0\0\0\0", 12) != 0
	    && memcmp (buf, "Pod File\0\0\0\0", 12) != 0
	    && memcmp (buf, "Pod\0\0\0\0\0\0\0\0\0", 12) != 0) {
		fprintf (stderr, "Bad signature\n");
		return 1;
	}

	int filecnt = READ_LE_UINT32(buf + 12);

	FILE *fout;

	int cur = filecnt * 16 + 16;
	int ctr = 0;

	fprintf (stderr, "%d files\n", filecnt);

	struct file_descriptor {
		Common::String name;
		int offset;
		int size;

		file_descriptor(Common::String nm, int off, int sz) : name(nm), offset(off), size(sz) {}
		file_descriptor() = default;
	};

	Common::Array<file_descriptor> images;
	bool palette_found = false;
	file_descriptor palette;

	images.reserve(filecnt);

	static const char *kPaletteName = "0";
 
	for (ctr = 0; ctr < filecnt; ctr++) {
		unsigned char * headptr = buf + 16 + 16 * ctr;
		char c = headptr[12];
		headptr[12] = 0;
		Common::String name = (const char *) headptr;
		headptr[12] = c;
		int csz = READ_LE_UINT32(headptr+12);
		if (name == kPaletteName) {
			palette = file_descriptor(name, cur, csz);
			palette_found = true;
		} else
			images.push_back(file_descriptor(name, cur, csz));
		cur += csz;
	}

	// Read palette
	if (!palette_found) {
		fprintf (stderr, "Couldn't find palette file\n");
		return 1;
	}

	const unsigned char *palette_ptr = buf + palette.offset;
	int palette_size = palette.size;
	const unsigned char *palette_block_ptr = NULL;
	int palette_block_size = 0;
	int ssz = 0;

	if (palette_size < 12) {
		fprintf (stderr, "Palette file is too short\n");
		return 1;
	}

	if (memcmp (palette_ptr, "CEL ", 4) != 0) {
		fprintf (stderr, "Bad palette signature\n");
		return 1;
	}

	for (cur = 8; cur < palette_size; cur += ssz) {
		const unsigned char *ptr = palette_ptr + cur;
		ssz = READ_BE_UINT32(ptr + 4);

		if (memcmp (ptr, "PAL ", 4) == 0) {
			palette_block_ptr = ptr + 8;
			palette_block_size = ssz - 8;
			continue;
		}

		fprintf (stderr, "Unknown palette section: %s\n", ptr);
	}

	if (palette_block_ptr == NULL) {
		fprintf (stderr, "Palette file is missing palette section\n");
		return 1;
	}

	Common::Array<palette_element> parsed_palette;

	parsed_palette.reserve(palette_block_size / 4);

	for (int i = 0; i < palette_block_size / 4; i++)
	{
		const unsigned char *col = palette_block_ptr + 4 * i;
		parsed_palette.push_back(palette_element(col[0], col[1], col[2], col[3]));
	}

	for (Common::Array<file_descriptor>::const_iterator it = images.begin();
	     it != images.end(); it++) {
		image img;
		printf ("Frame %s\n", it->name.c_str());
		Common::String fn = Common::String::format("%s/%s.bmp", argv[2], it->name.c_str());
		fout = fopen(fn.c_str(), "wb");
		if (fout == NULL) {
			fprintf (stderr, "Unable to open %s: %s\n", fn.c_str(), strerror(errno));
			return -3;
		}
		if (decode_image(buf + it->offset, it->size, &img)) {
			fclose(fout);
			return 1;
		}
		write_bmp(parsed_palette, img, fout);
		fclose(fout);		
	}

	return 0;
}
