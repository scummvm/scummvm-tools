/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common/scummsys.h"

#define RAW_SECTOR_SIZE	2352
#define MIN(x, y)	((x) < (y) ? (x) : (y));

typedef unsigned char  Uint8;
typedef unsigned short Uint16;
typedef unsigned int   Uint32;

#include "common/pack-start.h"
typedef struct {
	Uint16 file;
	Uint16 channel;
	Uint16 submode;
	Uint16 datatype;
} sub_header;

typedef struct {
	Uint8	sync[12];  // synch string at beginning of sector "0x00 FF FF FF FF FF FF FF FF FF FF 00"
	Uint8	header[4]; // header with info on current mode... we'll be always at mode 2 with this file
	sub_header	subheader; // sub header with info on current submode (for mixed sectors)
	Uint8	data[2048];
	Uint8	edc[4];
	Uint8	ecc[276];
} sect_xa_f1;

typedef struct {
	Uint8	sync[12];  // synch string at beginning of sector "0x00 FF FF FF FF FF FF FF FF FF FF 00"
	Uint8	header[4]; // header with info on current mode... we'll be always at mode 2 with this file
	sub_header	subheader; // sub header with info on current submode (for mixed sectors)
	Uint8	data[2324];
	Uint8	edc[4];
} sect_xa_f2;

typedef struct {
	char	filename[16];
	Uint32	offset; // In RAW (2352b) sectors
	Uint32	size; // In bytes.
	Uint8	unk[8]; // always 0?
} rtf_entry;
#include "common/pack-end.h"

void fix_entry_endianess(rtf_entry* entry) {
	Uint32 data;
#ifdef SCUMM_LITTLE_ENDIAN
	data = entry->offset;
	data = ((data >> 24) & 0x000000FF) |
		((data >> 8) & 0x0000FF00) |
		((data << 8) & 0x00FF0000) |
		((data << 24) & 0xFF000000);
	entry->offset = data;

	data = entry->size;
	data = ((data >> 24) & 0x000000FF) |
		((data >> 8) & 0x0000FF00) |
		((data << 8) & 0x00FF0000) |
		((data << 24) & 0xFF000000);

	entry->size = data;
#else
	; // Do nothing...
#endif
}

int isSectorMode2(sect_xa_f1* sect) {
#ifdef SCUMM_LITTLE_ENDIAN
	return (sect->subheader.datatype) & 0x0060 ? 1 : 0;
#else
	return (sect->subheader.datatype) & 0x6000 ? 1 : 0;
#endif
}

int main(int argc, char** argv) {
	FILE* src_raw;
	FILE* dst_file;

	Uint8	index[4096];
	sect_xa_f1 sector;
	sect_xa_f2* sector_f2;

	if (argc != 2) {
		fprintf(stdout, "Usage: %s <real-time-file>\n", argv[0]);
		return -1;
	}

	src_raw = fopen(argv[1], "rb");
	if (src_raw == NULL) {
		perror(argv[1]);
		return -1;
	}

	// Read the index!
	fread(&sector, sizeof(sect_xa_f1), 1, src_raw);
	memcpy((index + 0), &(sector.data), 2048);
	fread(&sector, sizeof(sect_xa_f1), 1, src_raw);
	memcpy((index + 2048), &(sector.data), 2048);

	// Read entries
	int entryNum = 0;
	while (1) {
		rtf_entry* idx_entry = (rtf_entry*)(index + (entryNum * sizeof(rtf_entry)));
		entryNum++;
		if (idx_entry->filename[0] == 0) break;

		fix_entry_endianess(idx_entry);

		if (strcmp(idx_entry->filename, "DIRINFO") == 0) continue; // We are not interested in this

		fprintf(stdout, "Entry:     %s\n", idx_entry->filename);
		fprintf(stdout, "Begins at: %u RAW blocks.\n", idx_entry->offset);
		fprintf(stdout, "Size:      ~%uKb\n\n", (idx_entry->size / 1024) + (idx_entry->size % 1024 ? 1 : 0));

		Uint32 remaining_size = idx_entry->size;

		dst_file = fopen(idx_entry->filename, "wb");
		if (dst_file == NULL) {
			perror(idx_entry->filename);
			return -1;
		}

		// Get to the interesting sector.
		fseek(src_raw, idx_entry->offset * RAW_SECTOR_SIZE, SEEK_SET);
		while (remaining_size) {
			fread(&sector, sizeof(sect_xa_f1), 1, src_raw);

			if (isSectorMode2(&sector)) { // Mode 2 (2324b)
				Uint32 toRead = MIN(2324, remaining_size);

				sector_f2 = (sect_xa_f2*)&sector;
				fwrite(&(sector_f2->data), toRead, 1, dst_file);

				remaining_size -= toRead;
			}
			else { // Mode 1 (2048b)
				Uint32 toRead = MIN(2048, remaining_size);

				fwrite(&(sector.data), toRead, 1, dst_file);

				remaining_size -= toRead;
			}
		}

		fclose(dst_file);
	}

	fclose(src_raw);

	return 0;
}
