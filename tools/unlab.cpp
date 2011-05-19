/* Residual - A 3D game interpreter
 *
 * Residual is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the AUTHORS
 * file distributed with this source distribution.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 *
 * $URL: https://residual.svn.sourceforge.net/svnroot/residual/residual/trunk/tools/unlab.cpp $
 * $Id: unlab.cpp 1374 2009-05-27 07:20:49Z aquadran $
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>


#define GT_GRIM 1
#define GT_EMI 2

struct lab_header {
	uint32_t magic;
	uint32_t magic2;
	uint32_t num_entries;
	uint32_t string_table_size;
	uint32_t string_table_offset;
};

struct lab_entry {
	uint32_t fname_offset;
	uint32_t start;
	uint32_t size;
	uint32_t reserved;
};

uint16_t READ_LE_UINT16(const void *ptr) {
	const uint8_t *b = (const uint8_t *)ptr;
	return (b[1] << 8) + b[0];
}
uint32_t READ_LE_UINT32(const void *ptr) {
	const uint8_t *b = (const uint8_t *)ptr;
	return (b[3] << 24) + (b[2] << 16) + (b[1] << 8) + (b[0]);
}

int main(int argc, char **argv) {
	FILE *infile, *outfile;
	struct lab_header head;
	struct lab_entry *entries;
	char *str_table;
	uint32_t i;
	uint32_t offset;
	uint8_t g_type;

	if (argc < 2) {
		printf("No file specified\n");
		exit(1);
	}

	if(argc > 2 && !strcmp(argv[2], "EMI")) {
		printf("Opening file with EMI format.\n");
		g_type = GT_EMI;
	} else {
		printf("Opening file with GRIM format.\n");
		g_type = GT_GRIM;
	}

	infile = fopen(argv[1], "rb");
	if (infile == 0) {
		printf("Can not open source file: %s\n", argv[1]);
		exit(1);
	}

	fread(&head.magic, 1, 4, infile);
	fread(&head.magic2, 1, 4, infile);
	uint32_t num, s_size, s_offset;
	fread(&num, 1, 4, infile);
	fread(&s_size, 1, 4, infile);
	if(g_type == GT_EMI)
		fread(&s_offset,1,4,infile);
	head.num_entries = READ_LE_UINT32(&num);
	head.string_table_size = READ_LE_UINT32(&s_size);
	if (0 != memcmp(&head.magic, "LABN", 4)) {
		printf("There is no LABN header in source file\n");
		exit(1);
	}

	entries = (struct lab_entry *)malloc(head.num_entries * sizeof(struct lab_entry));
	str_table = (char *)malloc(head.string_table_size);
	if (!str_table || !entries) {
		printf("Could not allocate memory\n");
		exit(1);
	}
	// Grim-stuff
	if(g_type == GT_GRIM) {
		fread(entries, 1, head.num_entries * sizeof(struct lab_entry), infile);

		fread(str_table, 1, head.string_table_size, infile);
	} else if(g_type == GT_EMI) { // EMI-stuff
		// EMI has a string-table-offset
		head.string_table_offset = READ_LE_UINT32(&s_offset) - 0x13d0f;
		// Find the string-table
		fseek(infile, head.string_table_offset, SEEK_SET);
		// Read the entire string table into str-table
		fread(str_table, 1, head.string_table_size, infile);
		fseek(infile, 20, SEEK_SET);

		// Decrypt the string table
		uint32_t j;
		for (j = 0; j < head.string_table_size; j++)
			if (str_table[j] != 0)
				str_table[j] ^= 0x96;
		fread(entries, 1, head.num_entries * sizeof(struct lab_entry), infile);

	}
	// allocate a 1mb buffer to start with
	uint32_t bufSize = 1024*1024;
	char *buf = (char *)malloc(bufSize);
	if (!buf) {
		printf("Could not allocate memory\n");
		exit(1);
	}
	for (i = 0; i < head.num_entries; i++) {
		const char *fname = str_table + READ_LE_UINT32(&entries[i].fname_offset);
		outfile = fopen(fname, "wb");
		if (!outfile) {
			printf("Could not open file: %s\n", fname);
			continue;
		}
		offset = READ_LE_UINT32(&entries[i].start);
		uint32_t size = READ_LE_UINT32(&entries[i].size);
		if (bufSize < size) {
			bufSize = size;
			char *newBuf = (char *)realloc(buf, bufSize);
			if (!newBuf) {
				printf("Could not reallocate memory\n");
				exit(1);
			} else {
				buf = newBuf;
			}
		}

		fseek(infile, offset, SEEK_SET);
		fread(buf, 1, READ_LE_UINT32(&entries[i].size), infile);
		fwrite(buf, 1, READ_LE_UINT32(&entries[i].size), outfile);
		fclose(outfile);

	}
	free(buf);
	free(entries);
	free(str_table);

	fclose(infile);
	return 0;
}
