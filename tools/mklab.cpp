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
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>


#define GT_GRIM 1
#define GT_EMI 2

typedef struct {
	uint32_t magic;
	uint32_t magic2;
	uint32_t num_entries;
	uint32_t string_table_size;
	uint32_t string_table_offset;
} lab_header;

typedef struct {
	uint32_t fname_offset;
	uint32_t start;
	uint32_t size;
	uint32_t reserved;
} lab_entry;

uint32_t READ_LE_UINT32(const void *ptr) {
	const uint8_t *b = (const uint8_t *)ptr;
	return (b[3] << 24) + (b[2] << 16) + (b[1] << 8) + (b[0]);
}

void WRITE_LE_UINT16(void *ptr, uint16_t value) {
	uint8_t *b = (uint8_t *)ptr;
	b[0] = (uint8_t)(value >> 0);
	b[1] = (uint8_t)(value >> 8);
}
void WRITE_LE_UINT32(void *ptr, uint32_t value) {
	uint8_t *b = (uint8_t *)ptr;
	b[0] = (uint8_t)(value >>  0);
	b[1] = (uint8_t)(value >>  8);
	b[2] = (uint8_t)(value >> 16);
	b[3] = (uint8_t)(value >> 24);
}

void write(FILE *file, uint16_t value) {
	char v[2];
	WRITE_LE_UINT16(&v, value);
	fwrite(&v, 1, 2, file);
}

void write(FILE *file, uint32_t value) {
	char v[4];
	WRITE_LE_UINT32(&v, value);
	fwrite(&v, 1, 4, file);
}

void usage() {
	printf("Usage: mklab --grim/--emi DIRECTORY FILE\n");
}

void help() {
	usage();
	printf("Create a lab file containing all the files into the specified directory\n\n");
	printf("\t--grim\tCreate a Grim-compatible lab.\n");
	printf("\t--emi\tCreate an EMI-compatible lab.\n");
	printf("\t--help\tPrint this help.\n");
	exit(0);
}

int main(int argc, char **argv) {
	if (argc > 1 && !strcmp(argv[1], "--help")) {
		help();
	}

	if (argc < 4) {
		usage();
		exit(1);
	}

	const char *type = argv[1];
	const char *dirname = argv[2];
	const char *out = argv[3];

	uint8_t g_type;
	if (!strcmp(type, "--grim")) {
		g_type = GT_GRIM;
	} else if (!strcmp(type, "--emi")) {
		g_type = GT_EMI;
	} else {
		usage();
		exit(1);
	}

	lab_header head;

	DIR *dir = opendir(dirname);
	if (dir == 0) {
		printf("Can not open source dir: %s\n", argv[1]);
		exit(2);
	}

	head.num_entries = 0;
	head.string_table_size = 0;
	struct dirent *dirfile;
	while ((dirfile = readdir(dir))) {
		if (!strcmp(dirfile->d_name, ".") || !strcmp(dirfile->d_name, ".."))
			continue;

		++head.num_entries;
		head.string_table_size += strlen(dirfile->d_name) + 1;
	}
// 	printf("%d files, string table of size %d\n", head.num_entries, head.string_table_size);

	lab_entry *entries = (lab_entry *)malloc(head.num_entries * sizeof(lab_entry));
	char *str_table = (char *)malloc(head.string_table_size);
	if (!str_table || !entries) {
		printf("Could not allocate memory\n");
		exit(3);
	}

	uint32_t offset = 16 + head.num_entries * sizeof(lab_entry) + head.string_table_size + 16;
	uint32_t num_entry = 0;
	uint32_t name_offset = 0;
	char *str_offset = str_table;
	rewinddir(dir);
	while ((dirfile = readdir(dir))) {
		if (!strcmp(dirfile->d_name, ".") || !strcmp(dirfile->d_name, ".."))
			continue;

		lab_entry &entry = entries[num_entry++];

		WRITE_LE_UINT32(&entry.fname_offset, name_offset);
		WRITE_LE_UINT32(&entry.start, offset);
		entry.reserved = 0; //What is this??

		strcpy(str_offset, dirfile->d_name);
		str_offset[strlen(dirfile->d_name)] = 0;
		name_offset += strlen(dirfile->d_name) + 1;
		str_offset = str_table + name_offset;

		char *path = (char *)malloc(strlen(dirfile->d_name) + strlen(dirname) + 2);
		strcpy(path, dirname);
		path[strlen(dirname)] = '/';
		strcpy(path + strlen(dirname) + 1, dirfile->d_name);

		struct stat st;
		stat(path, &st);

		// 		printf("entry of file %s, at offset %d and of size %d\n", path, offset, st.st_size);
		free(path);

		offset += st.st_size;
		WRITE_LE_UINT32(&entry.size, st.st_size);
	}
	closedir(dir);

	// Open the output file after we've finished with the dir, so that we're sure
	// we don't include the lab into itself if it was asked to be created into the same dir.
	FILE *outfile = fopen(out, "wb");
	if (!outfile) {
		printf("Could not open file %s for writing\n", argv[2]);
		exit(2);
	}

	fwrite("LABN", 1, 4, outfile);
	fwrite("    ", 1, 4, outfile); //version
	write(outfile, head.num_entries);
	write(outfile, head.string_table_size);

	if (g_type == GT_GRIM) {
		uint32_t s_offset = 0; // First entry of the table has offset 0 for Grim
		fwrite(&s_offset, 1, 4, outfile);
		fseek(outfile, -4, SEEK_CUR);
	} else { // EMI has an offset instead.
		write(outfile, 20 + head.num_entries * sizeof(lab_entry) + 0x13d0f);
	}

	fwrite(entries, 1, head.num_entries * sizeof(lab_entry), outfile);
	if (g_type == GT_GRIM) {
		fwrite(str_table, 1, head.string_table_size, outfile);
	} else {
		char *s = (char *)malloc(head.string_table_size);
		memset(s, 0, head.string_table_size);
		for (uint32_t j = 0; j < head.string_table_size; j++) {
			if (str_table[j] != 0)
				s[j] = str_table[j] ^ 0x96;
		}
		fwrite(s, 1, head.string_table_size, outfile);
	}

	for (uint32_t i = 0; i < head.num_entries; ++i) {
		lab_entry &entry = entries[i];
		const char *fname = str_table + READ_LE_UINT32(&entry.fname_offset);

		char *path = (char *)malloc(strlen(fname) + strlen(dirname) + 2);
		strcpy(path, dirname);
		path[strlen(dirname)] = '/';
		strcpy(path + strlen(dirname) + 1, fname);

		FILE *file = fopen(path, "rb");
		free(path);

		offset = READ_LE_UINT32(&entry.start);
		uint32_t size = READ_LE_UINT32(&entry.size);

		char *buf = (char *)malloc(size);

// 		printf("writing file %s, at offset %d and of size %d\n", fname, offset, size);

		fread(buf, 1, size, file);
		fseek(outfile, offset, SEEK_SET);
		fwrite(buf, 1, size, outfile);

		free(buf);
		fclose(file);
	}

	fclose(outfile);
	free(entries);
	free(str_table);

	return 0;
}
