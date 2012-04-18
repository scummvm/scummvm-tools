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

void writeUint16(FILE *file, uint16_t value) {
	char v[2];
	WRITE_LE_UINT16(&v, value);
	fwrite(&v, 1, 2, file);
}

void writeUint32(FILE *file, uint32_t value) {
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

static char *appendPath(const char *name, const char *dir) {
	int namelen = strlen(name);
	int dirlen = strlen(dir);
	char *path = (char *)malloc(namelen + dirlen + 2);
	strcpy(path, dir);
	path[dirlen] = '/';
	strcpy(path + dirlen + 1, name);
	return path;
}

static void countFiles(lab_header *head, DIR *dir, const char *dirname, int additionalLen) {
	struct dirent *dirfile;
	while ((dirfile = readdir(dir))) {
		if (!strcmp(dirfile->d_name, ".") || !strcmp(dirfile->d_name, ".."))
			continue;

		if (dirfile->d_type == S_IFDIR) {
			char *d = appendPath(dirfile->d_name, dirname);
			DIR *subdir = opendir(d);
			countFiles(head, subdir, d, additionalLen + strlen(dirfile->d_name) + 1);
			free(d);
			closedir(subdir);
		} else {
			++head->num_entries;
			head->string_table_size += strlen(dirfile->d_name) + 1 + additionalLen;
		}
	}
}

static void createEntries(DIR *dir, lab_entry *entries, char *str_table, const char *dirname, uint32_t &offset) {
	static uint32_t num_entry = 0;
	static uint32_t name_offset = 0;
	static char *str_offset = str_table;
	struct dirent *dirfile;
	while ((dirfile = readdir(dir))) {
		if (!strcmp(dirfile->d_name, ".") || !strcmp(dirfile->d_name, ".."))
			continue;

		if (dirfile->d_type == S_IFDIR) {
			char *d = appendPath(dirfile->d_name, dirname);
			DIR *subdir = opendir(d);
			createEntries(subdir, entries, str_table, d, offset);
			free(d);
			closedir(subdir);
		} else {
			lab_entry &entry = entries[num_entry++];

			WRITE_LE_UINT32(&entry.fname_offset, name_offset);
			WRITE_LE_UINT32(&entry.start, offset);
			entry.reserved = 0; //What is this??

			char *path = appendPath(dirfile->d_name, dirname);
			char *name = strrchr(path, '/') + 1;
			strcpy(str_offset, name);
			str_offset[strlen(name)] = 0;
			name_offset += strlen(name) + 1;
			str_offset = str_table + name_offset;

			struct stat st;
			stat(path, &st);

			// 		printf("entry of file %s, at offset %d and of size %d\n", path, offset, st.st_size);
			free(path);

			offset += st.st_size;
			WRITE_LE_UINT32(&entry.size, st.st_size);
		}
	}
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

	DIR *dir = opendir(dirname);
	if (dir == 0) {
		printf("Can not open source dir: %s\n", dirname);
		exit(2);
	}

	lab_header head;

	head.num_entries = 0;
	head.string_table_size = 0;

	countFiles(&head, dir, dirname, 0);

// 	printf("%d files, string table of size %d\n", head.num_entries, head.string_table_size);

	lab_entry *entries = (lab_entry *)malloc(head.num_entries * sizeof(lab_entry));
	char *str_table = (char *)malloc(head.string_table_size);
	if (!str_table || !entries) {
		printf("Could not allocate memory\n");
		exit(3);
	}

	rewinddir(dir);
	uint32_t offset = 16 + head.num_entries * sizeof(lab_entry) + head.string_table_size + 16;
	createEntries(dir, entries, str_table, dirname, offset);

	closedir(dir);

	// Open the output file after we've finished with the dir, so that we're sure
	// we don't include the lab into itself if it was asked to be created into the same dir.
	FILE *outfile = fopen(out, "wb");
	if (!outfile) {
		printf("Could not open file %s for writing\n", out);
		exit(2);
	}

	fwrite("LABN", 1, 4, outfile);
	fwrite("\x00\x00\x01\x00", 1, 4, outfile); //version
	writeUint32(outfile, head.num_entries);
	writeUint32(outfile, head.string_table_size);

	if (g_type == GT_GRIM) {
		uint32_t s_offset = 0; // First entry of the table has offset 0 for Grim
		fwrite(&s_offset, 1, 4, outfile);
		fseek(outfile, -4, SEEK_CUR);
	} else { // EMI has an offset instead.
		writeUint32(outfile, 20 + head.num_entries * sizeof(lab_entry) + 0x13d0f);
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
		free(s);
	}

	uint32_t bufsize = 1024*1024;
	char *buf = (char *)malloc(bufsize);

	for (uint32_t i = 0; i < head.num_entries; ++i) {
		lab_entry &entry = entries[i];
		const char *fname = str_table + READ_LE_UINT32(&entry.fname_offset);

		char *path = appendPath(fname, dirname);

		FILE *file = fopen(path, "rb");
		free(path);

		uint32_t offset = READ_LE_UINT32(&entry.start);
		uint32_t size = READ_LE_UINT32(&entry.size);

		if (size > bufsize) {
			char *newbuf = (char *)realloc(buf, size);
			if (!newbuf) {
				free(buf);
				printf("Could not allocate memory\n");
				exit(3);
			}
			bufsize = size;
			buf = newbuf;
		}

// 		printf("writing file %s, at offset %d and of size %d\n", fname, offset, size);

		fread(buf, 1, size, file);
		fseek(outfile, offset, SEEK_SET);
		fwrite(buf, 1, size, outfile);

		fclose(file);
	}

	fclose(outfile);
	free(buf);
	free(entries);
	free(str_table);

	return 0;
}
