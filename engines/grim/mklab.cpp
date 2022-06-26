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

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include "common/endian.h"

#include <vector>
#include <string>


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


static void countFiles(std::vector<std::string> &files, lab_header *head, DIR *dir, const std::string &d, std::string subdirn = "") {
	struct dirent *dirfile;
	struct stat st;
	while ((dirfile = readdir(dir))) {
		if (!strcmp(dirfile->d_name, ".") || !strcmp(dirfile->d_name, "..")) {
			continue;
		}
		std::string dirname = d + "/" + dirfile->d_name;
		if (stat(dirname.c_str(), &st) != 0) {
			continue;
		}
		if (S_ISDIR(st.st_mode)) {
			std::string nextsub = subdirn;
			nextsub += dirfile->d_name;
			nextsub += "/";
			std::string subdirname = d + "/" + dirfile->d_name;
			DIR *subdir = opendir(subdirname.c_str());
			countFiles(files, head, subdir, subdirname, nextsub);
			closedir(subdir);
		} else {
			std::string fname = subdirn + dirfile->d_name;
			files.push_back(fname);
			head->string_table_size += fname.length() + 1;
			head->num_entries++;
		}
	}
}



static void createEntries(const std::vector<std::string> &files, lab_entry *entries, char *str_table, uint32_t offset, const std::string &dirname) {
	uint32_t name_offset = 0;
	char *str_offset = str_table;

	uint size = files.size();
	for (uint i = 0; i < size; ++i) {
		lab_entry &entry = entries[i];

		WRITE_LE_UINT32(&entry.fname_offset, name_offset);
		WRITE_LE_UINT32(&entry.start, offset);
		entry.reserved = 0; //What is this??

		const std::string &name = files[i];

		strcpy(str_offset, name.c_str());
		str_offset[name.length()] = 0;
		name_offset += name.length() + 1;
		str_offset = str_table + name_offset;

		std::string fullname = dirname + "/" + name;

		struct stat st;
		stat(fullname.c_str(), &st);

		offset += st.st_size;
		WRITE_LE_UINT32(&entry.size, st.st_size);
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

	std::vector<std::string> files;
	countFiles(files, &head, dir, dirname);
	closedir(dir);


	lab_entry *entries = new lab_entry[head.num_entries];
	char *str_table = new char[head.string_table_size];
	if (!str_table || !entries) {
		printf("Could not allocate memory\n");
		exit(3);
	}

	uint32_t offset = 16 + head.num_entries * sizeof(lab_entry) + head.string_table_size + 16;
	createEntries(files, entries, str_table, offset, dirname);


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
			if (str_table[j] != 0) {
				s[j] = str_table[j] ^ 0x96;
			}
		}
		fwrite(s, 1, head.string_table_size, outfile);
		free(s);
	}

	uint32_t bufsize = 1024 * 1024;
	char *buf = (char *)malloc(bufsize);

	for (uint i = 0; i < files.size(); ++i) {
		lab_entry &entry = entries[i];
		std::string fname = files[i];

		std::string path = dirname;
		path += "/" + fname;

		FILE *file = fopen(path.c_str(), "rb");

		uint32_t file_offset = READ_LE_UINT32(&entry.start);
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


		fread(buf, 1, size, file);
		fseek(outfile, file_offset, SEEK_SET);
		fwrite(buf, 1, size, outfile);

		fclose(file);
	}

	fclose(outfile);
	free(buf);
	delete[] entries;
	delete[] str_table;

	return 0;
}
