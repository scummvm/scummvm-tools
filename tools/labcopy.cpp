/* Residual - A 3D game interpreter
*
* Residual is the legal property of its developers, whose names
* are too numerous to list here. Please refer to the AUTHORS
* file distributed with this source distribution.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
*
* $URL:
* $Id:
*
*/
/*labcopy.cpp
 quick & dirty LAB file copier based on residual code.

 Adapted from code posted on residual forum by Joost Peters
 http://residual.scummvm.org/viewtopic.php?t=91
*/


#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "common/endian.h"

#define BUFFER_SIZE 		0x100000
FILE *inLab = NULL, *outLab = NULL;
void *buffer = NULL;

struct lab_entry {
	uint32 fname_offset;
	uint32 start;
	uint32 size;
	uint32 reserved;
};


bool copyFile(uint32 offset, uint32 lenght) {
	uint32 copied_bytes, count, bytesToRead;

	copied_bytes = 0;
	fseek(inLab, offset, SEEK_SET);
	fseek(outLab, offset, SEEK_SET);

	while (copied_bytes < lenght) {
		bytesToRead = (lenght - copied_bytes < BUFFER_SIZE) ? lenght - copied_bytes : BUFFER_SIZE;
		count = (uint32)fread(buffer, 1, bytesToRead, inLab);
		fwrite(buffer, count, 1, outLab);
		copied_bytes += count;
		if(ferror(inLab) != 0 || ferror(outLab) != 0)
			return false;
	}

	return true;
}

bool copyLab() {
	char header[16];
	uint32 num_entries, string_table_size;
	char *string_table;
	lab_entry *lab_entries;

	//Read and parse header
	fseek(inLab, 0, SEEK_SET);
	fread(header, 16, 1, inLab);
	if (READ_BE_UINT32(header) != MKTAG('L','A','B','N')) {
		printf("This isn't a valid .lab file!\n");
		exit(1);
	}

	num_entries = READ_LE_UINT32(header + 8);
	string_table_size = READ_LE_UINT32(header + 12);

	//Read files entries
	lab_entries = (lab_entry *)calloc(sizeof(lab_entry), num_entries);
	fread(lab_entries, 1, num_entries * sizeof(struct lab_entry), inLab);
	
	//Read string table
	string_table = (char *)malloc(string_table_size);
	fread(string_table, 1, string_table_size, inLab);

	//Write out new lab
	fwrite(header, 16, 1, outLab);
	fwrite(lab_entries, sizeof(struct lab_entry), num_entries, outLab);
	fwrite(string_table, string_table_size, 1, outLab);

	//Check for errors
	if(ferror(inLab) != 0 || ferror(outLab) != 0) {
		free(lab_entries);
		free(string_table);
		return false;
	}

	//Copy the files, except cp_0_intha.bm
	for (uint32 i = 0; i < num_entries; i++)
		if(strcmp(string_table + READ_LE_UINT32(&lab_entries[i].fname_offset), "cp_0_intha.bm") != 0)
			if (!copyFile(lab_entries[i].start, lab_entries[i].size)) {
				free(lab_entries);
				free(string_table);
				return false;
			}

	free(lab_entries);
	free(string_table);
	return true;
}

void cleanup() {
	if (inLab)
		fclose(inLab);

	if (outLab)
		fclose(outLab);

	if (buffer)
		free(buffer);
}

int main(int argc, char *argv[]) {
	atexit(cleanup);
	
	//Argument checks and usage display
	if (argc != 3) {
		printf("Usage: labcopy original.lab destination.lab\n");
		printf("Copy original.lab from Grimfandango cd with illegal-toc protection.\n");
		return 1;
	}
	
	//Files opening
	inLab = fopen(argv[1], "rb");
	if (!inLab) {
		printf("Couldn't open %s!\n", argv[1]);
		return 1;
	}

	outLab = fopen(argv[2], "wb");
	if (!outLab) {
		printf("Couldn't write to %s!\n", argv[2]);
		return 1;
	}

	buffer = malloc(BUFFER_SIZE);
	if (!buffer) {
		printf("Unable to allocate memory!\n");
		return 1;
	}

	if (!copyLab()) {
		printf("I/O error!\n");
		return 1;
	}

	printf("%s successfully copied to %s.\n", argv[1], argv[2]);
	return 0;
}
