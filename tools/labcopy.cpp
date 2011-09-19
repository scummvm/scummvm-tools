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

#define BUFFER_SIZE 		102400
FILE *inLab = NULL, *outLab = NULL;

int copyLab(long lenght = 0) {
	void *buffer;
	long copied_bytes, count, bytesToRead, pos;
	int retCod = 0;
	
	if (lenght == 0) {
		fseek(inLab, 0L, SEEK_END);
		lenght = ftell(inLab);
	}

	buffer = malloc(BUFFER_SIZE);
	copied_bytes = 0;
	fseek(inLab, 0, SEEK_SET);

	while (copied_bytes < lenght) {
		bytesToRead = (lenght - copied_bytes < BUFFER_SIZE) ? lenght - copied_bytes : BUFFER_SIZE;
		pos = ftell(inLab);
		count = (long)fread(buffer, 1, bytesToRead, inLab);

		//If we get an input error, we skip this block
		if(ferror(inLab) != 0) {
			clearerr(inLab);
			fseek(inLab, pos + bytesToRead, SEEK_SET);
			count = bytesToRead;
			memset (buffer, 0, BUFFER_SIZE);
			retCod = 1;
		}

		fwrite(buffer, count, 1, outLab);
		copied_bytes += count;
		if(ferror(outLab) != 0) {
			free(buffer);
			return -1;
		}
	}

	free(buffer);
	return retCod;
}

long getLabSize() {
	long total_size = 0;
	char header[16], binary_entry[16];
	int num_entries, string_table_size;

	fseek(inLab, 0, SEEK_SET);

	fread(header, 16, 1, inLab);
	if (READ_BE_UINT32(header) != MKTAG('L','A','B','N')) 
		return -1;

	num_entries = READ_LE_UINT32(header + 8);
	string_table_size = READ_LE_UINT32(header + 12);

	total_size = 16 + num_entries * 16 + string_table_size;

	for (int i = 0; i < num_entries; i++) {
		fread(binary_entry, 16, 1, inLab);
		total_size += READ_LE_UINT32(binary_entry + 8);
	}
	
	return total_size;
}

void cleanup() {
	if (inLab)
		fclose(inLab);
	
	if (outLab)
		fclose(outLab);
}

int main(int argc, char *argv[]) {
	long labSize;
	int retCod;
	
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

	//Get the correct lab size
	labSize = getLabSize();
	if (labSize < 0) {
		printf("%s isn't a valid .lab file!\n", argv[1]);
		return 1;
	}

	//Lab copying
	retCod = copyLab(labSize);
	switch(retCod) {
		case 0:
			printf("%s successfully copied to %s.\n", argv[1], argv[2]);
			return 0;
		case 1:
			printf("%s copied to %s, but with some read errors.\n", argv[1], argv[2]);
			printf("On some version (e.g. the German one) this is due to copy-protection\n");
			printf("and this message can be safely ignored. Otherwise check your disk.\n");
			return 0;
		case -1:
			printf("Output error!\n");
			return 1;
	}
}
