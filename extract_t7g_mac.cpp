#include "util.h"

#define NUM_FILES 45

int main(int argc, char *argv[]) {
	FILE *ifp;
	char *filenames[NUM_FILES];

	if (argc != 2) {
		displayHelp("Usage: %s <file>\n", argv[0]);
	}

	if ((ifp = fopen(argv[1], "rb")) == NULL) {
		error("Could not open \'%s\'", argv[1]);
	}

	// Load the file names
	printf("Getting the name of the files...\n");
	if (fseek(ifp, 0x1BEEA8, SEEK_SET)) {
		fclose(ifp);
		error("Seek error");
	}
	for (int i = 0; i < NUM_FILES; i++) {
		uint8 len = readByte(ifp);
		char *name = new char[len + 1];
		fread(name, len, 1, ifp);
		name[len] = 0;
		filenames[i] = name;
	}

	// Extract the data
	printf("Extracting the files...\n");
	if (fseek(ifp, 0x1777B2, SEEK_SET)) {
		fclose(ifp);
		error("Seek error");
	}
	for (int i = 0; i < NUM_FILES; i++) {
		printf("  %s\n", filenames[i]);
		uint32 file_size = readUint32BE(ifp);

		byte *buf = new byte[file_size];
		if (!buf) {
			fclose(ifp);
			error("Could not allocate %ld bytes of memory", file_size);
		}

		FILE *ofp = fopen(filenames[i], "wb");
		fread(buf, 1, file_size, ifp);
		fwrite(buf, 1, file_size, ofp);
		fclose(ofp);
		delete[] buf;
	}

	// Free the allocated filenames
	for (int i = 0; i < NUM_FILES; i++) {
		delete[] filenames[i];
	}

	fclose(ifp);
	return 0;
}
