/* ResidualVM - A 3D game interpreter
*
* ResidualVM is the legal property of its developers, whose names
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
*/

//Large parts of this program have been taken from bsdiff written by Colin Percival:
/*-
 * Copyright 2003-2005 Colin Percival
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted providing that the following conditions 
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <iostream>
#include <fstream>
#include <cstdlib>
#include "common/endian.h"
#include "common/zlib.h"
#include "common/md5.h"
#include "common/getopt.h"

uint8 *old_block, *new_block;
GZipReadStream *ctrlDec, *diffDec, *extraDec;

void free_memory() {
	if (old_block)
		delete[] old_block;
	if (new_block)
		delete[] new_block;

	if (ctrlDec)
		delete ctrlDec;
	if (diffDec)
		delete diffDec;
	if (extraDec && extraDec != diffDec)
		delete extraDec;
}

void show_header_info(uint8 *header) {
	printf("PatchR v%d.%d\n", READ_LE_UINT16(header + 4), READ_LE_UINT16(header + 6));
	printf("Md5: ");
	for (int i = 0; i < 16; ++i)
		printf("%x", *(header + 12 + i));
	printf("\n");

	uint32 flags = READ_LE_UINT32(header + 8);
	printf("MIX_DIFF_EXTRA %s\n", (flags & 1 << 0) ? "YES" : "NO");
	printf("COMPRESS_CTRL %s\n", (flags & 1 << 1) ? "YES" : "NO");
	printf("\n");

	printf("OLD FILE SIZE %d\n", READ_LE_UINT32(header + 28));
	printf("NEW FILE SIZE %d\n", READ_LE_UINT32(header + 32));
	printf("\n");
	printf("CTRL STREAM SIZE %d\n", READ_LE_UINT32(header + 36));
	printf("DIFF STREAM SIZE %d\n", READ_LE_UINT32(header + 40));
	printf("EXTRA STREAM SIZE %d\n", READ_LE_UINT32(header + 44));
	printf("\n");
}

typedef struct {
	char *oldfile;
	char *newfile;
	char *patchfile;
	bool show_info;
} arguments;

void show_usage(char *name) {
	printf("usage: %s [-a] oldfile newfile patchfile\n", name);
}

arguments parse_args(int argc, char *argv[]) {
	arguments arg;
	arg.show_info = false;

	int c;
	while ((c = getopt (argc, argv, "a")) != -1)
		switch (c) {
		case 'a':
			arg.show_info = true;
			break;
		case '?':
			show_usage(argv[0]);
			exit(0);
		default:
			fprintf (stderr, "Internal error\n");
			exit(1);
		}

	if (argc - optind < 3) {
		show_usage(argv[0]);
		exit(0);
	}

	arg.oldfile = argv[optind++];
	arg.newfile = argv[optind++];
	arg.patchfile = argv[optind++];

	return arg;
}

int main(int argc,char * argv[]) {
	uint32 oldsize, newsize;
	uint32 zctrllen, zdatalen, zextralen;
	uint8 header[48], buf[4];
	uint32 oldpos, newpos;
	uint32 ctrl[3];
	uint32 lenread;
	uint32 flags;
	uint8 md5[16];
	std::ifstream oldfile, patch, ctrlStream, diffStream, extraStream;
	std::ofstream newfile;
	bool comp_ctrl, mix;
	arguments args;

	old_block = 0;
	new_block = 0;
	atexit(free_memory);

	args = parse_args(argc, argv);

	/* Opens the old file */
	oldfile.open(args.oldfile, std::ios::in | std::ios::binary);
	if (oldfile.fail()) {
		std::cerr << "Unable to open" << args.oldfile << std::endl;
		return 1;
	}

	//Get the file size
	oldfile.seekg(0, std::ios::end);
	oldsize = oldfile.tellg();

	/* Open patch file */
	patch.open(args.patchfile, std::ios::in | std::ios::binary);
	ctrlStream.open(args.patchfile, std::ios::in | std::ios::binary);
	diffStream.open(args.patchfile, std::ios::in | std::ios::binary);
	extraStream.open(args.patchfile, std::ios::in | std::ios::binary);

	if (patch.fail() || ctrlStream.fail() || diffStream.fail() || extraStream.fail()) {
		std::cerr << "Unable to open " << args.patchfile << std::endl;
		return 1;
	}

	/* Read header */
	patch.read((char*)header, 48);
	if (patch.eof() || patch.bad() || patch.fail()) {
		std::cerr << "Corrupt patch\n";
		return 1;
	}

	/* Check for appropriate signature */
	if (READ_BE_UINT32(header) != MKTAG('P','A','T','R')) {
		std::cerr << "Corrupt patch\n";
		return 1;
	}

	/* Check the version */
	if (READ_LE_UINT16(header + 4) != 2 || READ_LE_UINT16(header + 6) > 0) {
		std::cerr << "Wrong version number\n";
		return 1;
	}

	//Set flags
	flags = READ_LE_UINT32(header + 8);
	mix = (flags & 1 << 0) ? true : false;
	comp_ctrl = (flags & 1 << 1) ? true : false;

	/* Check if the file to patch match */
	Common::md5_file(args.oldfile, md5, 5000);
	if (memcmp(md5, header + 12, 16) != 0 || oldsize != READ_LE_UINT32(header + 28)) {
		std::cerr << args.patchfile << " targets a different file\n";
		return 1;
	}

	/* Read lengths from header */
	newsize = READ_LE_UINT32(header + 32);
	zctrllen = READ_LE_UINT32(header + 36);
	zdatalen = READ_LE_UINT32(header + 40);
	zextralen = READ_LE_UINT32(header + 44);

	patch.close();
	if (args.show_info)
		show_header_info(header);

	// Open the compressed sub-streams
	//Check if the ctrl is compressed
	ctrlStream.seekg(48, std::ios::beg);
	if (comp_ctrl)
		ctrlDec = new GZipReadStream(&ctrlStream, 48, zctrllen);

	diffDec = new GZipReadStream(&diffStream, 48 + zctrllen, zdatalen);
	if (mix)
		extraDec = diffDec;
	else
		extraDec = new GZipReadStream(&extraStream, 48 + zctrllen + zdatalen, zextralen);

	old_block = new uint8[oldsize];
	new_block = new byte[newsize];
	if (old_block == NULL || new_block == NULL) {
		std::cerr << "Not enough memory\n";
		return 1;
	}
	
	//Read the oldfile
	oldfile.seekg(0, std::ios::beg);
	oldfile.read((char*)old_block, oldsize);
	oldfile.close();
	if (oldfile.bad() || oldfile.fail()) {
		std::cerr << "Input error\n";
		return 1;
	}

	oldpos=0;
	newpos=0;
	while(newpos < newsize) {
		/* Read control data */
		for (uint i = 0; i < 3; i++) {
			if (comp_ctrl)
				lenread = ctrlDec->read(buf, 4);
			else {
				ctrlStream.read((char*)buf, 4);
				lenread = ctrlStream.gcount();
			}
			if (lenread < 4) {
				std::cerr << "Corrupt patch\n";
				return 1;
			}
			ctrl[i] = READ_LE_UINT32(buf);
		};

		/* Sanity-check */
		if (newpos + ctrl[0] > newsize) {
			std::cerr << "Corrupt patch\n";
			return 1;
		}

		/* Read diff string */
		lenread = diffDec->read(new_block + newpos, ctrl[0]);
		if ((lenread < ctrl[0]) || diffDec->err()) {
			std::cerr << "Corrupt patch\n";
			return 1;
		}

		//Show info
		if (args.show_info && ctrl[0] > 0) {
			uint i = 0;
			while (i < ctrl[0]) {
				if (*(new_block + newpos + i) != 0) {
					printf("XOR");
					do {
						printf(" %02x", *(new_block + newpos + i));
						++i;
					} while (i < ctrl[0] && *(new_block + newpos + i) != 0);
					printf("\n");
				} else {
					uint pos = i;
					while (i < ctrl[0] && *(new_block + newpos + i) == 0)
						++i;
					printf("COPY %d\n", i - pos);
				}
			}
		}

		/* Add old data to diff string */
		for (uint i = 0; i < ctrl[0]; i++)
			if ((oldpos + i >= 0) && (oldpos + i < oldsize))
				new_block[newpos + i] ^= old_block[oldpos + i];

		/* Adjust pointers */
		newpos += ctrl[0];
		oldpos += ctrl[0];

		/* Sanity-check */
		if (newpos + ctrl[1] > newsize) {
			std::cerr << "Corrupt patch\n";
			return 1;
		}

		/* Read extra string */
		lenread = extraDec->read(new_block + newpos, ctrl[1]);
		if ((lenread < ctrl[1]) || extraDec->err()) {
			std::cerr << "Corrupt patch\n";
			return 1;
		}

		//Show info
		if (args.show_info) {
			if (ctrl[1] > 0) {
				printf("INSERT");
				for (uint i = 0; i < ctrl[1]; i++)
					printf(" %02x", *(new_block + newpos + i));
				printf("\n");
			}

			if (ctrl[2] != 0)
				printf("JUMP %d\n", ctrl[2]);
		}

		/* Adjust pointers */
		newpos += ctrl[1];
		oldpos += int32(ctrl[2]);
	};

	/* Clean up the bzip2 reads */
	ctrlStream.close();
	diffStream.close();
	extraStream.close();

	/* Write the new file */
	newfile.open(args.newfile, std::ios::out | std::ios::binary);
	if (newfile.fail()) {
		std::cerr << "Unable to open" << args.newfile << std::endl;
		return 1;
	}

	newfile.write((char*)new_block, newsize);
	if (patch.bad()) {
		std::cerr << "Output error.\n";
		return 1;
	}

	return 0;
}
