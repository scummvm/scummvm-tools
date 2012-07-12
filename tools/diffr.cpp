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
#include "common/endian.h"
#include "common/zlib.h"
#include "common/md5.h"
#include "common/getopt.h"

#define MIN(x,y) (((x)<(y)) ? (x) : (y))

static void split(int32 *I, int32 *V, int32 start, int32 len, int32 h) {
	int32 i, j, k, x, tmp, jj, kk;

	if (len < 16) {
		for (k = start; k < start + len; k += j) {
			j = 1;
			x = V[I[k] + h];
			for (i = 1; k + i < start + len; i++) {
				if (V[I[k + i] + h] < x) {
					x = V[I[k + i] + h];
					j = 0;
				};
				if (V[I[k + i] + h] == x) {
					tmp = I[k + j];
					I[k + j] = I[k + i];
					I[k + i] = tmp;
					j++;
				};
			};
			for (i = 0; i < j; i++) V[I[k + i]] = k + j - 1;
			if (j == 1) I[k] = -1;
		};
		return;
	};

	x = V[I[start + len/2] + h];
	jj = 0;
	kk = 0;
	for (i = start; i < start + len; i++) {
		if (V[I[i] + h] < x) jj++;
		if (V[I[i] + h] == x) kk++;
	};
	jj += start;
	kk += jj;

	i = start;
	j = 0;
	k = 0;
	while (i < jj) {
		if (V[I[i] + h] < x) {
			i++;
		} else if (V[I[i] + h] == x) {
			tmp = I[i];
			I[i] = I[jj + j];
			I[jj + j] = tmp;
			j++;
		} else {
			tmp = I[i];
			I[i] = I[kk + k];
			I[kk + k] = tmp;
			k++;
		};
	};

	while (jj + j < kk) {
		if (V[I[jj + j] + h] == x) {
			j++;
		} else {
			tmp = I[jj + j];
			I[jj + j] = I[kk + k];
			I[kk + k] = tmp;
			k++;
		};
	};

	if (jj > start) split(I, V, start, jj - start, h);

	for (i = 0; i < kk - jj; i++) V[I[jj + i]] = kk - 1;
	if (jj == kk - 1) I[jj] = -1;

	if (start + len > kk) split(I, V, kk, start + len - kk, h);
}

static void qsufsort(int32 *I, int32 *V, byte *old, int32 oldsize) {
	int32 buckets[256];
	int32 i, h, len;

	for (i = 0; i < 256; i++) buckets[i] = 0;
	for (i = 0; i < oldsize; i++) buckets[old[i]]++;
	for (i = 1; i < 256; i++) buckets[i] += buckets[i-1];
	for (i = 255; i > 0; i--) buckets[i] = buckets[i-1];
	buckets[0] = 0;

	for (i = 0; i < oldsize; i++) I[++buckets[old[i]]] = i;
	I[0] = oldsize;
	for (i = 0; i < oldsize; i++) V[i] = buckets[old[i]];
	V[oldsize] = 0;
	for (i = 1; i < 256; i++) if (buckets[i] == buckets[i-1] + 1) I[buckets[i]] = -1;
	I[0] = -1;

	for (h = 1; I[0] != -(oldsize + 1); h += h) {
		len = 0;
		for (i = 0; i < oldsize + 1;) {
			if (I[i] < 0) {
				len -= I[i];
				i -= I[i];
			} else {
				if (len) I[i-len] = -len;
				len = V[I[i]] + 1 - i;
				split(I, V, i, len, h);
				i += len;
				len = 0;
			};
		};
		if (len) I[i-len] = -len;
	};

	for (i = 0; i < oldsize + 1; i++)
		I[V[i]] = i;
}

static int32 matchlen(byte *old, int32 oldsize, byte *new_block, int32 new_size) {
	int32 i;

	for (i = 0; (i < oldsize) && (i < new_size); i++)
		if (old[i] != new_block[i])
			break;

	return i;
}

static int32 search(int32 *I, byte *old, int32 oldsize,
                    byte *new_block, int32 newsize, int32 st, int32 en, int32 *pos) {
	int32 x, y;

	if (en - st < 2) {
		x = matchlen(old + I[st], oldsize - I[st], new_block, newsize);
		y = matchlen(old + I[en], oldsize - I[en], new_block, newsize);

		if (x > y) {
			*pos = I[st];
			return x;
		} else {
			*pos = I[en];
			return y;
		}
	};

	x = st + (en - st) / 2;
	if (memcmp(old + I[x], new_block, MIN(oldsize - I[x], newsize)) < 0) {
		return search(I, old, oldsize, new_block, newsize, x, en, pos);
	} else {
		return search(I, old, oldsize, new_block, newsize, st, x, pos);
	};
}

typedef struct {
	char *oldfile;
	char *newfile;
	char *patchfile;
	bool mix;
	bool comp_ctrl;
} arguments;

void show_usage(char *name) {
	printf("usage: %s [-m][-n] oldfile newfile patchfile\n", name);
}

arguments parse_args(int argc, char *argv[]) {
	arguments arg;
	arg.comp_ctrl = true;
	arg.mix = false;

	int c;
	while ((c = getopt (argc, argv, "nm")) != -1)
		switch (c) {
		case 'n':
			arg.comp_ctrl = false;
			break;
		case 'm':
			arg.mix = true;
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

int main(int argc, char *argv[]) {
	byte *old, *new_block;
	int32 oldsize, newsize, newsize2;
	int32 *I, *V;
	int32 scan, pos, len;
	int32 lastscan, lastpos, lastoffset;
	int32 oldscore, scsc;
	int32 s, Sf, lenf, Sb, lenb;
	int32 overlap, Ss, lens;
	int32 i;
	int32 dblen, eblen;
	uint32 flags = 0;
	byte *db, *eb;
	byte buf[4];
	byte header[48];
	std::ofstream patch;
	std::ifstream in;
	arguments args;

	args = parse_args(argc, argv);

	//Set flags
	if (args.mix)
		flags |= 1 << 0;
	if (args.comp_ctrl)
		flags |= 1 << 1;

	/* Allocate oldsize+1 bytes instead of oldsize bytes to ensure
	    that we never try to alloc zero elements and get a NULL pointer */

	//Read old file
	in.open(args.oldfile, std::ios::in | std::ios::binary);
	if (in.fail()) {
		std::cerr << "Unable to open " << args.oldfile << std::endl;
		return 1;
	}
	in.seekg(0, std::ios::end);
	oldsize = in.tellg();
	in.seekg(0);
	if ((old = new byte[oldsize + 1]) == NULL) {
		std::cerr << "Unable to allocate memory" << std::endl;
		return 1;
	}
	in.read((char*)old, oldsize);
	if (in.fail()) {
		std::cerr << "Unable to read from " << args.oldfile << std::endl;
		return 1;
	}
	in.close();

	I = new int32[oldsize + 1];
	V = new int32[oldsize + 1];
	if (I == NULL || V == NULL) {
		std::cerr << "Unable to allocate memory" << std::endl;
		return 1;
	}
	qsufsort(I, V, old, oldsize);

	delete[] V;

	//Read new file
	in.open(args.newfile, std::ios::in | std::ios::binary);
	if (in.fail()) {
		std::cerr << "Unable to open " << args.newfile << std::endl;
		return 1;
	}
	in.seekg(0, std::ios::end);
	newsize = in.tellg();
	in.seekg(0);
	if ((new_block = new byte[newsize + 1]) == NULL) {
		std::cerr << "Unable to allocate memory" << std::endl;
		return 1;
	}
	in.read((char*)new_block, newsize);
	if (in.fail()) {
		std::cerr << "Unable to read from " << args.newfile << std::endl;
		return 1;
	}
	in.close();


	db = new byte[newsize + 1];
	if (db == NULL) {
		std::cerr << "Unable to allocate memory" << std::endl;
		return 1;
	}
	if (!args.mix) {
		eb = new byte[newsize + 1];
		if (eb == NULL) {
			std::cerr << "Unable to allocate memory" << std::endl;
			return 1;
		}
	}
	dblen = 0;
	eblen = 0;

	/* Create the patch file */
	patch.open(args.patchfile, std::ios::out | std::ios::binary);
	if (patch.fail()) {
		std::cerr << "Unable to open " << args.patchfile << std::endl;
		return 1;
	}

	memcpy(header, "PATR", 4);							//Signature
	WRITE_LE_UINT16(header + 4, 2);						//Version major
	WRITE_LE_UINT16(header + 6, 0);						//Version minor
	WRITE_LE_UINT32(header + 8, flags);					//flags
	Common::md5_file(args.oldfile, header + 12, 5000);	//Md5sum
	WRITE_LE_UINT32(header + 28, oldsize);				//oldsize
	WRITE_LE_UINT32(header + 32, newsize);				//newsize
	//WRITE_LE_UINT32(header + 36, 0);					//ctrl compressed size
	//WRITE_LE_UINT32(header + 40, 0);					//diff compressed size
	//WRITE_LE_UINT32(header + 44, 0);					//extra compressed size
	patch.write((char *)header, 48);
	if (patch.bad()) {
		std::cerr << "Write error on " << args.patchfile << std::endl;
		return 1;
	}

	/* Compute the differences, writing ctrl as we go */
	GZipWriteStream *ctrlBlock;
	if (args.comp_ctrl)
		ctrlBlock = new GZipWriteStream(&patch);

	scan = 0;
	len = 0;
	lastscan = 0;
	lastpos = 0;
	lastoffset = 0;
	while (scan < newsize) {
		oldscore = 0;

		for (scsc = scan += len; scan < newsize; scan++) {
			len = search(I, old, oldsize, new_block + scan, newsize - scan,
			             0, oldsize, &pos);

			for (; scsc < scan + len; scsc++)
				if ((scsc + lastoffset < oldsize) &&
				        (old[scsc + lastoffset] == new_block[scsc]))
					oldscore++;

			if (((len == oldscore) && (len != 0)) ||
			        (len > oldscore + 8)) break;

			if ((scan + lastoffset < oldsize) &&
			        (old[scan + lastoffset] == new_block[scan]))
				oldscore--;
		};

		if ((len != oldscore) || (scan == newsize)) {
			s = 0;
			Sf = 0;
			lenf = 0;
			for (i = 0; (lastscan + i < scan) && (lastpos + i < oldsize);) {
				if (old[lastpos + i] == new_block[lastscan + i]) s++;
				i++;
				if (s * 2 - i > Sf * 2 - lenf) {
					Sf = s;
					lenf = i;
				};
			};

			lenb = 0;
			if (scan < newsize) {
				s = 0;
				Sb = 0;
				for (i = 1; (scan >= lastscan + i) && (pos >= i); i++) {
					if (old[pos - i] == new_block[scan - i]) s++;
					if (s * 2 - i > Sb * 2 - lenb) {
						Sb = s;
						lenb = i;
					};
				};
			};

			if (lastscan + lenf > scan - lenb) {
				overlap = (lastscan + lenf) - (scan - lenb);
				s = 0;
				Ss = 0;
				lens = 0;
				for (i = 0; i < overlap; i++) {
					if (new_block[lastscan + lenf - overlap + i] ==
					        old[lastpos + lenf - overlap + i]) s++;
					if (new_block[scan - lenb + i] ==
					        old[pos - lenb + i]) s--;
					if (s > Ss) {
						Ss = s;
						lens = i + 1;
					};
				};

				lenf += lens - overlap;
				lenb -= lens;
			};

			for (i = 0; i < lenf; i++)
				db[dblen + i] = new_block[lastscan + i] ^ old[lastpos + i];
			dblen += lenf;

			if (!args.mix) {
				for (i = 0; i < (scan - lenb) - (lastscan + lenf); i++)
					eb[eblen + i] = new_block[lastscan + lenf + i];
				eblen += (scan - lenb) - (lastscan + lenf);
			} else {
				for (i = 0; i < (scan - lenb) - (lastscan + lenf); i++)
					db[dblen + i] = new_block[lastscan + lenf + i];
				dblen += (scan - lenb) - (lastscan + lenf);
			}

			WRITE_LE_UINT32(buf, lenf);
			if (args.comp_ctrl) {
				ctrlBlock->write(buf, 4);
				if (ctrlBlock->err()) {
					std::cerr << "Write error on " << args.patchfile << std::endl;
					return 1;
				}
			} else
				patch.write((char*)buf, 4);


			WRITE_LE_UINT32(buf, (scan - lenb) - (lastscan + lenf));
			if (args.comp_ctrl) {
				ctrlBlock->write(buf, 4);
				if (ctrlBlock->err()) {
					std::cerr << "Write error on " << args.patchfile << std::endl;
					return 1;
				}
			} else
				patch.write((char*)buf, 4);

			WRITE_LE_UINT32(buf, int32((pos - lenb) - (lastpos + lenf)));
			if (args.comp_ctrl) {
				ctrlBlock->write(buf, 4);
				if (ctrlBlock->err()) {
					std::cerr << "Write error on " << args.patchfile << std::endl;
					return 1;
				}
			} else
				patch.write((char*)buf, 4);

			lastscan = scan - lenb;
			lastpos = pos - lenb;
			lastoffset = pos - scan;
		};
	};
	if (args.comp_ctrl)
		delete ctrlBlock;

	/* Compute size of ctrl data (compressed or not)*/
	if ((len = patch.tellp()) == -1) {
		std::cerr << "Read error on " << args.patchfile << std::endl;
		return 1;
	}

	WRITE_LE_UINT32(header + 36, len - 48);

	/* Write compressed diff data */
	GZipWriteStream *diffBlock = new GZipWriteStream(&patch);
	diffBlock->write(db, dblen);
	if (diffBlock->err()) {
		std::cerr << "Write error on " << args.patchfile << std::endl;
		return 1;
	}
	delete diffBlock;

	/* Compute size of compressed diff data */
	if ((newsize = patch.tellp()) == -1) {
		std::cerr << "Read error on " << args.patchfile << std::endl;
		return 1;
	}
	WRITE_LE_UINT32(header + 40, newsize - len);

	/* Write compressed extra data */
	if (!args.mix) {
		GZipWriteStream *extraBlock = new GZipWriteStream(&patch);
		extraBlock->write(eb, eblen);
		if (extraBlock->err()) {
			std::cerr << "Write error on " << args.patchfile << std::endl;
			return 1;
		}
		delete extraBlock;

		/* Compute size of compressed extra data */
		if ((newsize2 = patch.tellp()) == -1) {
			std::cerr << "Read error on " << args.patchfile << std::endl;
			return 1;
		}
		WRITE_LE_UINT32(header + 44, newsize2 - newsize);

		delete[] eb;
	}
	else
		WRITE_LE_UINT32(header + 44, 0);

	/* Seek to the beginning, write the header, and close the file */
	patch.seekp(0, std::ios::beg);
	patch.write((char *)header, 48);
	if (patch.bad()) {
		std::cerr << "Write error on " << args.patchfile << std::endl;
		return 1;
	}
	patch.close();

	/* Free the memory we used */
	delete[] db;
	delete[] I;
	delete[] old;
	delete[] new_block;

	return 0;
}
