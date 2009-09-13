/* extract_cruise_pc
 * Copyright (C) 2009  The ScummVM Team
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *
 */
#include "util.h"

struct Disk1Header {
	unsigned char signature[4];
	int headerSize;
	int uncompressedSize;
	int compressedSize;
	char name[14];
	int creationTime;
	int creationDate;
};

struct Disk1Stream {

	FILE *_fp;

	unsigned int _bitsBuffer;
	int _bitsLeft;

	Disk1Stream(FILE *fp) : _fp(fp), _bitsBuffer(0), _bitsLeft(0) {}

	bool readHeader(Disk1Header *hdr) {
		fread(hdr->signature, 4, 1, _fp);
		hdr->headerSize = getInt(2);
		hdr->uncompressedSize = getInt(4);
		hdr->compressedSize = getInt(4);
		fread(hdr->name, 14, 1, _fp);
		hdr->creationTime = getInt(2);
		hdr->creationDate = getInt(2);
		return memcmp(hdr->signature, "PKD\0", 4) == 0 && hdr->headerSize == 32;
	}
	int getInt(int e) {
		int num = 0;
		for (int i = 0; i < e; ++i) {
			num |= fgetc(_fp) << (i * 8);
		}
		return num;
	}
	int getBits(int count) {
		while (_bitsLeft <= 24) {
			int chr = fgetc(_fp);
			if (feof(_fp)) chr = 0;
			_bitsBuffer |= chr << (24 - _bitsLeft);
			_bitsLeft += 8;
		}
		const unsigned int bits = _bitsBuffer >> (32 - count);
		_bitsBuffer = (_bitsBuffer << count) & 0xFFFFFFFF;
		_bitsLeft -= count;
		return bits;
	}
};

struct Disk1Decoder { // LzHuffman

	enum {
		kCharsCount = 314,
		kTableSize = kCharsCount * 2 - 1,
		kHuffmanRoot = kTableSize - 1,
		kMaxFreq = 0x8000
	};

	Disk1Stream *_stream;
	int _child[kTableSize];
	int _freq[628];
	int _parent[943];
	unsigned char _historyBuffer[4156];
	int _uncompressedSize;
	FILE *_outputFile;

	Disk1Decoder(Disk1Stream *stream, FILE *output, int uncompressedSize)
		: _stream(stream), _outputFile(output), _uncompressedSize(uncompressedSize) {
		memset(_child, 0, sizeof(_child));
		memset(_freq, 0, sizeof(_freq));
		memset(_parent, 0, sizeof(_parent));
		memset(_historyBuffer, 0, sizeof(_historyBuffer));
	}

	void resetHuffTables() {
		for (int i = 0; i < kCharsCount; ++i) {
			_freq[i] = 1;
			_child[i] = kTableSize + i;
			_parent[kTableSize + i] = i;
		}
		for (int i = 0, j = kCharsCount; j <= kHuffmanRoot; i += 2, ++j) {
			_freq[j] = _freq[i] + _freq[i + 1];
			_child[j] = i;
			_parent[i] = _parent[i + 1] = j;
		}
		_freq[kTableSize] = 0xFFFF;
		_parent[kHuffmanRoot] = 0;
	}
	int getHuffCode() {
        	static const int base[] = { 0, 1, 4, 12, 24, 48 };
	        static const int count[] = { 0, 2, 5, 9, 12, 15 };
		static const int length[] = { 0, 0, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5 };
		int index = _stream->getBits(8);
		const int len = length[index >> 4];
		const int code = base[len] + (index - count[len] * 16) / (1 << (5 - len));
		for (int i = 0; i <= len; ++i) {
			index = (index << 1) | _stream->getBits(1);
		}
		return (index & 63) | (code << 6);
	}
	int decodeChar() {
		int i = _child[kHuffmanRoot];
		while (i < kTableSize) {
			i += _stream->getBits(1);
			i = _child[i];
		}
		i -= kTableSize;
		update(i);
		return i;
	}
	void update(int num) {
		if (_freq[kHuffmanRoot] == kMaxFreq) {
			for (int j = 0, i = 0; j < kTableSize; ++j) {
				if (_child[j] >= kTableSize) {
					_freq[i] = (_freq[j] + 1) >> 1;
					_child[i] = _child[j];
					++i;
				}
			}
			for (int j = 0, i = kCharsCount; i < kTableSize; j += 2, ++i) {
				const int f = _freq[i] = _freq[j] + _freq[j + 1];
				int index = i - 1;
				while (_freq[index] > f) {
					--index;
				}
				++index;
				const int copySize = (i - index) * sizeof(int);
				memmove(_freq + index + 1, _freq + index, copySize);
				_freq[index] = f;
				memmove(_child + index + 1, _child + index, copySize);
				_child[index] = j;
			}
			for (int i = 0; i < kTableSize; ++i) {
				const int j = _child[i];
				if (j >= kTableSize) {
					_parent[j] = i;
				} else {
					_parent[j] = _parent[j + 1] = i;
				}
			}
		}
		int p = _parent[kTableSize + num];
		do {
			++_freq[p];
			const int i = _freq[p];
			int index = p + 1;
			if (_freq[index] < i) {
				while (_freq[++index] < i);
				--index;
				_freq[p] = _freq[index];
				_freq[index] = i;
				const int k = _child[p];
				_parent[k] = index;
				if (k < kTableSize) {
					_parent[k + 1] = index;
				}
				const int j = _child[index];
				_child[index] = k;
				_parent[j] = p;
				if (j < kTableSize) {
					_parent[j + 1] = p;
				}
				_child[p] = j;
				p = index;
			}
			p = _parent[p];
		} while (p != 0);		
	}
	bool decode() {
		resetHuffTables();
		for (int i = 0; i < 4036; ++i) {
			_historyBuffer[i] = ' ';
		}
		int offset = 4036;
		int currentSize = 0;
		while (currentSize < _uncompressedSize) {
			int chr = decodeChar();
			if (chr < 256) {
				fputc(chr & 255, _outputFile);
				_historyBuffer[offset++] = chr;
				offset &= 0xFFF;
				++currentSize;
			} else {
				const int baseOffset = (offset - getHuffCode() - 1) & 0xFFF;
				const int size = chr - 253;
				for (int i = 0; i < size; ++i) {
					chr = _historyBuffer[(baseOffset + i) & 0xFFF];
					fputc(chr & 255, _outputFile);
					_historyBuffer[offset++] = chr;
					offset &= 0xFFF;
					++currentSize;
				}
			}
		}
		return currentSize == _uncompressedSize;
	}
};

int main(int argc, char *argv[]) {
	if (argc != 2) {
		printf("usage: %s FILE\n", argv[0]);
		exit(2);
	}
	FILE *input = fopen(argv[1], "rb");
	if (!input) {
		error("Unable to open '%s' for reading", argv[1]);
	}
	Disk1Stream stream(input);
	Disk1Header hdr;
	if (!stream.readHeader(&hdr)) {
		error("Invalid file signature");
	}
	printf("Output filename '%s'\n", hdr.name);
	printf("Date %02d/%02d/%04d\n", hdr.creationDate & 31, (hdr.creationDate >> 5) & 15, ((hdr.creationDate >> 9) & 127) + 1980);
	printf("Time %02d:%02d:%02d\n", (hdr.creationTime >> 11) & 31, (hdr.creationTime >> 5) & 63, (hdr.creationTime & 31) << 1);
	printf("Size %d (uncompressed %d)\n", hdr.compressedSize, hdr.uncompressedSize);
	FILE *output = fopen(hdr.name, "wb");
	if (!output) {
		error("Unable to open '%s' for writing", hdr.name);
	}
	Disk1Decoder d(&stream, output, hdr.uncompressedSize);
	printf("Decompressing...");
	if (d.decode()) {
		printf("Ok\n");
	} else {
		printf("Error\n");
	}
	fclose(input);
	fclose(output);
	return 0;
}

