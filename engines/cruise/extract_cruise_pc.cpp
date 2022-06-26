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

/* Extract data from PC version of Cruise for a Corpse */

#include <stdarg.h>
#include <string.h>

#include "extract_cruise_pc.h"

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

	Common::File *_fp;

	unsigned int _bitsBuffer;
	int _bitsLeft;

	Disk1Stream(Common::File *fp) : _fp(fp), _bitsBuffer(0), _bitsLeft(0) {}

	bool readHeader(Disk1Header *hdr) {
		for (int i = 0; i < 4; i++)
			hdr->signature[i] = _fp->readByte();
		hdr->headerSize = _fp->readSint16LE();
		hdr->uncompressedSize = _fp->readSint32LE();
		hdr->compressedSize = _fp->readSint32LE();
		for (int i = 0; i < 14; i++)
			hdr->name[i] = _fp->readByte();
		hdr->creationTime = _fp->readSint16LE();
		hdr->creationDate = _fp->readSint16LE();
		return memcmp(hdr->signature, "PKD\0", 4) == 0 && hdr->headerSize == 32;
	}
	int getBits(int count) {
		while (_bitsLeft <= 24) {
			int chr = _fp->readByte();
			if (_fp->eos()) chr = 0;
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
	Common::File *_outputFile;

	Disk1Decoder(Disk1Stream *stream, Common::File *output, int uncompressedSize)
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
				while (_freq[++index] < i) {}
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
				_outputFile->writeByte(chr & 255);
				_historyBuffer[offset++] = chr;
				offset &= 0xFFF;
				++currentSize;
			} else {
				const int baseOffset = (offset - getHuffCode() - 1) & 0xFFF;
				const int size = chr - 253;
				for (int i = 0; i < size; ++i) {
					chr = _historyBuffer[(baseOffset + i) & 0xFFF];
					_outputFile->writeByte(chr & 255);
					_historyBuffer[offset++] = chr;
					offset &= 0xFFF;
					++currentSize;
				}
			}
		}
		return currentSize == _uncompressedSize;
	}
};

ExtractCruisePC::ExtractCruisePC(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Extract data files from the PC version of Cruise for a Corpse.";
	_helptext = "Usage: " + _name + " [-o outputdir] <infile>\n" + _shorthelp + "\n";
}

void ExtractCruisePC::execute() {
	if (_outputPath.empty())
		_outputPath.setFullPath("./");

	Common::File input(_inputPaths[0].path, "rb");

	Disk1Stream stream(&input);
	Disk1Header hdr;
	if (!stream.readHeader(&hdr)) {
		error("Invalid file signature");
	}
	print("Output filename '%s'", hdr.name);
	print("Date %02d/%02d/%04d", hdr.creationDate & 31, (hdr.creationDate >> 5) & 15, ((hdr.creationDate >> 9) & 127) + 1980);
	print("Time %02d:%02d:%02d", (hdr.creationTime >> 11) & 31, (hdr.creationTime >> 5) & 63, (hdr.creationTime & 31) << 1);
	print("Size %d (uncompressed %d)", hdr.compressedSize, hdr.uncompressedSize);

	_outputPath.setFullName(hdr.name);
	Common::File output(_outputPath, "wb");

	Disk1Decoder d(&stream, &output, hdr.uncompressedSize);
	print("Decompressing...");
	if (d.decode()) {
		print("Ok");
	} else {
		print("Error");
	}
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractCrsuisePC cruise(argv[0]);
	return cruise.run(argc, argv);
}
#endif
