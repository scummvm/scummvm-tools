/* ResidualVM - A 3D game interpreter
 *
 * ResidualVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * A tool that converts Grim's bm images to bmp bitmaps.
 */

#include <fstream>
#include <iostream>
#include <string>
#include <zlib.h>
#include <cassert>
#include <sys/types.h>
#include <sstream>
#include <stdint.h>
#include <cstdio>
#include <cstring>
#include "common/endian.h"
#include "lab.h"

struct BMPHeader {
	uint32_t size;
	uint32_t reserved;
	uint32_t offset;
	uint32_t headerSize;
	int32_t width;
	int32_t height;
	uint32_t nplanesbpp;
	uint32_t compress_type;
	uint32_t bmp_bytesz;
	int32_t hres;
	int32_t vres;
	uint32_t ncolors;
	uint32_t nimpcolors;
};

class Bitmap {
public:
	~Bitmap();

	inline uint32_t size() const { return _height * _width * _bpp / 8; }

	static Bitmap *load(const char *data, int len);

	void toBMP(const std::string &fname);

private:
	Bitmap() { }

	int _numImages;
	char **_data;
	int _bpp;
	int _width;
	int _height;
};

Bitmap::~Bitmap() {
	for (int i = 0; i < _numImages; ++i) {
		delete[] _data[i];
	}
	delete[] _data;
}

void Bitmap::toBMP(const std::string &fname) {
	for (int img = 0; img < _numImages; ++img) {
		std::stringstream name;
		name << fname << '.' << img << ".bmp";
		printf("Saving image %d to file %s\n", img, name.str().c_str());

		std::fstream file(name.str().c_str(), std::fstream::out | std::fstream::binary);
		BMPHeader header;
		unsigned short bm = TO_LE_16(19778);
		file.write((char *)&bm, 2);
		header.size = TO_LE_32(size() + 54);
		header.reserved = TO_LE_32(0);
		header.width = TO_LE_32(_width);
		header.height = TO_LE_32(_height);
		header.offset = TO_LE_32(54);
		header.headerSize = TO_LE_32(40);
		if (_bpp == 32) {
			header.nplanesbpp = TO_LE_32(2097153);
		} else if (_bpp == 16) {
			header.nplanesbpp = TO_LE_32(1048577);
		}
		header.compress_type = TO_LE_32(0);
		header.bmp_bytesz = TO_LE_32(0);
		header.hres = TO_LE_32(2835);
		header.vres = TO_LE_32(2835);
		header.ncolors = TO_LE_32(0);
		header.nimpcolors = TO_LE_32(0);
		file.write((char *)&header, sizeof(BMPHeader));
		for (int i = _height - 1; i >= 0; --i) {
			char *d = _data[0] + (_width * i * _bpp / 8);
			file.write(d, _width * _bpp / 8);
		}
		file.close();
	}
}

#define GET_BIT do { bit = bitstr_value & 1; \
		bitstr_len--; \
		bitstr_value >>= 1; \
		if (bitstr_len == 0) { \
			bitstr_value = READ_LE_UINT16(compressed); \
			bitstr_len = 16; \
			compressed += 2; \
		} \
	} while (0)

static bool decompress_codec3(const byte *compressed, char *result, int maxBytes) {
	int bitstr_value = READ_LE_UINT16(compressed);
	int bitstr_len = 16;
	compressed += 2;
	bool bit;

	int byteIndex = 0;
	for (;;) {
		GET_BIT;
		if (bit == 1) {
			if (byteIndex >= maxBytes) {
				printf("Buffer overflow when decoding image: decompress_codec3 walked past the input buffer!\n");
				return false;
			} else {
				*result++ = *compressed++;
			}
			++byteIndex;
		} else {
			GET_BIT;
			int copy_len, copy_offset;
			if (bit == 0) {
				GET_BIT;
				copy_len = 2 * bit;
				GET_BIT;
				copy_len += bit + 3;
				copy_offset = *(compressed++) - 0x100;
			} else {
				copy_offset = (*compressed | (*(compressed + 1) & 0xf0) << 4) - 0x1000;
				copy_len = (*(compressed + 1) & 0xf) + 3;
				compressed += 2;
				if (copy_len == 3) {
					copy_len = *(compressed++) + 1;
					if (copy_len == 1) {
						return true;
					}
				}
			}
			while (copy_len > 0) {
				if (byteIndex >= maxBytes) {
					printf("Buffer overflow when decoding image: decompress_codec3 walked past the input buffer!\n");
					return false;
				} else {
					assert(byteIndex + copy_offset >= 0);
					assert(byteIndex + copy_offset < maxBytes);
					*result = result[copy_offset];
					result++;
				}
				++byteIndex;
				copy_len--;
			}
		}
	}
	return true;
}

Bitmap *Bitmap::load(const char *data, int len) {
	if (len < 8 || memcmp(data, "BM  F\0\0\0", 8) != 0) {
		printf("Invalid magic loading bitmap.\n");
		return NULL;
	}

	Bitmap *b = new Bitmap();

	int codec = READ_LE_UINT32(data + 8);
//	_paletteIncluded = READ_LE_UINT32(data + 12);
	b->_numImages = READ_LE_UINT32(data + 16);
//	int x = READ_LE_UINT32(data + 20);
//	int y = READ_LE_UINT32(data + 24);
//	_transparentColor = READ_LE_UINT32(data + 28);
	int format = READ_LE_UINT32(data + 32);

	if (format != 1) {
		printf("ZBuffer images are not supported.\n");
		return NULL;
	}

	b->_bpp = READ_LE_UINT32(data + 36);
//	_blueBits = READ_LE_UINT32(data + 40);
//	_greenBits = READ_LE_UINT32(data + 44);
//	_redBits = READ_LE_UINT32(data + 48);
//	_blueShift = READ_LE_UINT32(data + 52);
//	_greenShift = READ_LE_UINT32(data + 56);
//	_redShift = READ_LE_UINT32(data + 60);
	b->_width = READ_LE_UINT32(data + 128);
	b->_height = READ_LE_UINT32(data + 132);

	b->_data = new char *[b->_numImages];
	int pos = 0x88;
	for (int i = 0; i < b->_numImages; i++) {
		b->_data[i] = new char[b->_bpp / 8 * b->_width * b->_height];
		if (codec == 0) {
			memcpy(b->_data[i], data + pos, b->_bpp / 8 * b->_width * b->_height);
			pos += b->_bpp / 8 * b->_width * b->_height + 8;
		} else if (codec == 3) {
			int compressed_len = READ_LE_UINT32(data + pos);
			bool success = decompress_codec3((const byte *)data + pos + 4, b->_data[i], b->_bpp / 8 * b->_width * b->_height);
			if (!success) {
				printf(".. when loading image\n");
			}
			char *temp = new char[b->_bpp / 8 * b->_width * b->_height];
			memcpy(temp, b->_data[i], b->_bpp / 8 * b->_width * b->_height);
			delete[] b->_data[i];
			b->_data[i] = temp;
			pos += compressed_len + 12;
		} else {
			printf("Unknown image codec in BitmapData ctor!\n");
			return NULL;
		}

#ifdef SCUMM_BIG_ENDIAN
		for (int j = 0; j < b->_width * b->_height; ++j) {
			((uint16 *)b->_data[i])[j] = SWAP_16(((uint16 *)b->_data[i])[j]);
		}
#endif
	}

	for (int img = 0; img < b->_numImages; img++) {
		char *texData = new char[4 * b->_width * b->_height];
		// Convert data to 32-bit RGBA format
		char *texDataPtr = texData;
		uint16 *bitmapData = reinterpret_cast<uint16 *>(b->_data[img]);
		for (int i = 0; i < b->_width * b->_height; i++, texDataPtr += 4, bitmapData++) {
			uint16 pixel = *bitmapData;
			int r = pixel >> 11;
			int g = (pixel >> 5) & 0x3f;
			int bb = pixel & 0x1f;
			texDataPtr[2] = (r << 3) | (r >> 2);
			texDataPtr[1] = (g << 2) | (g >> 4);
			texDataPtr[0] = (bb << 3) | (bb >> 2);
		}
		delete[] b->_data[img];
		b->_data[img] = texData;
		b->_bpp = 32;
	}

	return b;
}

void usage() {
	std::cout << "Usage: bm2bmp [labfilename] <filename>" << std::endl;
}

int main(int argc, char **argv) {
	if (argc < 2) {
		std::cout << "No Argument" << std::endl;
		usage();
		return 0;
	}
	if (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) {
		usage();
		return 0;
	}

	Lab *lab = NULL;
	std::string filename;
	int length = 0;

	if (argc > 2) {
		lab = new Lab(argv[1]);
		filename = argv[2];
	} else {
		filename = argv[1];
	}

	std::istream *file = getFile(filename, lab, length);

	if (!file) {
		std::cout << "Could not open file" << std::endl;
		return 1;
	}

	int p = filename.rfind('/');
	std::string outname = filename.substr(p + 1);

	char *data = new char[length];
	file->read(data, length);
	delete file;
	Bitmap *b = Bitmap::load(data, length);
	if (b) {
		b->toBMP(filename.substr(p + 1));
		delete b;
	} else {
		printf("Could not load file %s.\n", filename.c_str());
		return 1;
	}

	delete[] data;
	return 0;
}
