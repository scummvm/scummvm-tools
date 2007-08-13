/* encode_dxa - compressor for dxa files
 * Copyright (c) 2006 The ScummVM Team
 * Copyright (c) 2006 Benjamin Haisch
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

#include "compress.h"
#include "util.h"

#include <png.h>
#include <zlib.h>

const uint32 typeDEXA = 0x41584544;
const uint32 typeFRAM = 0x4d415246;
const uint32 typeWAVE = 0x45564157;
const uint32 typeCMAP = 0x50414D43;
const uint32 typeNULL = 0x4C4C554E;

#define	 BUFFER_LEN	1024

// other block dimensions than 4x4 are not really supported yet
#define  BLOCKW	4
#define  BLOCKH	4

static CompressMode gCompMode = kMP3Mode;

enum ScaleMode { S_NONE, S_INTERLACED, S_DOUBLE };

struct DiffStruct {
	uint16 map;
	int count;
	byte pixels[BLOCKW*BLOCKH];
};

class DxaEncoder {
private:
	FILE *_dxa;
	int _width, _height, _framerate, _framecount, _workheight;
	uint8 *_prevframe, *_prevpalette;
	ScaleMode _scaleMode;

	byte *_codeBuf, *_dataBuf, *_motBuf, *_maskBuf;
	void grabBlock(byte *frame, int x, int y, int blockw, int blockh, byte *block);
	bool m13blocksAreEqual(byte *frame, int x, int y, int x2, int y2, int w, int h);
	bool m13blockIsSolidColor(byte *frame, int x, int y, int w, int h, byte &color);
	void m13blockDelta(byte *frame, int x, int y, int x2, int y2, DiffStruct &diff);
	bool m13motionVector(byte *frame, int x, int y, int w, int h, int &mx, int &my);
	int m13countColors(byte *block, byte *pixels, unsigned long &code, int &codeSize);
	uLong m13encode(byte *frame, byte *outbuf);

public:
	DxaEncoder(char *filename, int width, int height, int framerate, ScaleMode scaleMode);
	~DxaEncoder();
	void writeHeader();
	void writeNULL();
	void writeFrame(uint8 *frame, uint8 *palette);
};

DxaEncoder::DxaEncoder(char *filename, int width, int height, int framerate, ScaleMode scaleMode) {
	_dxa = fopen(filename, "wb");
	_width = width;
	_height = height;
	_framerate = framerate;
	_framecount = 0;
	_prevframe = new uint8[_width * _height];
	_prevpalette = new uint8[768];
	_scaleMode = scaleMode;
	_workheight = _scaleMode == S_NONE ? _height : _height / 2;

	_codeBuf = new byte[_width * _height / 16];
	_dataBuf = new byte[_width * _height];
	_motBuf = new byte[_width * _height];
	_maskBuf = new byte[_width * _height];

	writeHeader();
}

DxaEncoder::~DxaEncoder() {
	fseek(_dxa, 0, SEEK_SET);

	writeHeader();

	fclose(_dxa);

	delete[] _codeBuf;
	delete[] _dataBuf;
	delete[] _motBuf;
	delete[] _maskBuf;

	delete[] _prevframe;
	delete[] _prevpalette;
}

void DxaEncoder::writeHeader() {
	//DEXA
	uint8 version = 0;

	/* remember the scaling mode */
	if (_scaleMode == S_INTERLACED)
		version |= 0x80;
	else if (_scaleMode == S_DOUBLE)
		version |= 0x40;

	writeUint32LE(_dxa, typeDEXA);
	writeByte(_dxa, version);

	writeUint16BE(_dxa, _framecount);
	writeUint32BE(_dxa, _framerate);
	writeUint16BE(_dxa, _width);
	writeUint16BE(_dxa, _height);
}

void DxaEncoder::writeNULL() {
	//NULL
	writeUint32LE(_dxa, typeNULL);
}

void DxaEncoder::writeFrame(byte *frame, byte *palette) {

	if (_framecount == 0 || memcmp(_prevpalette, palette, 768)) {
		writeUint32LE(_dxa, typeCMAP);
		fwrite(palette, 768, 1, _dxa);
		memcpy(_prevpalette, palette, 768);
	} else {
		writeNULL();
	}

	if (_framecount == 0 || memcmp(_prevframe, frame, _width * _workheight)) {
		//FRAM
		byte compType;

		writeUint32LE(_dxa, typeFRAM);

		if (_framecount == 0)
			compType = 2;
		else
			compType = 13;

		switch (compType) {

		case 2:
			{
				uLong outsize = _width * _workheight;
				byte *outbuf = new byte[outsize];
				compress2(outbuf, &outsize, frame, _width * _workheight, 9);
				writeByte(_dxa, compType);
				writeUint32BE(_dxa, outsize);
				fwrite(outbuf, outsize, 1, _dxa);
				delete[] outbuf;
				break;
			}

		case 13:
			{
				int r;
				uLong frameoutsize;
				byte *frameoutbuf;

				byte *xorbuf = new byte[_width * _workheight];
				uLong xorsize_z = _width * _workheight;
				byte *xorbuf_z = new byte[xorsize_z];

				uLong rawsize_z = _width * _workheight;
				byte *rawbuf_z = new byte[rawsize_z];

				uLong m13size = _width * _workheight * 2;
				byte *m13buf = new byte[m13size];
				uLong m13size_z = _width * _workheight;
				byte *m13buf_z = new byte[m13size_z];

				/* encode the delta frame with mode 12 */
				m13size = m13encode(frame, m13buf);

				/* create the xor buffer */
				for (int i = 0; i < _width * _workheight; i++)
					xorbuf[i] = _prevframe[i] ^ frame[i];

				/* compress the m13 buffer */
				compress2(m13buf_z, &m13size_z, m13buf, m13size, 9);

				/* compress the xor buffer */
				xorsize_z = m13size_z;
				r = compress2(xorbuf_z, &xorsize_z, xorbuf, _width * _workheight, 9);
				if (r != Z_OK) xorsize_z = 0xFFFFFFF;

				if (m13size_z < xorsize_z) {
					compType = 13;
					frameoutsize = m13size_z;
					frameoutbuf = m13buf_z;
				} else {
					compType = 3;
					frameoutsize = xorsize_z;
					frameoutbuf = xorbuf_z;
				}

				/* compress the raw frame */
				rawsize_z = frameoutsize;
				r = compress2(rawbuf_z, &rawsize_z, frame, _width * _workheight, 9);
				if (r != Z_OK) rawsize_z = 0xFFFFFFF;

				if (rawsize_z < frameoutsize) {
					compType = 2;
					frameoutsize = rawsize_z;
					frameoutbuf = rawbuf_z;
				}

				writeByte(_dxa, compType);
				writeUint32BE(_dxa, frameoutsize);
				fwrite(frameoutbuf, frameoutsize, 1, _dxa);

				delete[] xorbuf_z;
				delete[] rawbuf_z;
				delete[] m13buf_z;
				delete[] m13buf;
				delete[] xorbuf;

				break;
			}
		}

		memcpy(_prevframe, frame, _width * _workheight);

	} else {
		writeNULL();
	}

	_framecount++;
}

bool DxaEncoder::m13blocksAreEqual(byte *frame, int x, int y, int x2, int y2, int w, int h) {
	byte *b1 = _prevframe + x + y * _width;
	byte *b2 = frame + x2 + y2 * _width;
	for (int yc = 0; yc < h; yc++) {
		if (memcmp(b1, b2, w))
			return false;
		b1 += _width;
		b2 += _width;
	}
	return true;
}

bool DxaEncoder::m13blockIsSolidColor(byte *frame, int x, int y, int w, int h, byte &color) {
	byte *b2 = frame + x + y * _width;
	color = *b2;
	for (int yc = 0; yc < h; yc++) {
		for (int xc = 0; xc < w; xc++) {
			if (b2[xc] != color)
				return false;
		}
		b2 += _width;
	}
	return true;
}

void DxaEncoder::m13blockDelta(byte *frame, int x, int y, int x2, int y2, DiffStruct &diff) {
	byte *b1 = _prevframe + x + y * _width;
	byte *b2 = frame + x2 + y2 * _width;
	diff.count = 0;
	diff.map = 0;
	for (int yc = 0; yc < BLOCKH; yc++) {
		for (int xc = 0; xc < BLOCKW; xc++) {
			if (b1[xc] != b2[xc]) {
				diff.map = (diff.map << 1) | 1;
				diff.pixels[diff.count++] = b2[xc];
			} else {
				diff.map = (diff.map << 1) | 0;
			}
		}
		b1 += _width;
		b2 += _width;
	}
}

bool DxaEncoder::m13motionVector(byte *frame, int x, int y, int w, int h, int &mx, int &my) {
	int xmin = (0 > x-7) ? 0 : x-7;
	int ymin = (0 > y-7) ? 0 : y-7;
	int xmax = (_width < x+8) ? _width : x+8;
	int ymax = (_workheight < y+8) ? _height : y+8;
	for (int yc = ymin; yc < ymax; yc++) {
		for (int xc = xmin; xc < xmax; xc++) {
			if (m13blocksAreEqual(frame, xc, yc, x, y, w, h)) {
				mx = xc - x;
				my = yc - y;
				return true;
			}
		}
	}
	return false;
}

int DxaEncoder::m13countColors(byte *block, byte *pixels, unsigned long &code, int &codeSize) {

	code = 0;
	codeSize = 0;

	/* count the number of colors used in this block */
	int count = 0;
	int colTab[256];
	for (int i = 0; i < 256; i++)
		colTab[i] = -1;

	for (int i = 0; i < BLOCKW * BLOCKH; i++) {
		if (colTab[block[i]] == -1) {
			colTab[block[i]] = count;
			pixels[count] = block[i];
			count++;
		}
	}

	if (count <= 4) {
		/* set the bitmask */
		if (count == 2) {
			for (int i = 15; i >= 0; i--) {
				code = (code << 1) | colTab[block[i]];
			}
			codeSize = 2;
		} else if (count == 4 || count == 3) {
			for (int i = 15; i >= 0; i--) {
				code = (code << 2) | colTab[block[i]];
			}
			codeSize = 4;
		}
	}

	return count;
}

/* grab the block */
void DxaEncoder::grabBlock(byte *frame, int x, int y, int blockw, int blockh, byte *block) {
	byte *b2 = (byte*)frame + x + y * _width;
	for (int yc = 0; yc < blockh; yc++) {
		memcpy(&block[yc*blockw], b2, blockw);
		b2 += _width;
	}
}

uLong DxaEncoder::m13encode(byte *frame, byte *outbuf) {

	byte *codeB = _codeBuf;
	byte *dataB = _dataBuf;
	byte *motB = _motBuf;
	byte *maskB = _maskBuf;

	byte *outb = outbuf;
	byte color;
	int mx, my;
	DiffStruct diff;

	memset(_codeBuf, 0, _width * _height / 16);
	memset(_dataBuf, 0, _width * _height);
	memset(_motBuf, 0, _width * _height);
	memset(_maskBuf, 0, _width * _height);

	for (int by = 0; by < _workheight; by += BLOCKH) {
		for (int bx = 0; bx < _width; bx += BLOCKW) {
			if (m13blocksAreEqual(frame, bx, by, bx, by, BLOCKW, BLOCKH)) {
				*codeB++ = 0;
				continue;
			}

			if (m13blockIsSolidColor(frame, bx, by, BLOCKW, BLOCKH, color)) {
				*codeB++ = 2;
				*dataB++ = color;
				continue;
			}

			if (m13motionVector(frame, bx, by, BLOCKW, BLOCKH, mx, my)) {
				/* motion vector */
				byte motionByte = 0;
				if (mx < 0) motionByte |= 0x80;
				motionByte |= (abs(mx) & 7) << 4;
				if (my < 0) motionByte |= 0x08;
				motionByte |= abs(my) & 7;
				*codeB++ = 4;
				*motB++ = motionByte;
				continue;
			}

			byte subMask = 0;
			byte subMot[4], subData[16];
			int subMotSize = 0, subDataSize = 0;

 			static const int subX[4] = {0, 2, 0, 2};
			static const int subY[4] = {0, 0, 2, 2};

			/* 0: skip
			   1: solid color (+ data byte)
			   2: motion vector (+ mot byte)
			   3: raw block (+ 4 data bytes)
			*/

			for (int subBlock = 0; subBlock < 4; subBlock++) {

				int sx = bx + subX[subBlock], sy = by + subY[subBlock];
				byte scolor;
				int smx, smy;

				if (m13blocksAreEqual(frame, sx, sy, sx, sy, BLOCKW/2, BLOCKH/2)) {
					subMask = (subMask << 2) | 0;
					continue;
				}

				if (m13blockIsSolidColor(frame, sx, sy, BLOCKW/2, BLOCKH/2, scolor)) {
					subData[subDataSize++] = scolor;
					subMask = (subMask << 2) | 1;
					continue;
				}

				if (m13motionVector(frame, sx, sy, BLOCKW/2, BLOCKH/2, smx, smy)) {
					byte motionByte = 0;
					if (smx < 0) motionByte |= 0x80;
					motionByte |= (abs(smx) & 7) << 4;
					if (smy < 0) motionByte |= 0x08;
					motionByte |= abs(smy) & 7;
					subMot[subMotSize++] = motionByte;
					subMask = (subMask << 2) | 2;
					continue;
				}

				byte *b2 = (byte*)frame + sx + sy * _width;
				for (int yc = 0; yc < BLOCKH/2; yc++) {
					memcpy(&subData[subDataSize], b2, BLOCKW/2);
					subDataSize += BLOCKW/2;
					b2 += _width;
				}

				subMask = (subMask << 2) | 3;
			}

			int blockSize = 0;

			m13blockDelta(frame, bx, by, bx, by, diff);

			byte block[16];
			grabBlock(frame, bx, by, BLOCKW, BLOCKW, block);

			unsigned long code;
			int codeSize;
			byte pixels[16];
			int count = m13countColors(block, pixels, code, codeSize);

			int countColorsSize = 1000;
			if (count == 2)
				countColorsSize = 4; // 2 bytes mask, 2 pixels
			else if (count <= 4)
				countColorsSize = 4 + count; // 4 bytes mask, count pixels

			if (countColorsSize < diff.count + 2) {
				blockSize = countColorsSize;
			} else {
				if (diff.count <= 12) {
					blockSize = 2 + diff.count;
				} else {
					blockSize = 16;
				}
			}

			if (1 + subMotSize + subDataSize < blockSize) {
				/* store subblocks */
				*codeB++ = 8;
				*maskB++ = subMask;
				memcpy(dataB, subData, subDataSize);
				dataB += subDataSize;
				memcpy(motB, subMot, subMotSize);
				motB += subMotSize;
			} else {
				/* store full block */
				if (countColorsSize < diff.count + 2) {
					/* write the pixel values */
					*codeB++ = 30 + count;
					memcpy(dataB, pixels, count);
					dataB += count;
					if (codeSize == 2) {
						WRITE_BE_UINT16(maskB, code);
					} else {
						WRITE_BE_UINT32(maskB, code);
					}
					maskB += codeSize;
				} else {
					if (diff.count <= 12) {
						/* difference map */
						*codeB++ = 1;
						WRITE_BE_UINT16(maskB, diff.map);
						maskB += 2;
						memcpy(dataB, diff.pixels, diff.count);
						dataB += diff.count;
					} else {
						/* write the whole block */
						*codeB++ = 3;
						memcpy(dataB, block, 16);
						dataB += 16;
					}
				}
			}
		}
	}

	outb = outbuf;

	int size;

	size = dataB - _dataBuf;
	WRITE_BE_UINT32(outb, size);
	outb += 4;
	size = motB - _motBuf;
	WRITE_BE_UINT32(outb, size);
	outb += 4;
	size = maskB - _maskBuf;
	WRITE_BE_UINT32(outb, size);
	outb += 4;

	/* this size is always constant throughout a DXA */
	memcpy(outb, _codeBuf, codeB - _codeBuf);
	outb += codeB - _codeBuf;

	memcpy(outb, _dataBuf, dataB - _dataBuf);
	outb += dataB - _dataBuf;

	memcpy(outb, _motBuf, motB - _motBuf);
	outb += motB - _motBuf;

	memcpy(outb, _maskBuf, maskB - _maskBuf);
	outb += maskB - _maskBuf;

	return outb - outbuf;
}

int read_png_file(char* filename, unsigned char *&image, unsigned char *&palette, int &width, int &height) {
	png_byte header[8];

	png_byte color_type;
	png_byte bit_depth;

	png_structp png_ptr;
	png_infop info_ptr;
	int number_of_passes;
	png_bytep *row_pointers;

	FILE *fp = fopen(filename, "rb");
	if (!fp) {
		printf("read_png_file: Can't open file: %s\n", filename);
		exit(-1);
	}
	fread(header, 1, 8, fp);
	if (png_sig_cmp(header, 0, 8))
		return 1;

	png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);

	if (!png_ptr)
		return 1;

	info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr)
		return 1;

	if (setjmp(png_jmpbuf(png_ptr)))
		return 1;

	png_init_io(png_ptr, fp);
	png_set_sig_bytes(png_ptr, 8);

	png_read_info(png_ptr, info_ptr);

	width = info_ptr->width;
	height = info_ptr->height;
	color_type = info_ptr->color_type;
	bit_depth = info_ptr->bit_depth;

	if (color_type != PNG_COLOR_TYPE_PALETTE) {
		palette = NULL;
		return 2;
	}

	number_of_passes = png_set_interlace_handling(png_ptr);
	png_read_update_info(png_ptr, info_ptr);

	// read file
	if (setjmp(png_jmpbuf(png_ptr)))
		return 1;

	row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
	for (int y=0; y<height; y++)
		row_pointers[y] = (png_byte*) malloc(info_ptr->rowbytes);

	png_read_image(png_ptr, row_pointers);

	image = new unsigned char[width * height];
	for (int y=0; y<height; y++)
		memcpy(&image[y*width], row_pointers[y], info_ptr->rowbytes);

	for (int y=0; y<height; y++)
		free(row_pointers[y]);
	free(row_pointers);

	png_colorp pngpalette;
	int num_palette;

	png_get_PLTE(png_ptr, info_ptr, &pngpalette, &num_palette);

	palette = new unsigned char[768];
	memcpy(palette, pngpalette, 768);
	free(pngpalette);

	fclose(fp);

	return 0;
}

void readVideoInfo(char *filename, int &width, int &height, int &framerate, int &frames,
	ScaleMode &scaleMode) {

	FILE *smk = fopen(filename, "rb");
	if (!smk) {
		printf("readVideoInfo: Can't open file: %s\n", filename);
		exit(-1);
	}

	scaleMode = S_NONE;

	char buf[4];
	fread(buf, 1, 4, smk);
	if (!memcmp(buf, "BIK", 3)) {
		// Skip file size
		readUint32LE(smk);

		frames = readUint32LE(smk);

		// Skip unknown
		readUint32LE(smk);
		readUint32LE(smk);

		width = readUint32LE(smk);
		height = readUint32LE(smk);
		framerate = readUint32LE(smk);
	} else if (!memcmp(buf, "SMK2", 4) || !memcmp(buf, "SMK4", 4)) {
		uint32 flags;

		width = readUint32LE(smk);
		height = readUint32LE(smk);
		frames = readUint32LE(smk);
		framerate = readUint32LE(smk);
		flags = readUint32LE(smk);

		// If the Y-interlaced or Y-doubled flag is set, the RAD Video Tools
		// will have scaled the frames to twice their original height.

		if (flags & 0x02)
			scaleMode = S_INTERLACED;
		else if (flags & 0x04)
			scaleMode = S_DOUBLE;

		if (scaleMode != S_NONE)
			height *= 2;
	} else {
		error("readVideoInfo: Unknown type");
	}

	fclose(smk);
}

void convertWAV(char *wavName, char *prefix) {
	const char *ext;
	char outName[256];

	switch (gCompMode) {
	case kMP3Mode:
		ext = "mp3"; break;
	case kVorbisMode:
		ext = "ogg"; break;
	case kFlacMode:
		ext = "fla"; break;
	default:
		error("Unknown compression mode");
	}

	printf("Encoding audio...");
	fflush(stdout);

	sprintf(outName, "%s.%s", prefix, ext);
	encodeAudio(wavName, false, -1, outName, gCompMode);
}

void showhelp(char *exename) {
	printf("\nUsage: %s [params] <file>\n", exename);

	printf("\nParams:\n");
	printf(" --mp3        encode to MP3 format (default)\n");
	printf(" --vorbis     encode to Vorbis format\n");
	printf(" --flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

	printf("\nMP3 mode params:\n");
	printf(" -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%d)\n", minBitrDef);
	printf(" -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf(" --vbr        LAME uses the VBR mode (default)\n");
	printf(" --abr        LAME uses the ABR mode\n");
	printf(" -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf(" -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf(" --silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf(" -b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf(" -m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf(" -M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf(" -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%d)\n", oggqualDef);
	printf(" --silent     the output of oggenc is hidden (default:disabled)\n");

	printf("\nFlac mode params:\n");
	printf(" --fast       FLAC uses compresion level 0\n");
	printf(" --best       FLAC uses compresion level 8\n");
	printf(" -<value>     specifies the value (0 - 8) of compresion (8=best)(default:%d)\n", flacCompressDef);
	printf(" -b <value>   specifies a blocksize of <value> samples (default:%d)\n", flacBlocksizeDef);
	printf(" --verify     files are encoded and then decoded to check accuracy\n");
	printf(" --silent     the output of FLAC is hidden (default:disabled)\n");

	printf("\n --help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

int main(int argc, char *argv[]) {
	if (argc < 2)
		showhelp(argv[0]);

	char strbuf[512];
	int width, height, framerate, frames;
	ScaleMode scaleMode;

	/* compression mode */
	gCompMode = kMP3Mode;
	int i = 1;
	if (!strcmp(argv[1], "--mp3")) {
		gCompMode = kMP3Mode;
		i++;
	} else if (!strcmp(argv[1], "--vorbis")) {
		gCompMode = kVorbisMode;
		i++;
	} else if (!strcmp(argv[1], "--flac")) {
		gCompMode = kFlacMode;
		i++;
	}

	switch (gCompMode) {
	case kMP3Mode:
		if (!process_mp3_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		if (!process_ogg_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		if (!process_flac_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	}

	i = argc - 1;

	// get filename prefix
	char *filename = argv[i++];
	char prefix[256];
	char *p;

	getFilename(filename, prefix);

	p = strrchr(prefix, '.');
	if (p) {
		*p = '\0';
	}

	// check if the wav file exists.
	sprintf(strbuf, "%s.wav", prefix);
	struct stat statinfo;
	if (!stat(strbuf, &statinfo)) {
		convertWAV(strbuf, prefix);
	}

	// read some data from the Bink or Smacker file.
	readVideoInfo(filename, width, height, framerate, frames, scaleMode);

	printf("Width = %d, Height = %d, Framerate = %d, Frames = %d\n",
		   width, height, framerate, frames);

	// create the encoder object
	sprintf(strbuf, "%s.dxa", prefix);
	DxaEncoder dxe(strbuf, width, height, framerate, scaleMode);

	// No sound block
	dxe.writeNULL();

	uint8 *image = NULL;
	uint8 *palette = NULL;
	int framenum = 0;

	printf("Encoding video...");
	fflush(stdout);

	for (int f = 0; f < frames; f++) {
		if (frames > 999)
			sprintf(strbuf, "%s%04d.png", prefix, framenum);
		else if (frames > 99)
			sprintf(strbuf, "%s%03d.png", prefix, framenum);
		else if (frames > 9)
			sprintf(strbuf, "%s%02d.png", prefix, framenum);
		else
			sprintf(strbuf, "%s%d.png", prefix, framenum);

		int r = read_png_file(strbuf, image, palette, width, height);

		if (!palette) {
			printf("Error: 8-bit 256-color image expected!\n");
			exit(0);
		}

		if (!r) {
			if (scaleMode != S_NONE) {
				byte *unscaledImage = new byte[width * height / 2];

				for (int y = 0; y < height; y += 2)
					memcpy(&unscaledImage[(width*y)/2], &image[width*y], width);

				dxe.writeFrame(unscaledImage, palette);
				delete[] unscaledImage;
			} else {
				dxe.writeFrame(image, palette);
			}
		}

		if (image) delete[] image;
		if (palette) delete[] palette;

		if (r)
			break;

		framenum++;

		if (framenum % 20 == 0) {
			printf("\rEncoding video...%d%% (%d of %d)", 100 * framenum / frames, framenum, frames);
			fflush(stdout);
		}
	}

	printf("\rEncoding video...100%% (%d of %d)\n", frames, frames);

	return 0;
}
