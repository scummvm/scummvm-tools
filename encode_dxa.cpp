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
#include <sys/stat.h>
#include <png.h>
#include <zlib.h>

#include "encode_dxa.h"
#include "common/endian.h"

const uint32 typeDEXA = 0x41584544;
const uint32 typeFRAM = 0x4d415246;
//const uint32 typeWAVE = 0x45564157;
const uint32 typeCMAP = 0x50414D43;
const uint32 typeNULL = 0x4C4C554E;

#define	 BUFFER_LEN	1024

// other block dimensions than 4x4 are not really supported yet
#define  BLOCKW	4
#define  BLOCKH	4

struct DiffStruct {
	uint16 map;
	int count;
	byte pixels[BLOCKW*BLOCKH];
};

class DxaEncoder {
private:
	Common::File _dxa;
	int _width, _height, _framerate, _framecount, _workheight;
	uint8 *_prevframe, *_prevpalette;
	ScaleMode _scaleMode;
	byte _compType;

	byte *_codeBuf, *_dataBuf, *_motBuf, *_maskBuf;
	void grabBlock(byte *frame, int x, int y, int blockw, int blockh, byte *block);
	bool m13blocksAreEqual(byte *frame, int x, int y, int x2, int y2, int w, int h);
	bool m13blockIsSolidColor(byte *frame, int x, int y, int w, int h, byte &color);
	void m13blockDelta(byte *frame, int x, int y, int x2, int y2, DiffStruct &diff);
	bool m13motionVector(byte *frame, int x, int y, int w, int h, int &mx, int &my);
	int m13countColors(byte *block, byte *pixels, unsigned long &code, int &codeSize);
	uLong m12encode(byte *frame, byte *outbuf);
	uLong m13encode(byte *frame, byte *outbuf);

public:
	DxaEncoder(Tool &tool, Common::Filename filename, int width, int height, int framerate, ScaleMode scaleMode, byte compType);
	~DxaEncoder();
	void writeHeader();
	void writeNULL();
	void writeFrame(uint8 *frame, uint8 *palette);
};

DxaEncoder::DxaEncoder(Tool &tool, Common::Filename filename, int width, int height, int framerate, ScaleMode scaleMode, byte compType) {
	_dxa.open(filename, "wb");
	_width = width;
	_height = height;
	_framerate = framerate;
	_framecount = 0;
	_prevframe = new uint8[_width * _height];
	_prevpalette = new uint8[768];
	_scaleMode = scaleMode;
	_workheight = _scaleMode == S_NONE ? _height : _height / 2;
	_compType = compType;

	_codeBuf = new byte[_width * _height / 16];
	_dataBuf = new byte[_width * _height];
	_motBuf = new byte[_width * _height];
	_maskBuf = new byte[_width * _height];

	writeHeader();
}

DxaEncoder::~DxaEncoder() {
	_dxa.seek(0, SEEK_SET);

	writeHeader();

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

	_dxa.writeUint32LE(typeDEXA);
	_dxa.writeByte(version);

	_dxa.writeUint16BE(_framecount);
	_dxa.writeUint32BE(_framerate);
	_dxa.writeUint16BE(_width);
	_dxa.writeUint16BE(_height);
}

void DxaEncoder::writeNULL() {
	//NULL
	_dxa.writeUint32LE(typeNULL);
}

void DxaEncoder::writeFrame(byte *frame, byte *palette) {

	if (_framecount == 0 || memcmp(_prevpalette, palette, 768)) {
		_dxa.writeUint32LE(typeCMAP);
		_dxa.write(palette, 768);
		memcpy(_prevpalette, palette, 768);
	} else {
		writeNULL();
	}

	if (_framecount == 0 || memcmp(_prevframe, frame, _width * _workheight)) {
		//FRAM
		byte compType;

		_dxa.writeUint32LE(typeFRAM);

		if (_framecount == 0)
			compType = 2;
		else
			compType = _compType;

		switch (compType) {

		case 2:
			{
				uLong outsize = _width * _workheight;
				byte *outbuf = new byte[outsize];
				compress2(outbuf, &outsize, frame, _width * _workheight, 9);
				_dxa.writeByte(compType);
				_dxa.writeUint32BE(outsize);
				_dxa.write(outbuf, outsize);
				delete[] outbuf;
				break;
			}
		case 3:
			{
				uLong outsize;
				uLong outsize1 = _width * _workheight;
				uLong outsize2 = outsize1;
				byte *outbuf;
				byte *outbuf1 = new byte[outsize1];
				byte *outbuf2 = new byte[outsize2];
				byte *xorbuf = new byte[_width * _workheight];

				for (int i = 0; i < _width * _workheight; i++)
					xorbuf[i] = _prevframe[i] ^ frame[i];

				compress2(outbuf1, &outsize1, xorbuf, _width * _workheight, 9);
				compress2(outbuf2, &outsize2, frame, _width * _workheight, 9);

				if (outsize1 < outsize2) {
					compType = 3;
					outsize = outsize1;
					outbuf = outbuf1;
				} else {
					compType = 2;
					outsize = outsize2;
					outbuf = outbuf2;
				}

				_dxa.writeByte(compType);
				_dxa.writeUint32BE(outsize);
				_dxa.write(outbuf, outsize);

				delete[] outbuf1;
				delete[] outbuf2;
				delete[] xorbuf;

				break;
			}
                case 12:
                        {
				uLong outsize1 = _width * _workheight;
				uLong outsize2 = outsize1;
				uLong outsize3 = outsize1*2;
				uLong outsize4 = outsize1;
				uLong outsize;
				uint8 *outbuf;
				uint8 *outbuf1 = new uint8[outsize1];
				uint8 *outbuf2 = new uint8[outsize2];
				uint8 *outbuf3 = new uint8[outsize3];
				uint8 *outbuf4 = new uint8[outsize4];
				uint8 *xorbuf = new uint8[_width * _workheight];

				for (int i = 0; i < _width * _workheight; i++)
					xorbuf[i] = _prevframe[i] ^ frame[i];

				compress2(outbuf1, &outsize1, xorbuf, _width * _workheight, 9);
				compress2(outbuf2, &outsize2, frame, _width * _workheight, 9);
				if (outsize1 < outsize2) {
					compType = 3;
					outsize = outsize1;
					outbuf = outbuf1;
				} else {
					compType = 2;
					outsize = outsize2;
					outbuf = outbuf2;
				}

				outsize3 = m12encode(frame, outbuf3);

				compress2(outbuf4, &outsize4, outbuf3, outsize3, 9);

				if (outsize4 < outsize) {
					compType = 12;
					outsize = outsize4;
					outbuf = outbuf4;
				}

				_dxa.writeByte(compType);
				_dxa.writeUint32BE(outsize);
				_dxa.write(outbuf, outsize);

				delete[] outbuf1;
				delete[] outbuf2;
				delete[] outbuf3;
				delete[] outbuf4;
				delete[] xorbuf;

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

				_dxa.writeByte(compType);
				_dxa.writeUint32BE(frameoutsize);
				_dxa.write(frameoutbuf, frameoutsize);

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

uLong DxaEncoder::m12encode(byte *frame, byte *outbuf) {
	byte *outb = outbuf;
	byte color;
	int mx, my;
	DiffStruct diff;

	for (int by = 0; by < _workheight; by += BLOCKH) {
		for (int bx = 0; bx < _width; bx += BLOCKW) {
			if (m13blocksAreEqual(frame, bx, by, bx, by, BLOCKW, BLOCKH)) {
				*outb++ = 0;
				continue;
			}

			if (m13blockIsSolidColor(frame, bx, by, BLOCKW, BLOCKH, color)) {
				*outb++ = 2;
				*outb++ = color;
				continue;
			}

			if (m13motionVector(frame, bx, by, BLOCKW, BLOCKH, mx, my)) {
				byte mbyte = 0;
				if (mx < 0) mbyte |= 0x80;
				mbyte |= (abs(mx) & 7) << 4;
				if (my < 0) mbyte |= 0x08;
				mbyte |= abs(my) & 7;
				*outb++ = 4;
				*outb++ = mbyte;
				continue;
			}

			m13blockDelta(frame, bx, by, bx, by, diff);

			if (diff.count >= 14) {
				// in this case we store all 16 pixels
				*outb++ = 3;
				byte *b2 = (byte*)frame + bx + by * _width;
				for (int yc = 0; yc < BLOCKH; yc++) {
					memcpy(outb, b2, BLOCKW);
					b2 += _width;
					outb += BLOCKW;
				}
				continue;
			} else {
				static const struct { uint16 mask; uint8 sh1, sh2; } maskTbl[6] = {
					{0xFF00, 0, 0},
					{0x0FF0, 8, 0},
					{0x00FF, 8, 8},
					{0x0F0F, 8, 4},
					{0xF0F0, 4, 0},
					{0xF00F, 4, 4}
				};

				bool smallMask = false;

				// here we check if the difference bitmap can be stored in only one byte
				for (int m = 0; m < 6; m++) {
					if ((diff.map & maskTbl[m].mask) == 0) {
						smallMask = true;
						*outb++ = 10 + m;
						*outb++ = ((diff.map >> maskTbl[m].sh1) & 0xF0) | ((diff.map >> maskTbl[m].sh2) & 0x0F);
						break;
					}
				}

				if (!smallMask) {
					*outb++ = 1;
					WRITE_BE_UINT16(outb, diff.map);
					outb += 2;
				}

				memcpy(outb, diff.pixels, diff.count);
				outb += diff.count;
				continue;
			}
		}
	}

	return outb - outbuf;
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
						WRITE_BE_UINT16(maskB, (uint16)code);
					} else {
						WRITE_BE_UINT32(maskB, (uint16)code);
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

EncodeDXA::EncodeDXA(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION), _compType(13) {

	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Used to create DXA files from extracted Smacker archives.";
	_helptext =
		"Usage: " + getName() + " [mode] [mode-params] [-o outpufile = inputfile.san] <inputfile>\n" +
		"Output will be two files, one with .dxa extension and the other depending on the used audio codec.";
}

void EncodeDXA::parseExtraArguments() {
	if (!_arguments.empty()) {
		if (_arguments.front() == "-c") {
			_arguments.pop_front();
			_compType = atoi(_arguments.front().c_str());
			_arguments.pop_front();
		}
	}
}

void EncodeDXA::execute() {
	int width, height, framerate, frames;
	ScaleMode scaleMode;
	Common::Filename inpath(_inputPaths[0].path);
	Common::Filename outpath(_outputPath);

	if (outpath.empty())
		// Actual change of extension is done later...
		outpath = inpath;

	// check if the wav file exists.
	Common::Filename wavpath(inpath);
	wavpath.setExtension(".wav");
	struct stat statinfo;
	if (!stat(wavpath.getFullPath().c_str(), &statinfo)) {
		outpath.setExtension(audio_extensions(_format));
		convertWAV(&wavpath, &outpath);
	}

	// read some data from the Bink or Smacker file.
	readVideoInfo(&inpath, width, height, framerate, frames, scaleMode);

	print("Width = %d, Height = %d, Framerate = %d, Frames = %d, Compression type = %d",
		   width, height, framerate, frames, _compType);

	// create the encoder object
	outpath.setExtension(".dxa");
	DxaEncoder dxe(*this, outpath, width, height, framerate, scaleMode, _compType);

	// No sound block
	dxe.writeNULL();

	uint8 *image = NULL;
	uint8 *palette = NULL;

	char fullname[1024];
	strcpy(fullname, inpath.getFullPath().c_str());

	// Check starting frame (binkconv starts at 0, ffmpeg starts at 1)
	int framenum = 0;
	char strbuf[1024];
	sprintf(strbuf, "%s%04d.png", fullname, framenum);
	if (!Common::Filename(strbuf).exists())
		framenum++;

	print("Encoding video...");
	for (int f = 0; f < frames; f++) {
		if (frames > 999)
			sprintf(strbuf, "%s%04d.png", fullname, framenum);
		else if (frames > 99)
			sprintf(strbuf, "%s%03d.png", fullname, framenum);
		else if (frames > 9)
			sprintf(strbuf, "%s%02d.png", fullname, framenum);
		else
			sprintf(strbuf, "%s%d.png", fullname, framenum);
		inpath.setFullName(strbuf);

		int r = read_png_file(inpath.getFullPath().c_str(), image, palette, width, height);

		if (!palette) {
			error("8-bit 256-color image expected");
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

		delete[] image;
		delete[] palette;

		if (r)
			break;

		framenum++;

		if (framenum % 20 == 0) {
			print("Encoding video...%d%% (%d of %d)", 100 * framenum / frames, framenum, frames);
		}
	}

	print("Encoding video...100%% (%d of %d)", frames, frames);
}

int EncodeDXA::read_png_file(const char* filename, unsigned char *&image, unsigned char *&palette, int &width, int &height) {
	png_byte header[8];

	png_byte color_type;

	png_structp png_ptr;
	png_infop info_ptr;
	png_bytep *row_pointers;
	png_size_t rowbytes;

	Common::File fp(filename, "rb");

	fp.read_throwsOnError(header, 8);
	if (png_sig_cmp(header, 0, 8))
		return 1;

	png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);

	if (!png_ptr)
		return 1;

	info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr)
		return 1;

	//if (setjmp(png_jmpbuf(png_ptr)))
	//	return 1;

	png_init_io(png_ptr, fp.getFileHandle());
	png_set_sig_bytes(png_ptr, 8);

	png_read_info(png_ptr, info_ptr);

	width = png_get_image_width(png_ptr, info_ptr);
	height = png_get_image_height(png_ptr, info_ptr);
	color_type = png_get_color_type(png_ptr, info_ptr);
	/*png_byte bit_depth =*/ png_get_bit_depth(png_ptr, info_ptr);

	if (color_type != PNG_COLOR_TYPE_PALETTE) {
		palette = NULL;
		return 2;
	}

	/*int number_of_passes =*/ png_set_interlace_handling(png_ptr);
	png_read_update_info(png_ptr, info_ptr);

	// read file
	//if (setjmp(png_jmpbuf(png_ptr)))
	//	return 1;

	rowbytes = png_get_rowbytes(png_ptr, info_ptr);
	row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
	for (int y=0; y<height; y++)
		row_pointers[y] = (png_byte*) malloc(rowbytes);

	png_read_image(png_ptr, row_pointers);

	image = new unsigned char[width * height];
	for (int y=0; y<height; y++)
		memcpy(&image[y*width], row_pointers[y], rowbytes);

	for (int y=0; y<height; y++)
		free(row_pointers[y]);
	free(row_pointers);

	png_colorp pngpalette;
	int num_palette;

	png_get_PLTE(png_ptr, info_ptr, &pngpalette, &num_palette);

	palette = new unsigned char[768];
	memcpy(palette, pngpalette, 768);
	free(pngpalette);

	return 0;
}

void EncodeDXA::readVideoInfo(Common::Filename *filename, int &width, int &height, int &framerate, int &frames, ScaleMode &scaleMode) {

	Common::File smk(*filename, "rb");

	scaleMode = S_NONE;

	char buf[4];
	smk.read_throwsOnError(buf, 4);
	if (!memcmp(buf, "BIK", 3)) {
		// Skip file size
		smk.readUint32LE();

		frames = smk.readUint32LE();

		// Skip unknown
		smk.readUint32LE();
		smk.readUint32LE();

		width = smk.readUint32LE();
		height = smk.readUint32LE();
		framerate = smk.readUint32LE();
	} else if (!memcmp(buf, "SMK2", 4) || !memcmp(buf, "SMK4", 4)) {
		uint32 flags;

		width = smk.readUint32LE();
		height = smk.readUint32LE();
		frames = smk.readUint32LE();
		framerate = smk.readUint32LE();
		flags = smk.readUint32LE();

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
}

void EncodeDXA::convertWAV(const Common::Filename *inpath, const Common::Filename* outpath) {
	print("Encoding audio...");
	fflush(stdout);

	encodeAudio(inpath->getFullPath().c_str(), false, -1, outpath->getFullPath().c_str(), _format);
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	EncodeDXA encode_dxa(argv[0]);
	return encode_dxa.run(argc, argv);
}
#endif

