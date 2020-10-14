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
#include "tools/lab.h"
#include "tools/emi/filetools.h"
#include <GLFW/glfw3.h>

/*
This tool renders EMI-TILEs, optionally allowing the user to enable/disable layers in the TIL.

I'm rather sure this only works on LE-systems for now, and it REQUIRES GLFW 3 and zlib to work.

Usage:
renderTil [labName] <filename>

To enable or disable layers, simply press the matching key.

somaen.
*/

// Credits: The TILE-loading and texture-generation/drawing code was lifted directly from ScummVM
// The rendering setup was lifted directly from the GLFW-intro tutorial.

#define BITMAP_TEXTURE_SIZE 256

/**
 * The Color-formats used for bitmaps in Grim Fandango/Escape From Monkey Island
 */
enum colorFormat {
	BM_RGB565 = 1,    // Grim Fandango
	BM_RGB1555 = 2,   // EMI-PS2
	BM_RGBA = 3,      // EMI-PC (Also internal Material-format for Grim)
	BM_BGR888 = 4,    // EMI-TGA-materials (888)
	BM_BGRA = 5       // EMI-TGA-materials with alpha
};

class Tile {
	float *_texc;
	struct Vert {
		uint32 _texid;
		uint32 _pos;
		uint32 _verts;
	};
	struct Layer {
		uint32 _offset;
		uint32 _numImages;
	};
	Vert *_verts;
	Layer *_layers;
	uint32 _numCoords;
	uint32 _numVerts;
	uint32 _numLayers;
	int _numSubImages;
	char **_data;
	int _width, _height;
	int _bpp;
	int _colorFormat;
	// Rendering:
	int _numTex;
	void *_texIds;
	bool *_enabledLayers;
public:
	Tile() : _data(NULL), _layers(NULL), _verts(NULL), _numSubImages(0), _numVerts(0), _numLayers(0), _numCoords(0), _width(0), _height(0), _texc(0), _bpp(0), _colorFormat(0), _numTex(0), _texIds(NULL), _enabledLayers(NULL) {}
	~Tile() {
		for (int i = 0; i < _numSubImages; i++) {
			delete _data[i];
		}
		delete[] _enabledLayers;
		delete[] _data;
		delete[] _verts;
		delete[] _layers;
	}
	bool loadTile(SeekableReadStream *stream);
	void createBitmap();
	void drawBitmap(int dx, int dy, uint32 layer);
	void drawAllLayers() {
		for (int layer = _numLayers - 1; layer >= 0; layer--) {
			if (_enabledLayers[layer]) {
				drawBitmap(0, 0, layer);
			}
		}
	}
	void toggleLayer(int layer) {
		if (layer < _numLayers) {
			_enabledLayers[layer] = !_enabledLayers[layer];
			std::cout << "Layer " << layer << (_enabledLayers[layer] ? " enabled" : " disabled") << std::endl;
		}
	}
};

bool Tile::loadTile(SeekableReadStream *stream) {
	uint32 id = FROM_BE_32(stream->readUint32());
	char *test = (char *)&id;
	// Should check that we actually HAVE a TIL
	uint32 bmoffset = stream->readUint32();
	_numCoords = stream->readUint32();
	_numLayers = stream->readUint32();
	_numVerts = stream->readUint32();

	// skip some 0
	stream->seek(16, SEEK_CUR);
	_texc = new float[_numCoords * 4];
	std::cout << "Num Coords: " << _numCoords << std::endl;
	for (uint32 i = 0; i < _numCoords * 4; ++i) {
		char f[4];
		stream->read(f, 4);
		_texc[i] = get_float(f);
	}

	_layers = new Layer[_numLayers];
	std::cout << "Num Layers: " << _numLayers << std::endl;
	for (uint32 i = 0; i < _numLayers; ++i) {
		_layers[i]._offset = stream->readUint32();
		_layers[i]._numImages = stream->readUint32();
	}

	_verts = new Vert[_numVerts];
	std::cout << "Num Vertices:" << _numVerts << std::endl;
	for (uint32 i = 0; i < _numVerts; ++i) {
		_verts[i]._texid = stream->readUint32();
		_verts[i]._pos = stream->readUint32();
		_verts[i]._verts = stream->readUint32();
	}

	stream->seek(16, SEEK_CUR);
	_numSubImages = stream->readUint32();
	std::cout << "Num Sub-images: " << _numSubImages << std::endl;
	if (_numSubImages < 5) {
		std::cout << "Can not handle a tile with less than 5 sub images, this has " << _numSubImages << std::endl;
		exit(0);
	}

	_data = new char *[_numSubImages];

	stream->seek(16, SEEK_CUR);
	_bpp = stream->readUint32();

	stream->seek(bmoffset + 128);

	_width = stream->readUint32();
	_height = stream->readUint32();
	std::cout << "Width: " << _width << std::endl << "Height: " << _height << std::endl;
	stream->seek(-8, SEEK_CUR);

	int size = _bpp / 8 * _width * _height;
	for (int i = 0; i < _numSubImages; ++i) {
		_data[i] = new char[size];
		stream->seek(8, SEEK_CUR);
		stream->read(_data[i], size);
	}
	//Graphics::PixelFormat pixelFormat;
	if (_bpp == 16) {
		//  _colorFormat = BM_RGB1555;
		//  pixelFormat = Graphics::createPixelFormat<1555>();
		//convertToColorFormat(0, BM_RGBA);
	} else {
		//  pixelFormat = Graphics::PixelFormat(4, 8, 8, 8, 8, 0, 8, 16, 24);
		//  _colorFormat = BM_RGBA;
	}

	_width = 256;
	_height = 256;

	_enabledLayers = new bool[_numLayers];
	for (int i = 0; i < _numLayers; i++) {
		_enabledLayers[i] = true;
	}

	return true;
}

void Tile::createBitmap() {
//	bitmap->_hasTransparency = false;
	_numTex = ((_width + (BITMAP_TEXTURE_SIZE - 1)) / BITMAP_TEXTURE_SIZE) *
			  ((_height + (BITMAP_TEXTURE_SIZE - 1)) / BITMAP_TEXTURE_SIZE);
	_texIds = new GLuint[_numTex * _numSubImages];
	GLuint *textures = (GLuint *)_texIds;
	glGenTextures(_numTex * _numSubImages, textures);

	byte *texData = 0;
	byte *texOut = 0;

	GLint format = GL_RGBA;
	GLint type = GL_UNSIGNED_BYTE;
	int bytes = 4;

	glPixelStorei(GL_UNPACK_ALIGNMENT, bytes);
	glPixelStorei(GL_UNPACK_ROW_LENGTH, _width);

	for (int pic = 0; pic < _numSubImages; pic++) {
		if (_bpp == 16 && _colorFormat != BM_RGB1555) {
			if (texData == 0) {
				texData = new byte[4 * _width * _height];
			}
			// Convert data to 32-bit RGBA format
			byte *texDataPtr = texData;
			uint16 *bitmapData = reinterpret_cast<uint16 *>(_data[pic]);
			for (int i = 0; i < _width * _height; i++, texDataPtr += 4, bitmapData++) {
				uint16 pixel = *bitmapData;
				int r = pixel >> 11;
				texDataPtr[0] = (r << 3) | (r >> 2);
				int g = (pixel >> 5) & 0x3f;
				texDataPtr[1] = (g << 2) | (g >> 4);
				int b = pixel & 0x1f;
				texDataPtr[2] = (b << 3) | (b >> 2);
				if (pixel == 0xf81f) { // transparent
					texDataPtr[3] = 0;
					//bitmap->_hasTransparency = true;
				} else {
					texDataPtr[3] = 255;
				}
			}
			texOut = texData;
		} else if (_colorFormat == BM_RGB1555) {
			std::cout << "TODO: BM_RGB1555 Support" << std::endl;
//			bitmap->convertToColorFormat(pic, Graphics::PixelFormat(4, 8, 8, 8, 8, 0, 8, 16, 24));
//			texOut = (byte *)bitmap->getImageData(pic).getRawBuffer();
		} else {
			texOut = (byte *)_data[pic];
		}

		for (int i = 0; i < _numTex; i++) {
			glBindTexture(GL_TEXTURE_2D, textures[_numTex * pic + i]);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
			glTexImage2D(GL_TEXTURE_2D, 0, format, BITMAP_TEXTURE_SIZE, BITMAP_TEXTURE_SIZE, 0, format, type, NULL);
		}

		int cur_tex_idx = _numTex * pic;

		for (int y = 0; y < _height; y += BITMAP_TEXTURE_SIZE) {
			for (int x = 0; x < _width; x += BITMAP_TEXTURE_SIZE) {
				int width  = (x + BITMAP_TEXTURE_SIZE >= _width) ? (_width - x) : BITMAP_TEXTURE_SIZE;
				int height = (y + BITMAP_TEXTURE_SIZE >= _height) ? (_height - y) : BITMAP_TEXTURE_SIZE;
				glBindTexture(GL_TEXTURE_2D, textures[cur_tex_idx]);
				glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, format, type,
								texOut + (y * bytes * _width) + (bytes * x));
				cur_tex_idx++;
			}
		}
	}

	glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
}

void Tile::drawBitmap(int dx, int dy, uint32 layer) {
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity();
	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity();
	glOrtho(-1, 1, -1, 1, 0, 1);

	glEnable(GL_ALPHA_TEST);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glDisable(GL_LIGHTING);
	glEnable(GL_TEXTURE_2D);
	glColor3f(1, 1, 1);
	glDisable(GL_DEPTH_TEST);
	glDepthMask(GL_FALSE);

	GLuint *textures = (GLuint *)_texIds;
	float *texc = _texc;

	uint32 offset = _layers[layer]._offset;
	for (uint32 i = offset; i < offset + _layers[layer]._numImages; ++i) {
		glBindTexture(GL_TEXTURE_2D, textures[_verts[i]._texid]);
		glBegin(GL_QUADS);
		uint32 ntex = _verts[i]._pos * 4;
		for (uint32 x = 0; x < _verts[i]._verts; ++x) {
			glTexCoord2f(texc[ntex + 2], texc[ntex + 3]);
			glVertex2f(texc[ntex + 0], texc[ntex + 1]);
			ntex += 4;
		}
		glEnd();
	}

	glDisable(GL_TEXTURE_2D);
	glDepthMask(GL_TRUE);
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_LIGHTING);

	glPopMatrix();
	glMatrixMode(GL_MODELVIEW);
	glPopMatrix();

	return;
}

void renderInit() {
	glClearColor(0.2f, 0.2f, 0.2f, 0.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glLoadIdentity();
}

// HACK, to enable and disable layers
static int keyForLayer = -1;
void renderLoop(GLFWwindow *window, Tile &t) {
	float rot = 0.5f;
	while (!glfwWindowShouldClose(window) && keyForLayer != -2) {
		renderInit();

		t.drawAllLayers();
		if (keyForLayer != -1) {
			t.toggleLayer(keyForLayer);
			keyForLayer = -1;
		}
		glFlush();
		glfwSwapBuffers(window);
		glfwPollEvents();
		usleep(5000);
	}
	glfwTerminate();
}

void keycallback(GLFWwindow *window, int key, int scancode, int action, int mods) {
	if (action == GLFW_PRESS) {
		if (key >= GLFW_KEY_0 && key <= GLFW_KEY_9) {
			keyForLayer = key - GLFW_KEY_0;
		} else if (key == GLFW_KEY_ESCAPE) {
			keyForLayer = -2;
		}
	}
}

Bytef *decompress(Bytef *in, int size, uint32_t &outsize) {
	const uint32_t block = 8 * 1024 * 1024;
	Bytef *dest = new Bytef[block]; // 8 MiB ought to be enough.

	int success = 0;
	z_stream_s zStream;

	zStream.next_in = Z_NULL;
	zStream.avail_in = 0;
	zStream.zalloc = Z_NULL;
	zStream.zfree = Z_NULL;
	zStream.opaque = Z_NULL;

	success = inflateInit2(&zStream, 16 + MAX_WBITS);
	if (success != Z_OK) {
		std::cout << "ZLIB failed to initialize\n";
		return 0;
	}
	zStream.avail_in = size;
	zStream.next_in = in;
	zStream.avail_out = block;
	zStream.next_out = dest;

	success = inflate(&zStream, Z_NO_FLUSH);

	outsize = zStream.total_out;

	if (success != Z_STREAM_END) {
		std::cout << "ERROR: decompressed size bigger than 8 MiB\n";
		return 0;
	}
	return dest;
}


int main(int argc, char **argv) {
	if (argc < 2) {
		std::cout << "No Argument" << std::endl;
		std::cout << "Usage: renderTil [labfilename] <filename>" << std::endl;
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
		std::cout << "ERROR: Could not open file" << std::endl;
		return 0;
	}

	Bytef *compressedData = new Bytef[length];
	file->read((char *)compressedData, length);
	uint32_t outsize;
	Bytef *data = decompress((Bytef *)compressedData, length, outsize);
	delete[] compressedData;
	if (!data) {
		return -1;
	}

	MemoryReadStream *stream = new MemoryReadStream(data, outsize);

	// Initialization and render-loop more or less copied directly from
	// http://www.glfw.org/documentation.html
	glfwInit();
	GLFWwindow *window = glfwCreateWindow(640, 480, filename.c_str(), NULL, NULL);
	glfwMakeContextCurrent(window);
	glfwSetKeyCallback(window, keycallback);
	Tile t;
	if (t.loadTile(stream)) {
		t.createBitmap();
		std::cout << "Loading of " << filename << " succeeded" << std::endl;
		std::cout << "Use keys 0 - 9 to enable or disable layers, close window to exit, or press ESC" << std::endl;
		renderLoop(window, t);
	} else {
		std::cout << "Load error for " << filename << std::endl;
	}
	delete stream;
	return 0;
}
