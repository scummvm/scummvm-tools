/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
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
 */


#ifndef COMMON_ZLIB_H
#define COMMON_ZLIB_H

#if defined(USE_ZLIB)
  #ifdef __SYMBIAN32__
    #include <zlib\zlib.h>
  #else
    #include <zlib.h>
  #endif

  #if ZLIB_VERNUM < 0x1204
  #error Version 1.2.0.4 or newer of zlib is required for this code
  #endif

/**
 * A simple wrapper class which can be used to wrap around an arbitrary
 * other std::ifstream and will then provide on-the-fly decompression support.
 * Assumes the compressed data to be in gzip format.
 */
class GZipReadStream {
protected:
	enum {
		BUFSIZE = 16384		// 1 << MAX_WBITS
	};

	byte	_buf[BUFSIZE];

	std::ifstream *_wrapped;
	z_stream _stream;
	int _zlibErr;
	uint32 _pos;
	uint32 _origSize;
	bool _eos;
	uint32 _start, _size;

public:
	GZipReadStream(std::ifstream *w, uint32 start, uint32 size = 0);
	~GZipReadStream();
	bool err() const;
	void clearErr();

	uint32 read(void *dataPtr, uint32 dataSize);

	bool eos() const;
	int32 pos() const;
	int32 size() const;
	bool seek(int32 offset, std::ios::seekdir whence = std::ios::beg);
};

/**
 * A simple wrapper class which can be used to wrap around an arbitrary
 * other std::ofstream and will then provide on-the-fly compression support.
 * The compressed data is written in the gzip format.
 */
class GZipWriteStream {
protected:
	enum {
		BUFSIZE = 16384		// 1 << MAX_WBITS
	};

	byte	_buf[BUFSIZE];
	std::ofstream *_wrapped;
	z_stream _stream;
	int _zlibErr;

	void processData(int flushType);

public:
	GZipWriteStream(std::ofstream *w);
	~GZipWriteStream();

	bool err() const;
	void finalize();

	uint32 write(const void *dataPtr, uint32 dataSize);
};

#endif

#endif
