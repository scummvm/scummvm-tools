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


#include <iostream>
#include <fstream>

#include "common/scummsys.h"
#include "common/endian.h"
#include "common/zlib.h"

#define MIN(x,y) (((x)<(y)) ? (x) : (y))

#if defined(USE_ZLIB)

GZipReadStream::GZipReadStream(std::ifstream *w, uint32 start, uint32 size_p) : _wrapped(w), _stream(), _start(start), _size(size_p) {
	char buf[4];
	assert(w != 0);

	// Verify file header is correct
	w->seekg(_start, std::ios::beg);
	w->read(buf, 2);
	uint16 header = READ_BE_UINT16(buf);
	assert(header == 0x1F8B ||
			((header & 0x0F00) == 0x0800 && header % 31 == 0));

	if (header == 0x1F8B && _size > 0) {
		// Retrieve the original file size
		w->seekg(_start + _size - 4, std::ios::beg);
		w->read(buf, 4);
		_origSize = READ_LE_UINT32(buf);
	} else {
		// Original size not available in zlib format
		_origSize = 0;
	}
	_pos = 0;
	w->seekg(_start, std::ios::beg);
	_eos = false;

	// Adding 32 to windowBits indicates to zlib that it is supposed to
	// automatically detect whether gzip or zlib headers are used for
	// the compressed file. This feature was added in zlib 1.2.0.4,
	// released 10 August 2003.
	// Note: This is *crucial* for savegame compatibility, do *not* remove!
	_zlibErr = inflateInit2(&_stream, MAX_WBITS + 32);
	if (_zlibErr != Z_OK)
		return;

	// Setup input buffer
	_stream.next_in = _buf;
	_stream.avail_in = 0;
}

GZipReadStream::~GZipReadStream() {
	inflateEnd(&_stream);
}

bool GZipReadStream::err() const { return (_zlibErr != Z_OK) && (_zlibErr != Z_STREAM_END); }
void GZipReadStream::clearErr() {
	// only reset _eos; I/O errors are not recoverable
	_eos = false;
}

uint32 GZipReadStream::read(void *dataPtr, uint32 dataSize) {
	_stream.next_out = (byte *)dataPtr;
	_stream.avail_out = dataSize;

	// Keep going while we get no error
	while (_zlibErr == Z_OK && _stream.avail_out) {
		if (_stream.avail_in == 0 && !_wrapped->eof()) {
			// If we are out of input data: Read more data, if available.
			_wrapped->read((char*)_buf, BUFSIZE);
			_stream.next_in = _buf;
			_stream.avail_in = _wrapped->gcount();
		}
		_zlibErr = inflate(&_stream, Z_NO_FLUSH);
	}

	// Update the position counter
	_pos += dataSize - _stream.avail_out;

	if (_zlibErr == Z_STREAM_END && _stream.avail_out > 0)
		_eos = true;

	return dataSize - _stream.avail_out;
}

bool GZipReadStream::eos() const {
	return _eos;
}

int32 GZipReadStream::pos() const {
	return _pos;
}

int32 GZipReadStream::size() const {
	return _origSize;
}

bool GZipReadStream::seek(int32 offset, std::ios::seekdir whence) {
	int32 newPos = 0;
	assert(whence != std::ios::end);	// std::ios::end not supported
	switch (whence) {
	case std::ios::beg:
		newPos = offset;
		break;
	case std::ios::cur:
		newPos = _pos + offset;
	default:
		assert(false);
	}

	assert(newPos >= 0);

	if ((uint32)newPos < _pos) {
		// To search backward, we have to restart the whole decompression
		// from the start of the file. A rather wasteful operation, best
		// to avoid it. :/
#if DEBUG
		warning("Backward seeking in GZipReadStream detected");
#endif
		_pos = 0;
		_wrapped->seekg(_start, std::ios_base::beg);
		_zlibErr = inflateReset(&_stream);
		if (_zlibErr != Z_OK)
			return false;	// FIXME: STREAM REWRITE
		_stream.next_in = _buf;
		_stream.avail_in = 0;
	}

	offset = newPos - _pos;

	// Skip the given amount of data (very inefficient if one tries to skip
	// huge amounts of data, but usually client code will only skip a few
	// bytes, so this should be fine.
	byte tmpBuf[1024];
	while (!err() && offset > 0) {
		offset -= read(tmpBuf, MIN((int32)sizeof(tmpBuf), offset));
	}

	_eos = false;
	return true;	// FIXME: STREAM REWRITE
}

void GZipWriteStream::processData(int flushType) {
	// This function is called by both write() and finalize().
	while (_zlibErr == Z_OK && (_stream.avail_in || flushType == Z_FINISH)) {
		if (_stream.avail_out == 0) {
			_wrapped->write((char*)_buf, BUFSIZE);
			if (_wrapped->bad()) {
				_zlibErr = Z_ERRNO;
				break;
			}

			_stream.next_out = _buf;
			_stream.avail_out = BUFSIZE;
		}
		_zlibErr = deflate(&_stream, flushType);
	}
}

GZipWriteStream::GZipWriteStream(std::ofstream *w) : _wrapped(w), _stream() {
	assert(w != 0);

	// Adding 16 to windowBits indicates to zlib that it is supposed to
	// write gzip headers. This feature was added in zlib 1.2.0.4,
	// released 10 August 2003.
	// Note: This is *crucial* for savegame compatibility, do *not* remove!
	_zlibErr = deflateInit2(&_stream,
						Z_DEFAULT_COMPRESSION,
						Z_DEFLATED,
						MAX_WBITS + 16,
						8,
				Z_DEFAULT_STRATEGY);
	assert(_zlibErr == Z_OK);

	_stream.next_out = _buf;
	_stream.avail_out = BUFSIZE;
	_stream.avail_in = 0;
	_stream.next_in = 0;
}

GZipWriteStream::~GZipWriteStream() {
	finalize();
	deflateEnd(&_stream);
}

bool GZipWriteStream::err() const {
	// CHECKME: does Z_STREAM_END make sense here?
	return (_zlibErr != Z_OK && _zlibErr != Z_STREAM_END) || _wrapped->bad();
}

void GZipWriteStream::finalize() {
	if (_zlibErr != Z_OK)
		return;

	// Process whatever remaining data there is.
	processData(Z_FINISH);

	// Since processData only writes out blocks of size BUFSIZE,
	// we may have to flush some stragglers.
	uint remainder = BUFSIZE - _stream.avail_out;
	if (remainder > 0) {
		_wrapped->write((char*)_buf, remainder);
		if (_wrapped->bad())
			_zlibErr = Z_ERRNO;
	}

	// Finalize the wrapped savefile, too
	_wrapped->flush();
}

uint32 GZipWriteStream::write(const void *dataPtr, uint32 dataSize) {
	if (err())
		return 0;

	// Hook in the new data ...
	// Note: We need to make a const_cast here, as zlib is not aware
	// of the const keyword.
	_stream.next_in = const_cast<byte *>((const byte *)dataPtr);
	_stream.avail_in = dataSize;

	// ... and flush it to disk
	processData(Z_NO_FLUSH);

	return dataSize - _stream.avail_in;
}

#endif
