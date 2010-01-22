/* Scumm Tools
 * Copyright (C) 2004-2006  The ScummVM Team
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
 * $URL: https://scummvm.svn.sourceforge.net/svnroot/scummvm/tools/branches/gsoc2009-gui/utils/stream.h $
 * $Id: stream.h 40868 2009-05-24 15:19:28Z lordhoto $
 *
 */

#ifndef COMMON_STREAM_H
#define COMMON_STREAM_H

#include "common/scummsys.h"

namespace Common {

/**
 * Virtual base class for both ReadStream and WriteStream.
 */
class Stream {
public:
	virtual ~Stream() {}

	/**
	 * Returns true if any I/O failure occured.
	 * This flag is never cleared automatically. In order to clear it,
	 * client code has to call clearIOFailed() explicitly.
	 *
	 * @todo Instead of returning a plain bool, maybe we should define
	 *       a list of error codes which can be returned here.
	 */
	virtual bool ioFailed() const { return false; }

	/**
	 * Reset the I/O error status.
	 */
	virtual void clearIOFailed() {}
};


/**
 * Generic interface for a readable data stream.
 */
class ReadStream : virtual public Stream {
public:
	/**
	 * Returns true if the end of the stream has been reached.
	 */
	virtual bool eos() const = 0;

	/**
	 * Read data from the stream. Subclasses must implement this
	 * method; all other read methods are implemented using it.
	 *
	 * @param dataPtr	pointer to a buffer into which the data is read
	 * @param dataSize	number of bytes to be read
	 * @return the number of bytes which were actually read.
	 */
	virtual uint32 read(void *dataPtr, uint32 dataSize) = 0;


	// The remaining methods all have default implementations; subclasses
	// need not (and should not) overload them.

	byte readByte() {
		byte b = 0;
		read(&b, 1);
		return b;
	}

	int8 readSByte() {
		int8 b = 0;
		read(&b, 1);
		return b;
	}

	uint16 readUint16LE() {
		uint16 a = readByte();
		uint16 b = readByte();
		return a | (b << 8);
	}

	uint32 readUint32LE() {
		uint32 a = readUint16LE();
		uint32 b = readUint16LE();
		return (b << 16) | a;
	}

	uint16 readUint16BE() {
		uint16 b = readByte();
		uint16 a = readByte();
		return a | (b << 8);
	}

	uint32 readUint32BE() {
		uint32 b = readUint16BE();
		uint32 a = readUint16BE();
		return (b << 16) | a;
	}

	int16 readSint16LE() {
		return (int16)readUint16LE();
	}

	int32 readSint32LE() {
		return (int32)readUint32LE();
	}

	int16 readSint16BE() {
		return (int16)readUint16BE();
	}

	int32 readSint32BE() {
		return (int32)readUint32BE();
	}
};


/**
 * Interface for a seekable & readable data stream.
 *
 * @todo We really need better error handling here!
 *       Like seek should somehow indicate whether it failed.
 */
class SeekableReadStream : virtual public ReadStream {
public:

	virtual uint32 pos() const = 0;
	virtual uint32 size() const = 0;

	virtual void seek(int32 offset, int whence = SEEK_SET) = 0;

	void skip(uint32 offset) { seek(offset, SEEK_CUR); }
};

/**
 * SubReadStream provides access to a ReadStream restricted to the range
 * [currentPosition, currentPosition+end).
 * Manipulating the parent stream directly /will/ mess up a substream.
 * Likewise, manipulating two substreams of a parent stream will cause them to
 * step on each others toes.
 */
class SubReadStream : virtual public ReadStream {
protected:
	ReadStream *_parentStream;
	bool _disposeParentStream;
	uint32 _pos;
	uint32 _end;
	bool _eos;
public:
	SubReadStream(ReadStream *parentStream, uint32 end, bool disposeParentStream = false)
		: _parentStream(parentStream),
		  _disposeParentStream(disposeParentStream),
		  _pos(0),
		  _end(end),
		  _eos(false) {
		assert(parentStream);
	}
	
	~SubReadStream() {
		if (_disposeParentStream) delete _parentStream;
	}

	virtual bool eos() const { return _eos; }
	
	virtual uint32 read(void *dataPtr, uint32 dataSize) {
		if (dataSize > _end - _pos) {
			dataSize = _end - _pos;
			_eos = true;
		}

		dataSize = _parentStream->read(dataPtr, dataSize);
		_eos |= _parentStream->eos();
		_pos += dataSize;

		return dataSize;
	}
};

/*
 * SeekableSubReadStream provides access to a SeekableReadStream restricted to
 * the range [begin, end).
 * The same caveats apply to SeekableSubReadStream as do to SeekableReadStream.
 */
class SeekableSubReadStream : public SubReadStream, public SeekableReadStream {
protected:
	SeekableReadStream *_parentStream;
	uint32 _begin;
public:
	SeekableSubReadStream(SeekableReadStream *parentStream, uint32 begin, uint32 end, bool disposeParentStream = false)
		: SubReadStream(parentStream, end, disposeParentStream),
		_parentStream(parentStream),
		_begin(begin) {
		assert(_begin <= _end);
		_pos = _begin;
		_parentStream->seek(_pos);
		_eos = false;
	}
	
	virtual uint32 pos() const { return _pos - _begin; }
	virtual uint32 size() const { return _end - _begin; }
	virtual bool eos() const { return _parentStream->eos(); }

	virtual void seek(int32 offset, int whence = SEEK_SET) {
		assert(_pos >= _begin);
		assert(_pos <= _end);

		switch(whence) {
		case SEEK_END:
			offset = size() + offset;
			// fallthrough
		case SEEK_SET:
			_pos = _begin + offset;
			break;
		case SEEK_CUR:
			_pos += offset;
		}

		assert(_pos >= _begin);
		assert(_pos <= _end);

		_parentStream->seek(_pos);
	}

	virtual uint32 read(void *dataPtr, uint32 dataSize) {
		return SubReadStream::read(dataPtr, dataSize);
	}
};


}	// End of namespace Common

#endif
