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

#ifndef FILETOOLS_H
#define FILETOOLS_H

#include <fstream>
#include <string>
#include <sstream>
#include "common/endian.h"

template<typename T>
struct Vector3 {
	T _x;
	T _y;
	T _z;
	void setVal(T x, T y, T z) {
		_x = x;
		_y = y;
		_z = z;
	}
};

struct Vector2d {
	float x;
	float y;
	std::string toString() {
		std::stringstream ss;
		ss << x << " " << y;
		return ss.str();
	}
};

struct Vector3d {
	float x;
	float y;
	float z;
	std::string toString() {
		std::stringstream ss;
		ss << x << " " << y << " " << z;
		return ss.str();
	}
};

struct Vector4d {
	float x;
	float y;
	float z;
	float w;
	std::string toString() {
		std::stringstream ss;
		ss << x << " " << y << " " << z << " " << w;
		return ss.str();
	}
};

float readFloat(std::istream &file) {
	float retVal = 0.0f;
	file.read((char *)&retVal, 4);
	retVal = get_float((char *) &retVal);
	return retVal;
}

int readInt(std::istream &file) {
	int retVal = 0;
	file.read((char *)&retVal, 4);
	return FROM_LE_32(retVal);
}

short readShort(std::istream &file) {
	short retVal = 0;
	file.read((char *)&retVal, 2);
	return FROM_LE_16(retVal);
}

int readByte(std::istream &file) {
	char retVal = 0;
	file.read((char *)&retVal, 1);
	return retVal;
}

std::string readString(std::istream &file) {
	int strLength = readInt(file);
	char *readString = new char[strLength];
	file.read(readString, strLength);

	std::string retVal(readString);
	delete readString;

	return retVal;
}

std::string readCString(std::istream &file, int len) {
	char *str = new char[len];
	file.read(str, len);
	std::string retVal = std::string(str);
	delete[] str;
	return retVal;
}

Vector2d *readVector2d(std::istream &file, int count = 1) {
	Vector2d *vec2d = new Vector2d[count];
	for (int i = 0; i < count; i++) {
		vec2d[i].x = readFloat(file);
		vec2d[i].y = readFloat(file);
	}
	return vec2d;
}

Vector3d *readVector3d(std::istream &file, int count = 1) {
	Vector3d *vec3d = new Vector3d[count];
	for (int i = 0; i < count; i++) {
		vec3d[i].x = readFloat(file);
		vec3d[i].y = readFloat(file);
		vec3d[i].z = readFloat(file);
	}
	return vec3d;
}

Vector4d *readVector4d(std::istream &file) {
	Vector4d *vec4d = new Vector4d();
	vec4d->x = readFloat(file);
	vec4d->y = readFloat(file);
	vec4d->z = readFloat(file);
	vec4d->w = readFloat(file);
	return vec4d;
}
//TODO: Endianness
class SeekableReadStream {
public:
	virtual ~SeekableReadStream() {}
	virtual uint32 readUint32() = 0;
	virtual void read(void *target, uint32 length) = 0;
	virtual void seek(int offset, int whence = SEEK_SET) = 0;
};

class MemoryReadStream : public SeekableReadStream {
	byte *_data;
	uint32 _pos;
	uint32 _length;
public:
	MemoryReadStream(byte *data, uint32 length) : _data(data), _length(length), _pos(0) {}
	virtual ~MemoryReadStream() {
		delete[] _data;
	}

	uint32 readUint32() {
		if (_pos + 4 > _length) {
			std::cout << "ERROR: Crossed end of stream" << std::endl;
			return 0;
		}
		uint32 result = 0;
		uint32 *temp = (uint32 *)(_data + _pos);
		result = *temp;
		_pos += 4;
		return result;
	}
	void read(void *target, uint32 length) {
		memcpy(target, _data + _pos, length);
		_pos += length;
	}
	void seek(int offset, int whence = SEEK_SET) {
		switch (whence) {
		case SEEK_SET:
			_pos = offset;
			break;
		case SEEK_CUR:
			_pos += offset;
			break;
		case SEEK_END:
			std::cout << "ERROR: SEEK_END not implemented yet" << std::endl;
			break;
		}
	}
};

#endif
