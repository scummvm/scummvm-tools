/* ScummVM Tools
 * Copyright (C) 2002-2009 The ScummVM project
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

#include "file.h"
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <sys/stat.h>   // for stat()
#include <sys/types.h>
#ifndef _MSC_VER
#include <unistd.h>	// for unlink()
#else

// Add a definition for S_IFDIR for MSVC
#ifndef S_ISDIR
#ifndef S_IFDIR
#ifndef _S_IFDIR
#define S_IFDIR		(-1)
#else
#define S_IFDIR		_S_IFDIR
#endif
#endif
#define S_ISDIR(m)	(((m)&S_IFDIR)==S_IFDIR)
#endif

#endif

namespace Common {

// Filenname implementation
Filename::Filename() {
}
Filename::Filename(const char *path) : _path(path) {
}
Filename::Filename(std::string path) : _path(path) {
}

Filename::Filename(const Filename& filename) : _path(filename._path) {
}

Filename& Filename::operator=(const Filename& filename) {
	_path = filename._path;
	return *this;
}

void Filename::setFullPath(const std::string &path) {
	_path = path;
}

void Filename::setFullName(const std::string &newname) {
	_path = getPath() + newname;
}

void Filename::addExtension(const std::string &ext) {
	_path += ext;
}

void Filename::setExtension(const std::string &ext) {
	// FIXME: quick and dirty hack. But then so is this whole class...
	std::string path = getPath();
	std::string name = getName();
	_path = path + name + ext;
}

bool Filename::equals(const Filename &other) const {
#ifdef _WIN32
	// On Windows paths are case-insensitive
	return scumm_stricmp(_path.c_str(), other._path.c_str()) == 0;
#else
	return _path == other._path;
#endif
}

bool Filename::empty() const {
	return _path.empty();
}

bool Filename::directory() const {
	return getFullName().size() == 0;
}

bool Filename::exists() const {
	// This fails if we don't have permission to read the file
	// but in most cases, that's the same thing for us.
	FILE *f = fopen(_path.c_str(), "r");
	if (f) {
		fclose(f);
		return true;
	}
	return false;
}

bool Filename::hasExtension(std::string ext) const {
	size_t dot = _path.rfind('.');
	if (dot == std::string::npos)
		return false;

	// Check that dot position is less than /, since some
	// directories contain ., like /home/.data/file
	size_t slash = _path.rfind('/');
	if (slash != std::string::npos)
		if (slash > dot)
			return false;

	slash = _path.rfind('\\');
	if (slash != std::string::npos)
		if (slash > dot)
			return false;

	// We compare extensions, skip any dots
	if (_path[dot] == '.')
		dot++;
	if (ext[0] == '.')
		ext = ext.substr(1);

	std::string tmp = _path.substr(dot);
	// On Windows paths are case-insensitive
	return scumm_stricmp(tmp.c_str(), ext.c_str()) == 0;
}

std::string Filename::getFullPath() const {
	return _path;
}

std::string Filename::getFullName() const {
	size_t slash = _path.rfind('/');
	if (slash == std::string::npos)
		slash = _path.rfind('\\');

	if (slash == std::string::npos)
		return _path;

	return _path.substr(slash + 1);
}

std::string Filename::getName() const {
	size_t slash = _path.rfind('/');
	size_t dot = _path.rfind('.');
	if (slash == std::string::npos)
		slash = _path.rfind('\\');

	if (dot == std::string::npos)
		dot = _path.size();

	if (slash == std::string::npos)
		return _path.substr(0, dot);

	if (dot < slash)
		dot = _path.size();

	return _path.substr(slash + 1, dot - slash - 1);
}

std::string Filename::getExtension() const {
	size_t slash = _path.rfind('/');
	size_t dot = _path.rfind('.');
	if (slash == std::string::npos)
		slash = _path.rfind('\\');

	if (slash == std::string::npos)
		slash = 0;

	if (dot == std::string::npos)
		return "";

	if (dot < slash)
		return "";

	return _path.substr(dot + 1);
}

std::string Filename::getPath() const {
	size_t slash = _path.rfind('/');
	if (slash == std::string::npos)
		slash = _path.rfind('\\');

	if (slash == std::string::npos)
		return "";

	return _path.substr(0, slash + 1);
}

// File interface
// While this does massive duplication of the code above, it's required to make sure that
// unconverted tools are backwards-compatible

File::File() {
	_file = NULL;
	_mode = FILEMODE_READ;
	_xormode = 0;
}

File::File(const Filename &filepath, const char *mode) {
	_file = NULL;
	_mode = FILEMODE_READ;
	_xormode = 0;

	open(filepath, mode);
}

File::~File() {
	close();
}

void File::open(const Filename &filepath, const char *mode) {

	// Clean up previously opened file
	close();

	_file = fopen(filepath.getFullPath().c_str(), mode);

	FileMode m = FILEMODE_READ;
	do {
		switch(*mode) {
		case 'w': m = FILEMODE_WRITE; break;
		case 'r': m = FILEMODE_READ; break;
		case 'b': m = FileMode(m | FILEMODE_BINARY); break;
		case '+': m = FileMode(m | FILEMODE_READ | FILEMODE_WRITE); break;
		default: throw FileException(std::string("Unsupported FileMode ") + mode);
		}
	} while (*++mode);
	_mode = m;

	_name = filepath;
	_xormode = 0;

	if (!_file)
		throw FileException("Could not open file " + filepath.getFullPath());
}

void File::close() {
	if (_file)
		fclose(_file);
	_file = NULL;
}

void File::setXorMode(uint8 xormode) {
	_xormode = xormode;
}

int File::readChar() {
	if (!_file)
		throw FileException("File is not open");
	if ((_mode & FILEMODE_READ) == 0)
		throw FileException("Tried to read from file opened in write mode (" + _name.getFullPath() + ")");

	int u8 = fgetc(_file);
	if (u8 == EOF)
		throw FileException("Read beyond the end of file (" + _name.getFullPath() + ")");
	u8 ^= _xormode;
	return u8;
}

uint8 File::readByte() {
	int u8 = readChar();
	return (uint8)u8;
}

uint16 File::readUint16BE() {
	uint16 ret = 0;
	ret |= uint16(readByte() << 8ul);
	ret |= uint16(readByte());
	return ret;
}

uint16 File::readUint16LE() {
	uint16 ret = 0;
	ret |= uint16(readByte());
	ret |= uint16(readByte() << 8ul);
	return ret;
}

uint32 File::readUint32BE() {
	uint32 ret = 0;
	ret |= uint32(readByte() << 24);
	ret |= uint32(readByte() << 16);
	ret |= uint32(readByte() << 8);
	ret |= uint32(readByte());
	return ret;
}

uint32 File::readUint32LE() {
	uint32 ret = 0;
	ret |= uint32(readByte());
	ret |= uint32(readByte() << 8);
	ret |= uint32(readByte() << 16);
	ret |= uint32(readByte() << 24);
	return ret;
}

int16 File::readSint16BE() {
	int16 ret = 0;
	ret |= int16(readByte() << 8ul);
	ret |= int16(readByte());
	return ret;
}

int16 File::readSint16LE() {
	int16 ret = 0;
	ret |= int16(readByte());
	ret |= int16(readByte() << 8ul);
	return ret;
}

int32 File::readSint32BE() {
	int32 ret = 0;
	ret |= int32(readByte() << 24);
	ret |= int32(readByte() << 16);
	ret |= int32(readByte() << 8);
	ret |= int32(readByte());
	return ret;
}

int32 File::readSint32LE() {
	int32 ret = 0;
	ret |= int32(readByte());
	ret |= int32(readByte() << 8);
	ret |= int32(readByte() << 16);
	ret |= int32(readByte() << 24);
	return ret;
}

void File::read_throwsOnError(void *dataPtr, size_t dataSize) {
	size_t data_read = read_noThrow(dataPtr, dataSize);
	if (data_read != dataSize)
		throw FileException("Read beyond the end of file (" + _name.getFullPath() + ")");
}

size_t File::read_noThrow(void *dataPtr, size_t dataSize) {
	if (!_file)
		throw FileException("File is not open");
	if ((_mode & FILEMODE_READ) == 0)
		throw FileException("Tried to read from file opened in write mode (" + _name.getFullPath() + ")");

	return fread(dataPtr, 1, dataSize, _file);
}

std::string File::readString() {
	if (!_file)
		throw FileException("File is not open");
	if ((_mode & FILEMODE_READ) == 0)
		throw FileException("Tried to read from file opened in write mode (" + _name.getFullPath() + ")");

	std::string s;
	try {
		char c;
		while ((c = readByte())) {
			s += c;
		}
	} catch (FileException &) {
		// pass, we reached EOF
	}

	return s;
}

std::string File::readString(size_t len) {
	if (!_file)
		throw FileException("File is not open");
	if ((_mode & FILEMODE_READ) == 0)
		throw FileException("Tried to read from file opened in write mode (" + _name.getFullPath() + ")");

	std::string s('\0', len);
	std::string::iterator is = s.begin();

	char c;
	while ((c = readByte())) {
		*is = c;
	}

	return s;
}

void File::scanString(char *result) {
	if (!_file)
		throw FileException("File is not open");
	if ((_mode & FILEMODE_READ) == 0)
		throw FileException("Tried to write to file opened in read mode (" + _name.getFullPath() + ")");

	fscanf(_file, "%s", result);
}

void File::writeChar(char i) {
	if (!_file)
		throw FileException("File is not open");
	if ((_mode & FILEMODE_WRITE) == 0)
		throw FileException("Tried to write to a file opened in read mode (" + _name.getFullPath() + ")");

	i ^= _xormode;

	if (fwrite(&i, 1, 1, _file) != 1)
		throw FileException("Could not write to file (" + _name.getFullPath() + ")");
}

void File::writeByte(uint8 b) {
	writeChar(b);
}

void File::writeUint16BE(uint16 value) {
	writeByte((uint8)(value >> 8));
	writeByte((uint8)(value));
}

void File::writeUint16LE(uint16 value) {
	writeByte((uint8)(value));
	writeByte((uint8)(value >> 8));
}

void File::writeUint32BE(uint32 value) {
	writeByte((uint8)(value >> 24));
	writeByte((uint8)(value >> 16));
	writeByte((uint8)(value >> 8));
	writeByte((uint8)(value));
}

void File::writeUint32LE(uint32 value) {
	writeByte((uint8)(value));
	writeByte((uint8)(value >> 8));
	writeByte((uint8)(value >> 16));
	writeByte((uint8)(value >> 24));
}

size_t File::write(const void *dataPtr, size_t dataSize) {
	if (!_file)
		throw FileException("File is not open");
	if ((_mode & FILEMODE_WRITE) == 0)
		throw FileException("Tried to write to file opened in read mode (" + _name.getFullPath() + ")");

	assert(_xormode == 0);	// FIXME: This method does not work in XOR mode (and probably shouldn't)

	size_t data_read = fwrite(dataPtr, 1, dataSize, _file);
	if (data_read != dataSize)
		throw FileException("Could not write to file (" + _name.getFullPath() + ")");

	return data_read;
}

void File::print(const char *format, ...) {
	if (!_file)
		throw FileException("File is not open");
	if ((_mode & FILEMODE_WRITE) == 0)
		throw FileException("Tried to write to file opened in read mode (" + _name.getFullPath() + ")");


	va_list va;

	va_start(va, format);
	vfprintf(_file, format, va);
	va_end(va);
}

void File::seek(long offset, int origin) {
	if (!_file)
		throw FileException("File is not open");

	if (fseek(_file, offset, origin) != 0)
		throw FileException("Could not seek in file (" + _name.getFullPath() + ")");
}

void File::rewind() {
	return ::rewind(_file);
}

int File::pos() const {
	return ftell(_file);
}

int File::err() const {
	return ferror(_file);
}

void File::clearErr() {
	clearerr(_file);
}

bool File::eos() const {
	return feof(_file) != 0;
}

uint32 File::size() const {
	uint32 sz;
	uint32 p = ftell(_file);
	fseek(_file, 0, SEEK_END);
	sz = ftell(_file);
	fseek(_file, p, SEEK_SET);
	return sz;
}

int removeFile(const char *path) {
	return unlink(path);
}

bool isDirectory(const char *path) {
	struct stat st;
	return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

} // End of namespace Common

