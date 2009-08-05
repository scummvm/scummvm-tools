/* Scumm Tools
 * Copyright (C) 2003-2006  The ScummVM Team
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

#include "stdio.h"
#include "util.h"
#include <stdarg.h>

#ifdef _MSC_VER
	#define	vsnprintf _vsnprintf
#endif

const char *audio_extensions(AudioFormat format) {
	switch(format) {
	case AUDIO_MP3:
		return ".mp3";
	case AUDIO_VORBIS:
		return ".ogg";
	case AUDIO_FLAC:
		return ".fla";
	case AUDIO_NONE:
	default:
		return ".unk";
	}
}

CompressionFormat compression_format(AudioFormat format) {
	switch(format) {
	case AUDIO_MP3:
		return COMPRESSION_MP3;
	case AUDIO_VORBIS:
		return COMPRESSION_OGG;
	case AUDIO_FLAC:
		return COMPRESSION_FLAC;
	case AUDIO_NONE:
	default:
		throw ToolException("Unknown compression format");
	}
}

void error(const char *s, ...) {
	char buf[1024];
	va_list va;

	va_start(va, s);
	vsnprintf(buf, 1024, s, va);
	va_end(va);

	fprintf(stderr, "ERROR: %s!\n", buf);

	exit(1);
}

void warning(const char *s, ...) {
	char buf[1024];
	va_list va;

	va_start(va, s);
	vsnprintf(buf, 1024, s, va);
	va_end(va);

	fprintf(stderr, "WARNING: %s!\n", buf);
}

void debug(int /*level*/, const char *s, ...) {
	char buf[1024];
	va_list va;

	va_start(va, s);
	vsnprintf(buf, 1024, s, va);
	va_end(va);

	fprintf(stderr, "DEBUG: %s!\n", buf);
}

void notice(const char *s, ...) {
	char buf[1024];
	va_list va;

	va_start(va, s);
	vsnprintf(buf, 1024, s, va);
	va_end(va);

	fprintf(stdout, "%s\n", buf);
}

uint32 fileSize(FILE *fp) {
	uint32 sz;
	uint32 pos = ftell(fp);
	fseek(fp, 0, SEEK_END);
	sz = ftell(fp);
	fseek(fp, pos, SEEK_SET);
	return sz;
}

// Filenname implementation
Filename::Filename(const char *path) {
	_path = path;
}
Filename::Filename(std::string path) {
	_path = path;
}

Filename::Filename(const Filename& filename) {
	_path = filename._path;
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
	size_t dot = _path.rfind('.');
	if (dot == std::string::npos) {
		_path += ext;
	} else {
		_path.resize(dot);
		_path += ext;
	}
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
#ifdef _WIN32
	// On Windows paths are case-insensitive
	return scumm_stricmp(tmp.c_str(), ext.c_str()) == 0;
#else
	return tmp == ext;
#endif
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

File::File(const Filename &filepath, FileMode mode) {
	_file = NULL;
	_mode = FILEMODE_READ;

	open(filepath, mode);
}

File::File(const Filename &filepath, const char *mode) {
	_file = NULL;
	_mode = FILEMODE_READ;

	open(filepath, mode);
}

File::~File() {
	close();
}

void File::open(const Filename &filepath, const char *mode) {
	FileMode m = FILEMODE_WRITE;
	do {
		switch(*mode) {
		case 'w': m = FILEMODE_WRITE; break;
		case 'r': m = FILEMODE_READ; break;
		case 'b': m = FileMode(m | FILEMODE_BINARY); break;
		case '+': m = FileMode(m | FILEMODE_APPEND); break;
		default: throw FileException(std::string("Unsupported FileMode ") + mode);
		}
	} while(*++mode);
	
	open(filepath, m);
}

void File::open(const Filename &filepath, FileMode mode) {
	// Clean up previously opened file
	close();

	std::string strmode;
	if (mode & FILEMODE_READ)
		strmode += 'r';
	if (mode & FILEMODE_WRITE)
		strmode += 'w';
	if (mode & FILEMODE_BINARY)
		strmode += 'b';
	if (mode & FILEMODE_APPEND)
		strmode += '+';

	_file = fopen(filepath.getFullPath().c_str(), strmode.c_str());
	_mode = mode;
	_name = filepath;

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

void File::read(void *data, size_t elementSize, size_t elementCount) {
	if (!_file) 
		throw FileException("File is not open");
	if ((_mode & FILEMODE_READ) == 0)
		throw FileException("Tried to read from file opened in write mode (" + _name.getFullPath() + ")");

	size_t data_read = fread(data, elementSize, elementCount, _file);
	if (data_read != elementCount)
		throw FileException("Read beyond the end of file (" + _name.getFullPath() + ")");
}

size_t File::readN(void *data, size_t elementSize, size_t elementCount) {
	if (!_file) 
		throw FileException("File is not open");
	if ((_mode & FILEMODE_READ) == 0)
		throw FileException("Tried to read from file opened in write mode (" + _name.getFullPath() + ")");

	return fread(data, elementSize, elementCount, _file);
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

void File::writeChar(int i) {
	if (!_file) 
		throw FileException("File  is not open");
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

size_t File::write(const void *data, size_t elementSize, size_t elementCount) {
	if (!_file) 
		throw FileException("File is not open");
	if ((_mode & FILEMODE_WRITE) == 0)
		throw FileException("Tried to write to file opened in read mode (" + _name.getFullPath() + ")");

	size_t data_read = fwrite(data, elementSize, elementCount, _file);
	if (data_read != elementCount)
		throw FileException("Could not write to file (" + _name.getFullPath() + ")");

	return data_read;
}

void File::printf(const char *format, ...) {
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

bool File::reachedEOF() const {
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


// Functions to parse the command line

void displayHelp(const char *msg, const char *exename) {
	if (!msg) {
		printf("\nUsage: %s [-o <output dir> = out/] <file 1> ... <file n>\n", exename);
	}
	else {
		printf(msg, exename);
	}
	exit(2);
}

void parseHelpArguments(const char * const argv[], int argc, const char *msg) {
	if (argc < 2 || strcmp(argv[1], "--help") == 0 || scumm_stricmp(argv[1], "-h") == 0) {
		displayHelp(msg, argv[0]);
	}
}

bool parseOutputArguments(Filename *outputname, bool output_directory, const char * const argv[], int argc, int start_arg) {
	if (start_arg >= 0 && (strcmp(argv[start_arg], "-o") == 0 || strcmp(argv[start_arg], "--output") == 0)) {
		/* It's a -o argument, can we check next arg? */

		if (start_arg + 1 < argc) {
			outputname->setFullPath(argv[start_arg + 1]);

			if (output_directory) {
				/* Ensure last character is a /, this way we force directory output */
				char lastchr = outputname->getFullPath()[outputname->getFullPath().size() - 1];
				if (lastchr != '/' && lastchr != '\\') {
					outputname->_path += '/';
				}
			}
			return true;
		} else {
			error("Expected directory path after '-o' or '--output'.");
		}
	}
	return false;
}

bool parseOutputFileArguments(Filename *outputname, const char * const argv[], int argc, int start_arg) {
	return parseOutputArguments(outputname, false, argv, argc, start_arg);
}

bool parseOutputDirectoryArguments(Filename *outputname, const char * const argv[], int argc, int start_arg) {
	return parseOutputArguments(outputname, true, argv, argc, start_arg);
}
