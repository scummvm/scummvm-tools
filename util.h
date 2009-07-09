/* Scumm Tools
 * Copyright (C) 2002-2006 The ScummVM project
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

#ifndef UTIL_H
#define UTIL_H

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#if !defined(_MSC_VER)
#include <unistd.h>
#endif

#ifdef WIN32
#include <io.h>
#include <process.h>
#endif

#include <exception>
#include <string>
#include <stdexcept>

/*
 * Some useful types
 */

typedef unsigned char byte;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef signed char int8;
typedef signed short int16;
#ifdef __amigaos4__
#include <exec/types.h>
#include <stdlib.h>
#else
typedef unsigned int uint32;
typedef signed int int32;
#endif


/*
 * Tools should not implement the actual main() themselves
 * when they are compiled as a part of the master tool. So
 * we make a macro to rename it automatically
 *
 * Perhaps EXPORT_MAIN is a bad name though.
 */

#ifdef EXPORT_MAIN
	#define export_main(tool_name) main_ ## tool_name
#else
	#define export_main(tool_name) main
#endif

/*
 * Various utility macros
 */

#if defined(_MSC_VER)

	#define scumm_stricmp stricmp
	#define scumm_strnicmp _strnicmp
	#define snprintf _snprintf

	#define SCUMM_LITTLE_ENDIAN
	#pragma once
	#pragma warning( disable : 4068 ) /* turn off "unknown pragma" warning */
	#pragma warning( disable : 4996 ) /* turn off warnings about unsafe functions */

#elif defined(__MINGW32__)

	#define scumm_stricmp stricmp
	#define scumm_strnicmp strnicmp

	#define SCUMM_LITTLE_ENDIAN

#elif defined(UNIX)

	#define scumm_stricmp strcasecmp
	#define scumm_strnicmp strncasecmp

	#if defined(__DECCXX) /* Assume alpha architecture */
	#define INVERSE_MKID
	#define SCUMM_NEED_ALIGNMENT
	#endif

#else

	#error No system type defined

#endif


/*
 * GCC specific stuff
 */
#if defined(__GNUC__)
        #define GCC_PACK __attribute__((packed))
        #define NORETURN __attribute__((__noreturn__))
        #define GCC_PRINTF(x,y) __attribute__((format(printf, x, y)))
#else
        #define GCC_PACK
        #define GCC_PRINTF(x,y)
#endif

#ifndef ARRAYSIZE
#define ARRAYSIZE(x) ((int)(sizeof(x) / sizeof(x[0])))
#endif

#if defined(INVERSE_MKID)
#define MKID_BE(a) ((uint32) \
		(((a) >> 24) & 0x000000FF) | \
		(((a) >>  8) & 0x0000FF00) | \
		(((a) <<  8) & 0x00FF0000) | \
		(((a) << 24) & 0xFF000000))

#else
#  define MKID_BE(a) ((uint32)(a))
#endif

static inline uint32 SWAP_32(uint32 a) {
	return uint16(((a >> 24) & 0xFF) | ((a >> 8) & 0xFF00) | ((a << 8) & 0xFF0000) |
		((a << 24) & 0xFF000000));
}

static inline uint16 SWAP_16(uint16 a) {
	return uint16(((a >> 8) & 0xFF) | ((a << 8) & 0xFF00));
}

#ifndef FORCEINLINE
#define FORCEINLINE static inline
#endif

FORCEINLINE uint16 READ_LE_UINT16(const void *ptr) {
	const byte *b = (const byte *)ptr;
	return uint16((b[1] << 8) + b[0]);
}
FORCEINLINE uint32 READ_LE_UINT32(const void *ptr) {
	const byte *b = (const byte *)ptr;
	return (b[3] << 24) + (b[2] << 16) + (b[1] << 8) + (b[0]);
}
FORCEINLINE void WRITE_LE_UINT16(void *ptr, uint16 value) {
	byte *b = (byte *)ptr;
	b[0] = (byte)(value >> 0);
	b[1] = (byte)(value >> 8);
}
FORCEINLINE void WRITE_LE_UINT32(void *ptr, uint32 value) {
	byte *b = (byte *)ptr;
	b[0] = (byte)(value >>  0);
	b[1] = (byte)(value >>  8);
	b[2] = (byte)(value >> 16);
	b[3] = (byte)(value >> 24);
}

FORCEINLINE uint16 READ_BE_UINT16(const void *ptr) {
	const byte *b = (const byte *)ptr;
	return uint16((b[0] << 8) + b[1]);
}
FORCEINLINE uint32 READ_BE_UINT32(const void *ptr) {
	const byte *b = (const byte *)ptr;
	return uint32((b[0] << 24) + (b[1] << 16) + (b[2] << 8) + (b[3]));
}
FORCEINLINE void WRITE_BE_UINT16(void *ptr, uint16 value) {
	byte *b = (byte *)ptr;
	b[0] = (byte)(value >> 8);
	b[1] = (byte)(value >> 0);
}
FORCEINLINE void WRITE_BE_UINT32(void *ptr, uint32 value) {
	byte *b = (byte *)ptr;
	b[0] = (byte)(value >> 24);
	b[1] = (byte)(value >> 16);
	b[2] = (byte)(value >>  8);
	b[3] = (byte)(value >>  0);
}

#if defined(__GNUC__)
#define NORETURN_PRE
#define NORETURN_POST	__attribute__((__noreturn__))
#elif defined(_MSC_VER)
#define NORETURN_PRE	_declspec(noreturn)
#define NORETURN_POST
#else
#define NORETURN_PRE
#define NORETURN_POST
#endif


/* File I/O */
uint8 readByte(FILE *fp);
uint16 readUint16BE(FILE *fp);
uint16 readUint16LE(FILE *fp);
uint32 readUint32BE(FILE *fp);
uint32 readUint32LE(FILE *fp);
void writeByte(FILE *fp, uint8 b);
void writeUint16BE(FILE *fp, uint16 value);
void writeUint16LE(FILE *fp, uint16 value);
void writeUint32BE(FILE *fp, uint32 value);
void writeUint32LE(FILE *fp, uint32 value);
uint32 fileSize(FILE *fp);

/* Misc stuff */
void NORETURN_PRE error(const char *s, ...) NORETURN_POST;
void warning(const char *s, ...);
void debug(int level, const char *s, ...);
void notice(const char *s, ...);


/** 
 * Different audio formats
 * You can bitwise them to represent several formats
 */
enum AudioFormat {
	AUDIO_NONE = 0,
	AUDIO_VORBIS = 1,
	AUDIO_FLAC = 2,
	AUDIO_MP3 = 4,
	AUDIO_ALL = AUDIO_VORBIS | AUDIO_FLAC | AUDIO_MP3
};

/**
 * Another enum, which cannot be ORed
 * These are the values written to the output files
 */
enum CompressionFormat {
	COMPRESSION_NONE = 0,
	COMPRESSION_MP3 = 1,
	COMPRESSION_OGG = 2,
	COMPRESSION_FLAC = 3
};

const char *audio_extensions(AudioFormat format);
CompressionFormat compression_format(AudioFormat format);

// Below this line are more C++ ish interface
// Above is kept for compatibility with non-converted tools

/**
 * Throw an exception of this type (or subtype of it), if the tool fails fatally.
 * This type is intended for general errors
 */
class ToolException : public std::runtime_error {
public:
	/**
	 * Construct an exception, with an appropriate error message
	 * A return value for the tool should be supplied if none is appropriate
	 * @todo If the tools are even more C++ized, the tool should decide retcode itself, not by exception
	 *
	 * @param error The error message
	 * @param retcode The return value of the process
	 */
	ToolException(std::string error, int retcode = -1) : std::runtime_error(error), _retcode(retcode) {}

	int _retcode;
};

/**
 * Something unexpected happened while reading / writing to a file
 * Usually premature end, or that it could not be opened (write / read protected)
 */
class FileException : public ToolException {
public: 
	FileException(std::string error, int retcode = -1) : ToolException(error, retcode) {}
};

/**
 * A file path, can be queried for different parts
 * and the parts can be modified seperately
 */
struct Filename {
	std::string _path;

	Filename(std::string path = "");
	Filename(const char *path);
	Filename(const Filename &path);
	Filename& operator=(const Filename &fn);

	inline bool operator==(const Filename &fn){
		return equals(fn);
	}
	
	void setFullPath(const std::string &path);
	void setFullName(const std::string &name);
	void addExtension(const std::string &ext);
	void setExtension(const std::string &ext);

	bool hasExtension(std::string suffix) const;
	bool empty() const;
	bool equals(const Filename &other) const;
	
	// Doesn't work
	bool mkdir(int permission = 077);

	const std::string &getFullPath() const;
	std::string getFullName() const;
	std::string getPath() const;
};

/**
 * Possible modes for opening files
 */
enum FileMode {
	FILEMODE_READ = 1,
	FILEMODE_WRITE = 2,
	FILEMODE_BINARY = 4,
};

/**
 * A basic wrapper around the FILE class.
 * Offers functionality to write words easily, and deallocates the FILE
 * automatically on destruction.
 */
class File {
public:
	/**
	 * Opens the given file path as an in/out stream, depending on the
	 * second argument.
	 * File is always opened in binary mode
	 *
	 * @param filename The file to open
	 * @param mode The mode to open the file in, can be either OR mask or in text
	 */
	File(const Filename &filename, FileMode mode);
	File(const Filename &filename, const char *mode);
	/**
	 * Create an empty file, used for two-step construction.
	 */
	File();
	~File();

	/**
	 * Opens the given file path as an in/out stream, depending on the
	 * second argument.
	 *
	 * @param filename The file to open
	 * @param mode The mode to open the file in
	 */
	void open(const Filename &filename, const char *mode);
	void open(const Filename &filename, FileMode mode);

	/**
	 * Closes the file, if it's open
	 */
	void close();

	/**
	 * Sets the xor mode of the file, bytes written / read to the file
	 * will be XORed with this value. This value is *not* reset when
	 * opening a new file.
	 * Only works for write* and read* operation, not for the array
	 * "read" and "write" methods
	 */
	void setXorMode(uint8 xormode);
	
	/**
	 * Read a single unsigned byte.
	 * Throws FileException if file is not open / if read failed.
	 */
	uint8 readByte();
	/**
	 * Read a single 16-bit word, big endian.
	 * Throws FileException if file is not open / if read failed.
	 */
	uint16 readU16BE();
	/**
	 * Read a single 16-bit word, little endian.
	 * Throws FileException if file is not open / if read failed.
	 */
	uint16 readU16LE();
	/**
	 * Read a single 32-bit word, big endian.
	 * Throws FileException if file is not open / if read failed.
	 */
	uint32 readU32BE();
	/**
	 * Read a single 32-bit word, little endian.
	 * Throws FileException if file is not open / if read failed.
	 */
	uint32 readU32LE();

	/**
	 * Works the same way as fread, but throws on error or if it could
	 * not read all elements.
	 *
	 * @param data Where to put the read data
	 * @param elementSize the size of one element (in bytes)
	 * @param elementCount the number of elements to read
	 */
	size_t read(void *data, size_t elementSize, size_t elementCount);

	
	/**
	 * Writes a single byte to the file.
	 * Throws FileException if file is not open / if write failed.
	 */
	void writeByte(uint8 b);
	/**
	 * Writes a single 16-bit word to the file, big endian.
	 * Throws FileException if file is not open / if write failed.
	 */
	void writeU16BE(uint16 value);
	/**
	 * Writes a single 16-bit word to the file, little endian.
	 * Throws FileException if file is not open / if write failed.
	 */
	void writeU16LE(uint16 value);
	/**
	 * Writes a single 32-bit word to the file, big endian.
	 * Throws FileException if file is not open / if write failed.
	 */
	void writeU32BE(uint32 value);
	/**
	 * Writes a single 32-bit word to the file, little endian.
	 * Throws FileException if file is not open / if write failed.
	 */
	void writeU32LE(uint32 value);

	/**
	 * Works the same way as fwrite, but throws on error or if
	 * it could not write all data.
	 *
	 * @param data Where to read data from
	 * @param elementSize the size of one element (in bytes)
	 * @param elementCount the number of elements to read
	 */
	size_t write(const void *data, size_t elementSize, size_t elementCount);

	/**
	 * Seek to the specified position in the stream.
	 *
	 * @param offset how many bytes to jump
	 * @param origin SEEK_SET, SEEK_CUR or SEEK_END
	 */
	void seek(long offset, int origin);

	/**
	 * Returns the position in the stream.
	 */
	int pos();

	/**
	 * Returns the length of the file, in bytes, does not move the cursor.
	 */
	uint32 size();

	/**
	 * We implicitly convert into a FILE, so we can use fread() etc. directly.
	 * @todo get rid of this ASAP
	 */
	operator FILE *() {return _file;}

protected:
	/** The mode the file was opened in. */
	FileMode _mode;
	/** Internal reference to the file. */
	FILE *_file;
	/** The name of the file, used for better error messages. */
	Filename _name;
	/** xor with this value while reading/writing (default 0), does not work for "read"/"write", only for byte operations. */
	uint8 _xormode;
};

// This generates a warning when used, so we don't use fclose accidently on 
// a File object (this is a VERY EASY error to do when converting, and a warning
// really helps find those cases)
void fclose(File& f);

void displayHelp(const char *msg = NULL, const char *exename = NULL);
void parseHelpArguments(const char * const argv[], int argc, const char *msg = NULL);
bool parseOutputFileArguments(Filename *outputname, const char * const argv[], int argc, int start_arg);
bool parseOutputDirectoryArguments(Filename *outputname, const char * const argv[], int argc, int start_arg);


#endif
