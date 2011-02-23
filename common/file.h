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

#ifndef COMMON_FILE_H
#define COMMON_FILE_H

#include "common/scummsys.h"
#include "common/noncopyable.h"

#include "tool_exception.h"


namespace Common {

/**
 * Something unexpected happened while reading / writing to a file.
 * Usually premature end, or that it could not be opened (write / read protected).
 */
class FileException : public ToolException {
public:
	FileException(std::string error, int retcode = -1) : ToolException(error, retcode) {}
};

/**
 * A file path, can be queried for different parts
 * and the parts can be modified separately.
 */
class Filename {
public:
	std::string _path;

	Filename();
	Filename(std::string path);
	Filename(const char *path);
	Filename(const Filename &path);
	Filename& operator=(const Filename &fn);

	inline bool operator==(const Filename &fn){
		return equals(fn);
	}

	/**
	 * Change the entire path including directory, volume and actual filname.
	 *
	 * @param path The new path.
	 */
	void setFullPath(const std::string &path);

	/**
	 * Sets the name of the file referred to, does not change the directory referred to.
	 *
	 * @param name New filename.
	 */
	void setFullName(const std::string &name);

	/**
	 * Adds an extension to the filename (does not replace any current extension).
	 *
	 * @param ext Extension to add.
	 */
	void addExtension(const std::string &ext);

	/**
	 * Sets the extension of the filename, replacing any current one, or adding one if there isn't any.
	 *
	 * @param ext The new extension of the filename.
	 */
	void setExtension(const std::string &ext);

	/**
	 * Returns true if the file has that extension.
	 *
	 * @param ext Extension to check for, only last extension is checked.
	 * @return True if the filename has that extension.
	 */
	bool hasExtension(std::string ext) const;

	/**
	 * Has the filename been set to anything.
	 */
	bool empty() const;

	/**
	 * Returns true if the file exists, does NOT work for directories.
	 */
	bool exists() const;

	/**
	 * Returns true if the file is a directory, which is if the path ends with '/'.
	 *
	 * @return true if the path looks like a directory.
	 */
	bool directory() const;

	/**
	 * True if the filenames are different (relative and absolute paths will NOT compare equally).
	 *
	 * @param other The filename to compare to.
	 * @return True if they are equal.
	 */
	bool equals(const Filename &other) const;


	/**
	 * Returns the entire path.
	 */
	std::string getFullPath() const;

	/**
	 * Returns the filename (ie. strips all directories from the path).
	 */
	std::string getFullName() const;

	/**
	 * Returns the name of the file, excluding extension and directories.
	 * Note that in the case of multiple extensions, only the last extension is stripped.
	 */
	std::string getName() const;

	/**
	 * Get the extension of the filename, only the last extension in case of many.
	 */
	std::string getExtension() const;

	/**
	 * Returns the path of the filename, the name and extension of the file is stripped.
	 */
	std::string getPath() const;
};

/**
 * Possible modes for opening files
 */
enum FileMode {
	FILEMODE_READ = 1,
	FILEMODE_WRITE = 2,
	FILEMODE_BINARY = 4
};

/**
 * A basic wrapper around the FILE class.
 * Offers functionality to write words easily, and deallocates the FILE
 * automatically on destruction.
 */
class File : public NonCopyable {
public:
	/**
	 * Opens the given file path as an in/out stream, depending on the
	 * second argument.
	 *
	 * @param filename	file to open
	 * @param mode		mode to open the file in
	 */
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
	 * @param filename	file to open
	 * @param mode		mode to open the file in
	 */
	void open(const Filename &filename, const char *mode);

	/**
	 * Closes the file, if it's open.
	 */
	void close();

	/**
	 * Check whether the file is open.
	 */
	bool isOpen() const { return _file != 0; }

	/**
	 * Sets the xor mode of the file, bytes written / read to the file
	 * will be XORed with this value. This value is *not* reset when
	 * opening a new file.
	 * Only works for write* and read* operation, not for the array
	 * "read" and "write" methods
	 */
	void setXorMode(uint8 xormode);

	/**
	 * Reads a single character (equivalent of fgetc).
	 */
	int readChar();
	/**
	 * Read a single unsigned byte.
	 * @throws FileException if file is not open / if read failed.
	 */
	uint8 readByte();
	/**
	 * Read a single 16-bit word, big endian.
	 * @throws FileException if file is not open / if read failed.
	 */
	uint16 readUint16BE();
	/**
	 * Read a single 16-bit word, little endian.
	 * @throws FileException if file is not open / if read failed.
	 */
	uint16 readUint16LE();
	/**
	 * Read a single 32-bit word, big endian.
	 * @throws FileException if file is not open / if read failed.
	 */
	uint32 readUint32BE();
	/**
	 * Read a single 32-bit word, little endian.
	 * @throws FileException if file is not open / if read failed.
	 */
	uint32 readUint32LE();

	/**
	 * Read a single 16-bit word, big endian.
	 * @throws FileException if file is not open / if read failed.
	 */
	int16 readSint16BE();
	/**
	 * Read a single 16-bit word, little endian.
	 * @throws FileException if file is not open / if read failed.
	 */
	int16 readSint16LE();
	/**
	 * Read a single 32-bit word, big endian.
	 * @throws FileException if file is not open / if read failed.
	 */
	int32 readSint32BE();
	/**
	 * Read a single 32-bit word, little endian.
	 * @throws FileException if file is not open / if read failed.
	 */
	int32 readSint32LE();


	/**
	 * Works the same way as fread, but throws on error or if it could
	 * not read all elements.
	 *
	 * @param dataPtr	pointer to a buffer into which the data is read
	 * @param dataSize	number of bytes to be read
	 */
	void read_throwsOnError(void *dataPtr, size_t dataSize);

	/**
	 * Works the same way as fread, does NOT throw if it could not read all elements
	 * still throws if file is not open.
	 *
	 * @param dataPtr	pointer to a buffer into which the data is read
	 * @param dataSize	number of bytes to be read
	 * @return the number of bytes which were actually read.
	 */
	size_t read_noThrow(void *dataPtr, size_t dataSize);

	/**
	 * Reads a full string, until NULL or EOF.
	 * @throws FileException if file is not open / if read failed.
	 */
	std::string readString();

	/**
	 * Reads the queried amount of bytes and returns it as a string.
	 * @throws FileException if file is not open / if read failed.
	 *
	 * @param len How many bytes to read
	 * @return the data read
	 */
	std::string readString(size_t len);

	/**
	 * Reads a string, using until NULL or EOF or CR/LF.
	 * @throws FileException if file is not open / if read failed.
	 */
	void scanString(char *result);

	/**
	 * Writes a single character (equivalent of fputc).
	 */
	void writeChar(char c);
	/**
	 * Writes a single byte to the file.
	 * @throws FileException if file is not open / if write failed.
	 */
	void writeByte(uint8 b);
	/**
	 * Writes a single 16-bit word to the file, big endian.
	 * @throws FileException if file is not open / if write failed.
	 */
	void writeUint16BE(uint16 value);
	/**
	 * Writes a single 16-bit word to the file, little endian.
	 * @throws FileException if file is not open / if write failed.
	 */
	void writeUint16LE(uint16 value);
	/**
	 * Writes a single 32-bit word to the file, big endian.
	 * @throws FileException if file is not open / if write failed.
	 */
	void writeUint32BE(uint32 value);
	/**
	 * Writes a single 32-bit word to the file, little endian.
	 * @throws FileException if file is not open / if write failed.
	 */
	void writeUint32LE(uint32 value);

	/**
	 * Works the same way as fwrite, but throws on error or if
	 * it could not write all data.
	 *
	 * @param dataPtr	pointer to the data to be written
	 * @param dataSize	number of bytes to be written
	 * @return the number of bytes which were actually written.
	 */
	size_t write(const void *dataPtr, size_t dataSize);

	/**
	 * Works the same as fprintf.
	 */
	void print(const char *format, ...);

	/**
	 * Seek to the specified position in the stream.
	 *
	 * @param offset how many bytes to jump
	 * @param origin SEEK_SET, SEEK_CUR or SEEK_END
	 */
	void seek(long offset, int origin);

	/**
	 * Resets the file pointer to the start of the file, in essence the same as re-opening it.
	 */
	void rewind();

	/**
	 * Returns current position of the file cursor.
	 */
	int pos() const;

	/**
	 * Check whether an error occurred.
	 */
	int err() const;

	void clearErr();

	/**
	 * True if there is nothing more to read from this file.
	 */
	bool eos() const;

	/**
	 * Returns the length of the file, in bytes, does not move the cursor.
	 */
	uint32 size() const;

	// FIXME: Remove this method eventually
	FILE *getFileHandle() { return _file; }

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


/**
 * Remove the specified file.
 * Currently this simply call unlink() internally,
 * but by using this wrapper we have an easier time
 * staying compatible with Windows.
 */
int removeFile(const char *path);

/**
 * Test if the specified path is a directory.
 * This is just a wrapper around stat/S_ISDIR.
 */
bool isDirectory(const char *path);

} // End of namespace Common


#endif
