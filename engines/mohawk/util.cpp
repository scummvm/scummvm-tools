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
 * $URL: https://scummvm.svn.sourceforge.net/svnroot/scummvm/tools/branches/gsoc2009-gui/util.cpp $
 * $Id: util.cpp 41547 2009-06-15 14:24:36Z Remere $
 *
 */

#include "util.h"
#include <stdarg.h>

#ifdef _MSC_VER
	#define	vsnprintf _vsnprintf
#endif

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

void debug(int level, const char *s, ...) {
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

uint8 readByte(FILE *fp) {
	return fgetc(fp);
}

uint16 readUint16BE(FILE *fp) {
	uint16 ret = 0;
	ret |= fgetc(fp) << 8;
	ret |= fgetc(fp);
	return ret;
}

uint16 readUint16LE(FILE *fp) {
	uint16 ret = 0;
	ret |= fgetc(fp);
	ret |= fgetc(fp) << 8;
	return ret;
}

uint32 readUint32BE(FILE *fp) {
	uint32 ret = 0;
	ret |= fgetc(fp) << 24;
	ret |= fgetc(fp) << 16;
	ret |= fgetc(fp) << 8;
	ret |= fgetc(fp);
	return ret;
}

uint32 readUint32LE(FILE *fp) {
	uint32 ret = 0;
	ret |= fgetc(fp);
	ret |= fgetc(fp) << 8;
	ret |= fgetc(fp) << 16;
	ret |= fgetc(fp) << 24;
	return ret;
}

void writeByte(FILE *fp, uint8 b) {
	fwrite(&b, 1, 1, fp);
}

void writeUint16BE(FILE *fp, uint16 value) {
	writeByte(fp, (uint8)(value >> 8));
	writeByte(fp, (uint8)(value));
}

void writeUint16LE(FILE *fp, uint16 value) {
	writeByte(fp, (uint8)(value));
	writeByte(fp, (uint8)(value >> 8));
}

void writeUint32BE(FILE *fp, uint32 value) {
	writeByte(fp, (uint8)(value >> 24));
	writeByte(fp, (uint8)(value >> 16));
	writeByte(fp, (uint8)(value >> 8));
	writeByte(fp, (uint8)(value));
}

void writeUint32LE(FILE *fp, uint32 value) {
	writeByte(fp, (uint8)(value));
	writeByte(fp, (uint8)(value >> 8));
	writeByte(fp, (uint8)(value >> 16));
	writeByte(fp, (uint8)(value >> 24));
}

uint32 fileSize(FILE *fp) {
	uint32 sz;
	uint32 pos = ftell(fp);
	fseek(fp, 0, SEEK_END);
	sz = ftell(fp);
	fseek(fp, pos, SEEK_SET);
	return sz;
}

Filename::Filename(const char *path) {
	strcpy(_path, path);
}

Filename::Filename(const Filename& filename) {
	strcpy(_path, filename._path);
}

Filename& Filename::operator=(const Filename& filename) {
	strcpy(_path, filename._path);
	return *this;
}

void Filename::setFullPath(const char *path) {
	strcpy(_path, path);
}

Filename *Filename::setFullName(const char *newname) {
	char p[1024];
	if (getPath(p)) {
		strcat(p, newname);
		strcpy(_path, p);
		return this;
	}
	return NULL;
}

void Filename::addExtension(const char *ext) {
	strcat(_path, ext);
}

void Filename::setExtension(const char *ext) {
	char *dot = strrchr(_path, '.');
	if(!dot)
		dot = _path + strlen(_path) - 1;
	// Don't copy the dot
	if(*ext == '.')
		ext++;
	strcpy(dot+1, ext);
}

bool Filename::equals(const Filename *other) const {
#ifdef _WIN32
	// On Windows paths are case-insensitive
	return scumm_stricmp(_path, other->_path) == 0;
#else
	return strcmp(_path, other->_path) == 0;
#endif
}

bool Filename::empty() const {
	return *_path == 0;
}

bool Filename::hasExtension(const char *suffix) const {
	const char *dot = strrchr(_path, '.');
	if(!dot)
		dot = _path + strlen(_path);

	// Check that dot position is less than /, since some
	// directories contain ., like /home/.data/file
	const char *slash = strrchr(_path, '/');
	if(slash && slash > dot)
		return false;

	slash = strrchr(_path, '\\');
	if(slash && slash > dot)
		return false;

	// We compare extensions, skip any dots
	if(*dot == '.')
		dot++;
	if(*suffix == '.')
		suffix++;

#ifdef _WIN32
	// On Windows paths are case-insensitive
	return scumm_stricmp(dot, suffix) == 0;
#else
	return strcmp(dot, suffix) == 0;
#endif
}

const char *Filename::getFullPath() const {
	return _path;
}

const char *Filename::getFullName(char *out) const {
	const char *slash;
	if ((slash = strrchr(_path, '/')) || (slash = strrchr(_path, '\\'))) {
		strcpy(out, slash + 1);
		return out;
	}
	strcpy(out, _path);
	return out;
}

const char *Filename::getFullName() const {
	const char *slash;
	if ((slash = strrchr(_path, '/')) || (slash = strrchr(_path, '\\'))) {
		return slash + 1;
	}
	return _path;
}

const char *Filename::getPath(char *out) const {
	const char *slash;
	if ((slash = strrchr(_path, '/')) || (slash = strrchr(_path, '\\'))) {
		int end = strlen(_path) - strlen(slash) + 1;
		strncpy(out, _path, end);
		out[end] = '\0';
		return out;
	}
	// If there was no '/', this was a local path
	out[0] = '\0';
	return out;
}

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
				char lastchr = outputname->getFullPath()[strlen(outputname->getFullPath()) - 1];
				if (lastchr != '/' && lastchr != '\\') {
					strcat(outputname->_path, "/");
				}
			} else {
				char* lastchr = outputname->_path + strlen(outputname->getFullPath()) - 1;
				if (*lastchr == '/' && *lastchr == '\\') {
					*lastchr = '\0';
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
