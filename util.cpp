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

bool Filename::empty() const {
	return *_path == 0;
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
	return NULL;
}

const char *Filename::getFullName() const {
	const char *slash;
	if ((slash = strrchr(_path, '/')) || (slash = strrchr(_path, '\\'))) {
		return slash + 1;
	}
	return NULL;
}

const char *Filename::getPath(char *out) const {
	const char *slash;
	if ((slash = strrchr(_path, '/')) || (slash = strrchr(_path, '\\'))) {
		int end = strlen(_path) - strlen(slash) + 1;
		strncpy(out, _path, end);
		out[end] = 0;
		return out;
	}
	// If there was no '/', this was a relative path
	strcpy(out, _path);
	return out;
}

void parseHelpArguments(const char * const argv[], int argc, const char *msg) {
	if (argc < 2 || strcmp(argv[1], "--help") == 0 || stricmp(argv[1], "-h") == 0) {
		if (!msg) {
			printf("\nUsage: %s [-o <output dir> = out/] <file 1> ... <file n>\n", argv[0]);
		}
		else {
			printf(msg, argv[0]);
		}
		exit(2);
	}
}

bool parseOutputArguments(Filename *outputname, const char * const argv[], int argc, int start_arg) {
	char lastchr;

	if (start_arg >= 0 && (strcmp(argv[start_arg], "-o") == 0 || strcmp(argv[start_arg], "--output") == 0)) {
		/* It's a -o argument, can we check next arg? */

		if (start_arg + 1 < argc) {
			outputname->setFullPath(argv[start_arg + 1]);

			/* Ensure last character is a /, this way we force directory output */
			lastchr = outputname->getFullPath()[strlen(outputname->getFullPath()) - 1];
			if (lastchr != '/' && lastchr != '\\') {
				strcat(outputname->_path, "/");
			}
			return true;
		} else {
			error("Expected directory path after '-o' or '--output'.");
		}
	}
	return false;
}
