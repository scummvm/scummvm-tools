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
 * $URL: https://scummvm.svn.sourceforge.net/svnroot/scummvm/tools/branches/gsoc2009-gui/utils/file.cpp $
 * $Id: file.cpp 23138 2006-06-15 15:44:06Z h00ligan $
 *
 */

#include "common/util.h"
#include "engines/mohawk/utils/file.h"


namespace Common {


File::File(FILE*file)
	: _handle(file), _ioFailed(false) {
}


File::~File() {
	close();
}

void File::close() {
	_handle = NULL;
}

bool File::isOpen() const {
	return _handle != NULL;
}

bool File::ioFailed() const {
	return _ioFailed != 0;
}

void File::clearIOFailed() {
	_ioFailed = false;
}

bool File::eof() const {
	if (_handle == NULL) {
		error("File::eof: File is not open!");
		return false;
	}

	return feof(_handle) != 0;
}

uint32 File::pos() const {
	if (_handle == NULL) {
		error("File::pos: File is not open!");
		return 0;
	}

	return ftell(_handle);
}

uint32 File::size() const {
	if (_handle == NULL) {
		error("File::size: File is not open!");
		return 0;
	}

	uint32 oldPos = ftell(_handle);
	fseek(_handle, 0, SEEK_END);
	uint32 length = ftell(_handle);
	fseek(_handle, oldPos, SEEK_SET);

	return length;
}

void File::seek(int32 offs, int whence) {
	if (_handle == NULL) {
		error("File::seek: File is not open!");
		return;
	}

	if (fseek(_handle, offs, whence) != 0)
		clearerr(_handle);
}

uint32 File::read(void *ptr, uint32 len) {
	byte *ptr2 = (byte *)ptr;
	size_t real_len;

	if (_handle == NULL) {
		error("File::read: File is not open!");
		return 0;
	}

	if (len == 0)
		return 0;

	real_len = fread(ptr2, 1, len, _handle);
	if (real_len < len) {
		_ioFailed = true;
	}

	return (uint32)real_len;
}

}	// End of namespace Common
