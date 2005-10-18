/* UnPak - Extractor for Kyrandia .pak archives
 * Copyright (C) 2004  Johannes Schickel
 * Copyright (C) 2004-2005  The ScummVM Team
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
 * $Header$
 *
 */

#ifndef UNPAK_H
#define UNPAK_H

#include "util.h"

// standard Package format for Kyrandia games
class PAKFile {
	public:
		PAKFile(const char* file);
		~PAKFile() { delete [] _buffer; }
		
		void drawFilelist(void);
		void outputAllFiles(void);
		void outputFile(const char* file);

		bool isValid(void) {return (_buffer != 0);}
		bool isOpen(void) {return _open;}

	private:
		bool _open;
		uint8* _buffer; // the whole file	
		uint32 _filesize;	
};

#endif
