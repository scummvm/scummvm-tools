/* DeKyra - Basic Kyrandia script disassembler
 * Copyright (C) 2004-  Johannes Schickel
 * Copyright (C) 2004-  The ScummVM Team
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * $URL$ * $Id$
 *
 */

#ifndef DEKYRA_H
#define DEKYRA_H

#include "util.h"

class Script {
	public:
		Script(const char* filename);
		~Script() { delete _scriptFile; }
		
		void decodeTextArea(void);
		void decodeScript(void);		
		bool decodeSpecialScript(int32 script);
		bool isOpen(void) { return (_scriptFile != 0); }
		
		uint32 getNextScriptPos(uint32 current_start);
		
	protected:
		
		enum ScriptChunkTypes {
			kForm = 0,
			kEmc2Ordr = 1,
			kText = 2,
			kData = 3,
			kCountChunkTypes
		};
		
		struct ScriptChunk {
			uint32 _size;
			uint8* _data; // by TEXT used for count of texts, by EMC2ODRD it is used for a count of somewhat
			uint8* _additional; // currently only used for TEXT
		};
		
		ScriptChunk _chunks[kCountChunkTypes];
		
		uint32 _scriptSize;
		uint32 _currentPos;	// current instruction pos
		uint8* _scriptFile;
};

#endif
