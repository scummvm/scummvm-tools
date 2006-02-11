/* DeKyra - Basic Kyrandia script disassembler
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
		const char* getParamsOnStack(void);
		const char* stringAtIndex(int32 index);

		void pushStack(int32 value) { _stack[_stackPos++] = value; }
		void registerValue(int16 reg, int16 value) { _registers[reg] = value; }
		int32 checkReg(int16 reg) { return _registers[reg]; }

		int32 popStack(void) { return _stack[--_stackPos]; }
		int32& topStack(void) { return _stack[_stackPos]; }

		int32 param(int32 index);
		const char* paramString(int32 index) { return stringAtIndex(param(index)); }
		
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
		uint8* _scriptFile;

		int32 _nextScriptPos;
		int32 _instructionPos;
		int32 _stackPos;
		int32 _tempPos;

		uint32 _returnValue;
		uint16 _argument;
		uint8 _currentCommand;
		uint32 _currentOpcode;

		int32 _stack[128];	// the stack
		int32 _registers[32];   // registers of the interpreter

		void execCommand(uint32 command);

		void goToLine(void);		// 0x00
		void setReturn(void);		// 0x01
		void pushRetRec(void);		// 0x02
		void push(void);		// 0x03 & 0x04
		void pushVar(void);		// 0x05
		void pushFrameNeg(void);	// 0x06
		void pushFramePos(void);	// 0x07
		void popRetRec(void);		// 0x08
		void popVar(void);		// 0x09
		void popFrameNeg(void);		// 0x0A
		void popFramePos(void);		// 0x0B
		void addToSP(void);		// 0x0C
		void subFromSP(void);		// 0x0D
		void execOpcode(void);		// 0x0E
		void ifNotGoTo(void);		// 0x0F
		void negate(void);		// 0x10
		void evaluate(void);		// 0x11
};

#endif
