/* ScummVM Tools
 * Copyright (C) 2010 The ScummVM project
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

#include "disassembler.h"
#include "engine.h"
#include <iostream>
#include <boost/format.hpp>

IFFChunk::IFFChunk() {
	_data = NULL;
}

Kyra::Disassembler::Disassembler(Engine *engine) : ::Disassembler(), _engine(engine) {
}

Kyra::Disassembler::~Disassembler() {
	if (_textChunk._data)
		delete[] _textChunk._data;
	if (_ordrChunk._data)
		delete[] _ordrChunk._data;
	if (_dataChunk._data)
		delete[] _dataChunk._data;
}

void Kyra::Disassembler::doDisassemble() throw(UnknownOpcodeException) {
	// Load data
	IFF_ID id;
	id = _f.readUint32BE();
	if (id != MKID_BE('FORM')) {
		std::cerr << boost::format("ERROR: Unexpected IFF magic number 0x%08X (expected 0x%08X)!\n") % id % MKID_BE('FORM');
		return;
	}
	_f.readUint32BE(); // Skip file length
	_formType = _f.readUint32BE();
	if (_formType != MKID_BE('EMC2')) {
		std::cerr << boost::format("ERROR: Unexpected file type 0x%08X (expected 0x%08X)!\n") % _formType % MKID_BE('EMC2');
		return;
	}

	// Read chunks into memory
	do {
		IFFChunk temp;
		temp._chunkType = _f.readUint32BE();
		temp._size = _f.readUint32BE();
		temp._data = new uint8[temp._size];
		_f.read_throwsOnError(temp._data, temp._size);
		switch (temp._chunkType) {
		case MKID_BE('TEXT'):
			_textChunk = temp;
			break;
		case MKID_BE('ORDR'):
			_ordrChunk = temp;
			break;
		case MKID_BE('DATA'):
			_dataChunk = temp;
			break;
		default:
			std::cerr << boost::format("ERROR: Unexpected chunk type 0x%08X!\n") % temp._chunkType;
			delete [] temp._data;
			return;
		}
		if (temp._size % 2 != 0) // Skip padding byte
			_f.readByte();
	} while (_f.pos() != (int)_f.size());

	// Extract strings from TEXT chunk
	uint16 minTextOffset = 0xFFFF;
	for (uint16 i = 0; i < _textChunk._size / 2; ++i) {
		if (minTextOffset > READ_BE_UINT16(&((uint16 *)_textChunk._data)[i])) {
			minTextOffset = READ_BE_UINT16(&((uint16 *)_textChunk._data)[i]);
		}
		if (minTextOffset <= i*2)
			break;
	}
	uint16 numStrings = minTextOffset / 2;
#define posString(x) (char*)&_textChunk._data[READ_BE_UINT16(&((uint16 *)_textChunk._data)[(x)])]
	for (uint16 i = 0; i < numStrings; ++i) {
		_engine->_textStrings.push_back(posString(i));
	}
#undef posString

	// Disassemble
	uint16 numInsts = _dataChunk._size / 2;
	for (uint16 i = 0; i < numInsts; ++i) {
		uint16 address = i*2;
		uint16 code = READ_BE_UINT16(&((uint16 *)_dataChunk._data)[i]);
		int16 opcode = (code >> 8) & 0x1F;
		int16 parameter;

		if (code & 0x8000) {
			opcode = 0;
			parameter = code & 0x7FFF;
		} else if (code & 0x4000) {
			parameter = (int8)(code);
		} else if (code & 0x2000) {
			i++;
			parameter = READ_BE_UINT16(&((uint16 *)_dataChunk._data)[i]);
		} else {
			parameter = 0;
		}

#define ADD_INST _insts.push_back(Instruction());
#define LAST_INST (_insts[_insts.size()-1])
#define OPCODE_MD(name, category, stackChange, codeGenData) \
		ADD_INST; \
		LAST_INST._opcode = opcode; \
		LAST_INST._address = address; \
		LAST_INST._stackChange = stackChange; \
		LAST_INST._name = name; \
		LAST_INST._type = category; \
		LAST_INST._codeGenData = codeGenData; \
		{ \
			Parameter p; \
			p._type = kShort; \
			p._value = parameter; \
			LAST_INST._params.push_back(p);\
		}
#define OPCODE(name, category, stackChange) OPCODE_MD(name, category, stackChange, "");

		// TOOD: Add metadata where applicable
		switch(opcode) {
		case 0:
			OPCODE("jumpTo", kJump, 0);
			break;
		case 1:
			OPCODE("setRetValue", kStore, 0);
			break;
		case 2:
			if (parameter == 0) {
				OPCODE("pushRet", kLoad, 1);
			} else if (parameter == 1) {
				OPCODE("pushPos", kSpecial, 2); // Sets up function call?
			} else {
				// Error: invalid parameter halts execution
			}
			break;
		case 3:
		case 4:
			OPCODE("push", kLoad, 1);
			break;
		case 5:
			OPCODE("pushVar", kLoad, 1);
			break;
		case 6:
			OPCODE("pushBPNeg", kLoad, 1);
			break;
		case 7:
			OPCODE("pushBPAdd", kLoad, 1);
			break;
		case 8:
			if (parameter == 0) {
				OPCODE("popRet", kStore, -1);
			} else if (parameter == 1) {
				OPCODE("popPos", kSpecial, -2); // Returns from function call?
			} else {
				// Error: invalid parameter halts execution
			}
			break;
		case 9:
			OPCODE("popVar", kStore, 1);
			break;
		case 10:
			OPCODE("popBPNeg", kStore, 1);
			break;
		case 11:
			OPCODE("popBPAdd", kStore, 1);
			break;
		case 12:
			OPCODE("addSP", kStack, -parameter);
			break;
		case 13:
			OPCODE("subSP", kStack, parameter);
			break;
		case 14:
			OPCODE("execOpcode", kSpecial, 0); // TODO: Full details of opcode
			break;
		case 15:
			OPCODE("ifNotJmp", kCondJump, -1);
			break;
		case 16:
			if (parameter == 0) {
				OPCODE_MD("boolCast", kUnaryOp, 0, "(bool)");
			} else if (parameter == 1) {
				OPCODE_MD("arithmeticNegate", kUnaryOp, 0, "-");
			} else if (parameter == 2) {
				OPCODE_MD("bitwiseNegate", kUnaryOp, 0, "~");
			} else {
				// Error: invalid parameter halts execution
			}
			break;
		case 17:
			OPCODE("eval", kBinaryOp, -1); // TODO: Full details of opcode
			break;
		case 18:
			OPCODE("setRetAndJmp", kSpecial, -2); // Returns from function call?
			break;
		default:
			throw UnknownOpcodeException(i*2, code);
		}
#undef OPCODE
#undef OPCODE_MD
#undef LAST_INST
#undef ADD_INST
	}
}
