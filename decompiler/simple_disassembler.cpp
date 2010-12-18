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

#include "simple_disassembler.h"

SimpleDisassembler::SimpleDisassembler(InstVec &insts) : Disassembler(insts) {
}

void SimpleDisassembler::readParams(InstPtr inst, const char *typeString) {
	while (*typeString) {
		inst->_params.push_back(readParameter(inst, *typeString));
		typeString++;
	}
}

ValuePtr SimpleDisassembler::readParameter(InstPtr inst, char type) {
	ValuePtr retval = NULL;
	switch (type) {
	case 'b': // signed byte
		retval = new IntValue(_f.readChar(), true);
		_address++;
		break;
	case 'B': // unsigned byte
		retval = new IntValue((uint32)_f.readByte(), false);
		_address++;
		break;
	case 's': // 16-bit signed integer (short), little-endian
		retval = new IntValue(_f.readSint16LE(), true);
		_address += 2;
		break;
	case 'S': // 16-bit signed integer (short), big-endian
		retval = new IntValue(_f.readSint16BE(), true);
		_address += 2;
		break;
	case 'w': // 16-bit unsigned integer (word), little-endian
		retval = new IntValue((uint32)_f.readUint16LE(), false);
		_address += 2;
		break;
	case 'W': // 16-bit unsigned integer (word), big-endian
		retval = new IntValue((uint32)_f.readUint16BE(), false);
		_address += 2;
		break;
	case 'i': // 32-bit signed integer (int), little-endian
		retval = new IntValue(_f.readSint32LE(), true);
		_address += 4;
		break;
	case 'I': // 32-bit signed integer (int), big-endian
		retval = new IntValue(_f.readSint32BE(), true);
		_address += 4;
		break;
	case 'd': // 32-bit unsigned integer (dword), little-endian
		retval = new IntValue(_f.readUint32LE(), false);
		_address += 4;
		break;
	case 'D': // 32-bit unsigned integer (dword), big-endian
		retval = new IntValue(_f.readUint32BE(), false);
		_address += 4;
		break;
	}
	return retval;
}
