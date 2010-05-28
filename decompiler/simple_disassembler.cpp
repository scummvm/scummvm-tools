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

void SimpleDisassembler::readParams(Instruction *inst, char *typeString) {
	while (*typeString) {
		Parameter p;
		switch (*typeString) {
		case 'b': //signed byte
			p._type = kSByte;
			p._sbyte = _f.readChar();
			_address++;
			break;
		case 'B': //unsigned byte
			p._type = kByte;
			p._byte = _f.readByte();
			_address++;
			break;
		case 's': //16-bit signed integer (short), little-endian
			p._type = kShort;
			p._short = _f.readSint16LE();
			_address += 2;
			break;
		case 'S': //16-bit signed integer (short), big-endian
			p._type = kShort;
			p._short = _f.readSint16BE();
			_address += 2;
			break;
		case 'w': //16-bit unsigned integer (word), little-endian
			p._type = kUShort;
			p._ushort = _f.readUint16LE();
			_address += 2;
			break;
		case 'W': //16-bit unsigned integer (word), big-endian
			p._type = kUShort;
			p._ushort = _f.readUint16BE();
			_address += 2;
			break;
		case 'i': //32-bit signed integer (int), little-endian
			p._type = kInt;
			p._int = _f.readSint32LE();
			_address += 4;
			break;
		case 'I': //32-bit signed integer (int), big-endian
			p._type = kInt;
			p._int = _f.readSint32BE();
			_address += 4;
			break;
		case 'd': //32-bit unsigned integer (dword), little-endian
			p._type = kUInt;
			p._uint = _f.readUint32LE();
			_address += 4;
			break;
		case 'D': //32-bit unsigned integer (dword), big-endian
			p._type = kUInt;
			p._uint = _f.readUint32BE();
			_address += 4;
			break;
		// Common::File doesn't have readFloat methods, but since the value is stored in a union, we just need to read the right bytes into memory.
		case 'f': //Single-precision float, little-endian
			p._type = kFloat;
			p._uint = _f.readUint32LE();
			_address += 4;
			break;
		case 'F': //Single-precision float, big-endian
			p._type = kFloat;
			p._uint = _f.readUint32BE();
			_address += 4;
			break;
		}
		inst->_params.push_back(p);
		typeString++;
	}
}
