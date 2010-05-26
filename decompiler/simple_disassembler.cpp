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

void SimpleDisassembler::readParams(Instruction *inst, char *typeString)
{
	while (*typeString)
	{
		Parameter p;
		switch (*typeString)
		{
		case 'b': //signed byte
			p._type = kSByte;
			p._sbyte = _f.readChar();
			break;
		case 'B': //unsigned byte
			p._type = kByte;
			p._byte = _f.readByte();
			break;
		case 's': //16-bit integer (short), little-endian
			p._type = kShort;
			p._short = _f.readSint16LE();
			break;
		case 'S': //16-bit integer (short), big-endian
			p._type = kShort;
			p._short = _f.readSint16BE();
			break;
		case 'w': //16-bit integer (word), little-endian
			p._type = kUShort;
			p._ushort = _f.readUint16LE();
			break;
		case 'W': //16-bit integer (word), big-endian
			p._type = kUShort;
			p._ushort = _f.readUint16BE();
			break;
		case 'i': //32-bit integer (int), little-endian
			p._type = kInt;
			p._int = _f.readSint32LE();
			break;
		case 'I': //32-bit integer (int), big-endian
			p._type = kInt;
			p._int = _f.readSint32BE();
			break;
		case 'd': //32-bit integer (dword), little-endian
			p._type = kUInt;
			p._uint = _f.readUint32LE();
			break;
		case 'D': //32-bit integer (dword), big-endian
			p._type = kUInt;
			p._uint = _f.readUint32BE();
			break;
		}
		inst->_params.push_back(p);
		typeString++;
	}
}
