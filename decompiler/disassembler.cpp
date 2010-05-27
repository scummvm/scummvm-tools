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

Disassembler::Disassembler() {
	_addressBase = 0;
}

void Disassembler::open(const char *filename) {
	_f.open(filename, "rb");
}

void Disassembler::dumpDisassembly(const char *filename) {
	Common::File _output;
	_output.open(filename, "w");

	char buf[1024];
	int length;

	for (size_t i = 0; i < _insts.size(); i++)
	{
		Instruction inst = _insts[i];
		length = sprintf(buf, "%08x: %s ",inst._address, inst._name.c_str());
		for (size_t j = 0; j < inst._params.size(); j++)
		{
			Parameter p = inst._params[j];
			if (j != 0)
				length += sprintf(&buf[length], ", ");
			switch(p._type)
			{
				case kSByte:
					length += sprintf(&buf[length], "%d", p._sbyte);
					break;
				case kByte:
					length += sprintf(&buf[length], "%u", p._byte);
					break;
				case kShort:
					length += sprintf(&buf[length], "%d", p._short);
					break;
				case kUShort:
					length += sprintf(&buf[length], "%u", p._ushort);
					break;
				case kInt:
					length += sprintf(&buf[length], "%d", p._int);
					break;
				case kUInt:
					length += sprintf(&buf[length], "%u", p._uint);
					break;
				case kFloat:
					length += sprintf(&buf[length], "%f", p._float);
					break;
			}
		}
		buf[length] = '\n';
		_output.write(buf, length + 1);
	}
}
