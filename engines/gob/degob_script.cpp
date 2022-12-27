/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

/* GobEngine Script disassembler */

#include <assert.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

#include "degob_script.h"
#include "common/endian.h"
#include "common/util.h"

ExtTable::ExtTable(byte *data, uint32 size, byte *dataCom, uint32 sizeCom) :
	_data(data), _size(size), _dataCom(dataCom), _sizeCom(sizeCom) {

	assert(data && (size >= 3));

	init();
}

ExtTable::~ExtTable() {
	delete[] _items;
}

void ExtTable::init() {
	byte *data = _data;

	_itemsCount = READ_LE_UINT16(data);
	_items = _itemsCount ? new Item[_itemsCount] : 0;
	data += 3;

	assert(_size >= (uint32) (3 + _itemsCount * 10));

	for (uint16 i = 0; i < _itemsCount; i++, data += 10) {
		_items[i].offset = READ_LE_UINT32(data);
		_items[i].size = READ_LE_UINT16(data + 4);
		_items[i].width = READ_LE_UINT16(data + 6);
		_items[i].height = READ_LE_UINT16(data + 8);
		_items[i].isPacked = (_items[i].width & 0x8000) != 0;

		_items[i].width &= 0x7FFF;

		if (_items[i].width & 0x4000)
			_items[i].size += 1 << 16;
		if (_items[i].width & 0x2000)
			_items[i].size += 2 << 16;
		if (_items[i].width & 0x1000)
			_items[i].size += 4 << 16;
		if (_items[i].height == 0)
			_items[i].size += _items[i].width << 16;

		_items[i].width &= 0xFFF;
	}
}

byte *ExtTable::getItem(uint16 i, uint32 &size) const {
	assert(i < _itemsCount);

	Item &item = _items[i];
	int32 offset = item.offset;
	byte *data, *dataBuf;

	if (offset < 0) {
		offset = -(offset + 1);

		if (!_dataCom)
			error("commun.exN needed");

		assert(((uint32) offset) < _sizeCom);

		data = _dataCom;
	} else {
		assert(((uint32) offset) < _size);

		data = _data + (3 + _itemsCount * 10);
	}

	size = item.size;

	if (item.isPacked)
		dataBuf = new byte[size + 2];
	else
		dataBuf = new byte[size];

	memcpy(dataBuf, data + offset, size);

	if (item.isPacked) {
		byte *packedBuf = dataBuf;
		dataBuf = unpack(packedBuf, size);
		delete[] packedBuf;
	}

	return dataBuf;
}

// Some LZ77-variant
byte *ExtTable::unpack(const byte *packedData, uint32 &size) const {
	byte *dataBuf, *tmpBuf, *dest;
	uint32 counter;
	uint16 tmpIndex;
	uint16 cmd;
	int16 off;
	byte len;

	counter = size = READ_LE_UINT32(packedData);
	dataBuf = new byte[size];

	tmpBuf = new byte[4114];

	for (int i = 0; i < 4078; i++)
		tmpBuf[i] = 0x20;
	tmpIndex = 4078;

	packedData += 4;

	dest = dataBuf;
	cmd = 0;
	while (1) {
		cmd >>= 1;
		if ((cmd & 0x0100) == 0) {
			cmd = *packedData | 0xFF00;
			packedData++;
		}
		if ((cmd & 1) != 0) { /* copy */
			*dest++ = *packedData;
			tmpBuf[tmpIndex] = *packedData;
			packedData++;
			tmpIndex++;
			tmpIndex %= 4096;
			counter--;
			if (counter == 0)
				break;
		} else { /* copy string */

			off = *packedData++;
			off |= (*packedData & 0xF0) << 4;
			len = (*packedData & 0x0F) + 3;
			packedData++;

			for (int i = 0; i < len; i++) {
				*dest++ = tmpBuf[(off + i) % 4096];
				counter--;
				if (counter == 0) {
					delete[] tmpBuf;
					return dataBuf;
				}
				tmpBuf[tmpIndex] = tmpBuf[(off + i) % 4096];
				tmpIndex++;
				tmpIndex %= 4096;
			}

		}
	}
	delete[] tmpBuf;

	return dataBuf;
}

Script::Script(byte *totData, uint32 totSize, ExtTable *extTable) :
	_totData(totData), _ptr(totData), _totSize(totSize), _extTable(extTable) {

	assert(totData && (totSize > 128));

	_indent = 0;

	loadProperties(totData);
}
Script::~Script() {}

uint16 Script::getStart() const { return _start; }
uint16 Script::getTextCenter() const { return _textCenter; }
uint16 Script::getVarsCount() const { return _varsCount; }
uint32 Script::getTotTextCount() const { return _totTextCount; }
uint32 Script::getTotResOffset() const { return _totResOffset; }
uint16 Script::getTotResCount() const { return _totResCount; }
uint16 Script::getAnimDataSize() const { return _animDataSize; }
uint8 Script::getVerScript() const { return _verScript; }
uint8 Script::getVerIMEX() const { return _verIMEX; }
uint8 Script::getSuffixIM() const { return _suffixIM; }
uint8 Script::getSuffixEX() const { return _suffixEX; }
uint32 Script::getFuncNamesCount() const { return _funcOffsetsNames.size(); }

void Script::putString(const char *s) const {
	printf("%s", s);
}
void Script::print(const char *s, ...) const {
	char buf[1024];
	va_list va;

	va_start(va, s);
	vsnprintf(buf, 1024, s, va);
	va_end(va);

	putString(buf);
}
void Script::printIndent() const {
	print("%08d:", getPos());
	for (uint32 i = 0; i < _indent; i++)
		putString("	");
}
void Script::printLine(const char *str) const {
	printIndent(); putString(str); putString("\n");
}
std::string Script::printStr(const char *s, ...) const {
	char buf[1024];
	va_list va;

	va_start(va, s);
	vsnprintf(buf, 1024, s, va);
	va_end(va);

	return buf;
}

void Script::incIndent() { _indent++; }
void Script::decIndent() { _indent--; }

uint32 Script::getPos() const { return _ptr - _totData; }
void Script::skip(uint32 off) { seek(off, SEEK_CUR); }
void Script::seek(uint32 off, int whence) {
	switch (whence) {
	case SEEK_END:
		off = _totSize - off;
		// fall through
	case SEEK_SET:
		_ptr = _totData + off;
	break;

	case SEEK_CUR:
		_ptr += (int32) off;
		break;
	}
}

uint8  Script::peekUint8(int32 offset)  const { return *(_ptr + offset); }
uint16 Script::peekUint16(int32 offset) const { return READ_LE_UINT16(_ptr + offset); }
uint32 Script::peekUint32(int32 offset) const { return READ_LE_UINT32(_ptr + offset); }
uint8  Script::readUint8()        { uint8  i = peekUint8();  _ptr += 1; return i; }
uint16 Script::readUint16()       { uint16 i = peekUint16(); _ptr += 2; return i; }
uint32 Script::readUint32()       { uint32 i = peekUint32(); _ptr += 4; return i; }
const char *Script::peekString(int32 offset) const { return (char *) _ptr + offset; }
const char *Script::readString()  { const char *i = peekString(); _ptr += strlen(i) + 1; return i; }

void Script::skipExpr(char stopToken) {
	int16 dimCount;
	byte operation;
	int16 num;
	int16 dim;

	num = 0;
	while (1) {
		operation = readUint8();

		if ((operation >= 14) && (operation <= 29)) {
			switch (operation) {
			case 14:
				skip(4);
				if (peekUint8() == 97)
					skip(1);
				break;

			case 17:
			case 18:
			case 20:
			case 23:
			case 24:
				skip(2);
				break;

			case 19:
				skip(4);
				break;

			case 21:
				skip(1);
				break;

			case 22:
				skip(strlen((char *) _ptr) + 1);
				break;

			case 25:
				skip(2);
				if (peekUint8() == 13) {
					skip(1);
					skipExpr(12);
				}
				break;

			case 15:
				skip(2);
				// fall through
			case 16:
			case 26:
			case 27:
			case 28:
				dimCount = _ptr[2];
				// skip header and dimensions
				skip(3 + dimCount);
				// skip indices
				for (dim = 0; dim < dimCount; dim++)
					skipExpr(12);

				if ((operation == 28) && (peekUint8() == 13)) {
					skip(1);
					skipExpr(12);
				}
				break;

			case 29:
				skip(1);
				skipExpr(10);
			}
			continue;
		} // if ((operation >= 14) && (operation <= 29))

		if (operation == 9) {
			num++;
			continue;
		}

		if ((operation == 11) || ((operation >= 1) && (operation <= 8)))
			continue;

		if ((operation >= 30) && (operation <= 37))
			continue;

		if (operation == 10)
			num--;

		if (operation != stopToken)
			continue;

		if ((stopToken != 10) || (num < 0))
			return;
	}
}

std::string Script::readExpr(char stopToken) {
	std::string expr;
	int16 dimCount;
	byte operation;
	int16 num;
	int16 dim;
	byte *arrDesc;
	byte func;

	num = 0;
	while (1) {
		operation = readUint8();

		while ((operation == 14) || (operation == 15)) {
			if (operation == 14) {
				expr += printStr("#%d#", readUint16() * 4);

				skip(2);
				if (peekUint8() == 97)
					skip(1);
			} else if (operation == 15) {
				expr += printStr("#%d->", readUint16() * 4);

				skip(2);
				uint8 var_A = readUint8();

				skip(var_A);

				for (int i = 0; i < var_A; i++)
					expr += readExpr(12) + "->";

				expr += "#";

				if (peekUint8() == 97)
					skip(1);
			}

			operation = readUint8();
		}

		if ((operation >= 16) && (operation <= 29)) {
			// operands

			switch (operation) {
			case 17: // uint16 variable load
				expr += printStr("var16_%d", readUint16() * 2);
				break;

			case 18: // uint8 variable load:
				expr += printStr("var8_%d", readUint16());
				break;

			case 19: // int32/uint32 immediate
				expr += printStr("%d", readUint32());
				break;

			case 20: // int16 immediate
				expr += printStr("%d", (int16) readUint16());
				break;

			case 21: // int8 immediate
				expr += printStr("%d", (int8) readUint8());
				break;

			case 22: // string immediate
				expr += printStr("\"%s\"", readString());
				break;

			case 23: // uint32 variable load
			case 24: // uint32 variable load as uint16
				expr += printStr("var32_%d", readUint16() * 4);
				break;

			case 25: // string variable load
				expr += printStr("(&var8_%d)", readUint16() * 4);
				if (peekUint8() == 13) {
					skip(1);
					expr += "+{*";
					expr += readExpr(12); // this also prints the closing }
				}
				break;

			case 16: // uint8 array access
			case 26: // uint32 array access
			case 27: // uint16 array access
			case 28: // string array access

				if (operation == 16)
					expr += printStr("var8_%d[", readUint16());
				else if (operation == 26)
					expr += printStr("var32_%d[", readUint16() * 4);
				else if (operation == 27)
					expr += printStr("var16_%d[", readUint16() * 2);
				else if (operation == 28)
					expr += printStr("(&var8_%d[", readUint16() * 4);

				dimCount = readUint8();
				arrDesc = _ptr;
				skip(dimCount);
        for (dim = 0; dim < dimCount; dim++) {
          expr += readExpr(12) + printStr(" of %d", (int16) arrDesc[dim]);
					if (dim != dimCount - 1)
						expr += "][";
        }

				expr += "]";
				if (operation == 28)
					expr += ")";

				if ((operation == 28) && (peekUint8() == 13)) {
					skip(1);
					expr += "+{*" + readExpr(12);
				}
				break;

			case 29: // function
				func = readUint8();
				if (func == 5)
					expr += "sqr(";
				else if (func == 10)
					expr += "rand(";
				else if (func == 7)
					expr += "abs(";
				else if ((func == 0) || (func == 1) || (func == 6))
					expr += "sqrt(";
				else
					expr += "id(";
				expr += readExpr(10);
				break;
			}
			continue;
		}		// if ((operation >= 16) && (operation <= 29))

		// operators
		switch (operation) {
		case 9:
			expr += "(";
			break;

		case 11:
			expr += "!";
			break;

		case 10:
			expr += ")";
			break;

		case 1:
			expr += "-";
			break;

		case 2:
			expr += "+";
			break;

		case 3:
			expr += "-";
			break;

		case 4:
			expr += "|";
			break;

		case 5:
			expr += "*";
			break;

		case 6:
			expr += "/";
			break;

		case 7:
			expr += "%";
			break;

		case 8:
			expr += "&";
			break;

		case 30:
			expr += " || ";
			break;

		case 31:
			expr += " && ";
			break;

		case 32:
			expr += "<";
			break;

		case 33:
			expr += "<=";
			break;

		case 34:
			expr += ">";
			break;

		case 35:
			expr += ">=";
			break;

		case 36:
			expr += "==";
			break;

		case 37:
			expr += "!=";
			break;

		case 99:
//			expr += "\n";
			break;

		case 12:
			expr += "}";
			if (stopToken != 12)
				warning("Closing paren without opening?");
			break;

		default:
			// Unknown token -- don't know how to handle this, so just
			// skip over everything until we reach the stopToken.
			while (((char) readUint8()) != stopToken) {}
			return printStr("Invalid operator in expression: <%d>", (int16) operation);
			break;
		}

		if (operation == 9) {
			num++;
			continue;
		}

		if ((operation == 11) || ((operation >= 1) && (operation <= 8)))
			continue;

		if ((operation >= 30) && (operation <= 37))
			continue;

		if (operation == 10)
			num--;

		if (operation == stopToken) {
			if ((stopToken != 10) || (num < 0)) {
				return expr;
			}
		}
	}
}

std::string Script::readVarIndex(uint16 *arg_0, uint16 *arg_4) {
	std::string expr, pref;
	byte *arrDesc;
	int16 dim;
	int16 dimCount;
	int16 operation;
	int16 temp;

	operation = readUint8();

	while ((operation == 14) || (operation == 15)) {
		if (operation == 14) {
			pref += printStr("#%d#", readUint16() * 4);

			if (arg_0)
				*arg_0 = peekUint16();
			if (arg_4)
				*arg_4 = 14;

			skip(2);
			if (peekUint8() != 97)
				return expr;

			skip(1);
		} else if (operation == 15) {
			pref += printStr("#%d->", readUint16() * 4);

			if (arg_0)
				*arg_0 = peekUint16();
			if (arg_4)
				*arg_4 = 14;

			skip(2);
			uint8 var_A = readUint8();

			skip(var_A);

			for (int i = 0; i < var_A; i++)
				pref += readExpr(12) + "->";

			pref += "#";

			if (peekUint8() != 97)
				return expr;

			skip(1);
		}

		operation = readUint8();
	}

	if (arg_0)
		*arg_0 = 0;
	if (arg_4)
		*arg_4 = operation;

	if ((operation == 16) || (operation == 18) || (operation == 25) || (operation == 28))
		expr = "var8_";
	else if ((operation == 17) || (operation == 24) || (operation == 27))
		expr = "var16_";
	else if ((operation == 23) || (operation == 26))
		expr = "var32_";

	expr += pref;

	switch (operation) {
	case 23:
	case 24:
	case 25:
		temp = readUint16() * 4;
		expr += printStr("%d", temp);
		if ((operation == 25) && (peekUint8() == 13)) {
			skip(1);
			expr += "+{*";
			expr += readExpr(12);
		}
		break;

	case 17:
		expr += printStr("%d", readUint16() * 2);
		break;

	case 18:
		expr += printStr("%d", readUint16());
		break;

	case 16:
	case 26:
	case 27:
	case 28:
		if (operation == 16)
			expr += printStr("%d[", readUint16());
		else if (operation == 26)
			expr += printStr("%d[", readUint16() * 4);
		else if (operation == 27)
			expr += printStr("%d[", readUint16() * 2);
		else if (operation == 28)
			expr += printStr("%d[", readUint16() * 4);

		dimCount = readUint8();
		arrDesc = _ptr;
		skip(dimCount);
		for (dim = 0; dim < dimCount; dim++) {
			expr += readExpr(12);
			expr += printStr(" of %d", (int16) arrDesc[dim]);
			if (dim != dimCount - 1)
				expr += "][";
		}
		expr += "]";

		if ((operation == 28) && (peekUint8() == 13)) {
			skip(1);
			expr += "+{*";
			expr += readExpr(12);
		}
		break;

	default:
		expr += "var_0";
		break;
	}

	return expr;
}

uint16 Script::getBlockSize() const {
	return READ_LE_UINT16(_ptr + 2) + 2;
}

void Script::evaluateParams(const Param *params) {
	bool first = true;

	while (*params != PARAM_NONE) {
		if (!first)
			print(", ");
		else
			first = false;

		switch (*params++) {
		case PARAM_UINT8:
			print("%u", readUint8());
			break;

		case PARAM_UINT16:
			print("%u", readUint16());
			break;

		case PARAM_UINT32:
			print("%u", readUint32());
			break;

		case PARAM_INT8:
			print("%d", (int8) readUint8());
			break;

		case PARAM_INT16:
			print("%d", (int16) readUint16());
			break;

		case PARAM_INT32:
			print("%d", (int32) readUint32());
			break;

		case PARAM_STR:
			print("\"%s\"", readString());
			break;

		case PARAM_EXPR:
			print("%s", readExpr().c_str());
			break;

		case PARAM_VARINDEX:
			print("%s", readVarIndex().c_str());
			break;

		default:
			error("Unknown parameter type");
			break;
		}
	}
}

void Script::printFuncDesc(const FuncParams &fParams, const Param *params) {
	if (!fParams.desc)
		return;

	printIndent();
	putString(fParams.desc);
	putString("(");

	if (params)
		evaluateParams(params);

	putString(");\n");
}

void Script::printFuncDesc(const FuncParams &fParams) const {
	if (!fParams.desc)
		return;

	printIndent();
	putString(fParams.desc);
	putString("(");

	print("%d, %d", fParams.objIndex, fParams.extraData);

	putString(");\n");
}

void Script::startFunc(const FuncParams &fParams) const {
	printIndent();
	print("%s(", fParams.desc);
}

void Script::endFunc() const {
	print(");\n");
}

void Script::loadProperties(byte *data) {
	_start = READ_LE_UINT16(data + 0x64);
	//assert(_start >= 128) -> pure "library" files may have no defined entry point (_start = 0)

	_varsCount = READ_LE_UINT16(data + 0x2C);
	_totTextCount = READ_LE_UINT32(data + 0x30);
	_totResOffset = READ_LE_UINT32(data + 0x34);
	_totResCount = (_totResOffset == 0xFFFFFFFF) ? 0 : READ_LE_UINT16(data + _totResOffset);
	_verScript = data[0x29];
	_verIMEX = data[0x3D];
	_suffixIM = data[0x3B];
	_suffixEX = data[0x3C];
	_animDataSize = READ_LE_UINT16(data + 0x38);
	_textCenter = READ_LE_UINT16(data + 0x7E);
}

void Script::loadIDE(const byte *ideData) {
	const byte *ptr = ideData;
	char buffer[17];

	uint16 count = *(const uint16 *)ptr;
	ptr += 2;
	for (uint32 i = 0; i < count; i++) {

		// skip function type
		byte functionType = *ptr;
		++ptr;

		memcpy(buffer, ptr, 17);
		buffer[16] = '\0';
		ptr += 17;

		// skip unknown word
		ptr += 2;

		// Read offset
		uint16 offset = *(const uint16 *)ptr;
		ptr += 2;

		// skip unknown word
		ptr += 2;

		if ((functionType != 0x47) && (functionType != 0x67))
			continue;

		_funcOffsetsNames[offset] = buffer;
	}
}

void Script::funcBlock(int16 retFlag) {
	FuncParams params;
	byte cmd;
	byte cmd2;

	params.retFlag = retFlag;

	skip(1);
	params.cmdCount = readUint8();
	skip(2);

	if (params.cmdCount == 0)
		return;

	params.counter = 0;
	do {
		cmd = readUint8();
		if ((cmd >> 4) >= 12) {
			cmd2 = 16 - (cmd >> 4);
			cmd &= 0xF;
		} else
			cmd2 = 0;

		params.counter++;

		if (cmd2 == 0)
			cmd >>= 4;

		funcOpcode(cmd2, cmd, params);

	} while (params.counter != params.cmdCount);
}

void Script::addStartingOffsets() {
	for (int i = 100; i < 128; i += 2) {
		uint16 offset = READ_LE_UINT16(_totData + i);
		if ((offset >= 128) && (offset != ((uint16) -1)))
			addFuncOffset(offset);
	}
}

void Script::addFuncOffset(uint32 offset) {
	for (std::list<uint32>::iterator it = _funcOffsets.begin(); it != _funcOffsets.end(); ++it)
		if (*it == offset)
			return;

	_funcOffsets.push_back(offset);
}

void Script::deGob(int32 offset, bool isLib) {
	_funcOffsets.clear();

	if (isLib) {
		// Use functions from IDE file as entry points
		for (auto it = _funcOffsetsNames.begin(); it != _funcOffsetsNames.end(); ++it)
			addFuncOffset(it->first);
	} else {
		if (offset < 0)
			addStartingOffsets();
		else
			_funcOffsets.push_back(offset);
	}


	for (std::list<uint32>::iterator it = _funcOffsets.begin(); it != _funcOffsets.end(); ++it) {
		seek(*it);
		deGobFunction();
		print("\n");
	}
}

void Script::deGobFunction() {
	if (!_funcOffsetsNames.empty()) {
		auto it = _funcOffsetsNames.find(getPos());
		if (it != _funcOffsetsNames.end()) {
			print("--- %s ---\n", it->second.c_str());
		}
	}
	printIndent();
	print("sub_%d {\n", getPos());
	incIndent();

	funcBlock(2);

	decIndent();
	printIndent();
	print("}\n");
}
