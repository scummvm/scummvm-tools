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

#include "disassembler.h"
#include "opcodes.h"

#include "common/util.h"
#include "common/endian.h"

namespace Groovie {

/**
 * Value representing an array of values.
 */
class ValuesArrayValue : public Value {
protected:
	const ValueList _values;

public:
	/**
	 * Constructor for ValuesArrayValue.
	 *
	 * @param values std::deque of values (left-to-right).
	 */
	ValuesArrayValue(ValueList values) : _values(values) { }
	//TODO: implement dup()?

	std::ostream &print(std::ostream &output) const;
};

std::ostream &ValuesArrayValue::print(std::ostream &output) const {
	output << "[";
	for (ValueList::const_iterator i = _values.begin(); i != _values.end(); ++i) {
		if (i != _values.begin())
			output << ", ";
		output << *i;
	}
	output << "]";
	return output;
}

class GroovieJumpInstruction : public Instruction {
public:
	bool isJump() const { return true; }
	uint32 getDestAddress() const;
	void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {}
};

uint32 GroovieJumpInstruction::getDestAddress() const {
	std::vector<ValuePtr>::const_iterator i = _params.begin();
	while (i != _params.end()) {
		if ((*i)->isAddress())
			return (*i)->getUnsigned();
		++i;
	}
	return 0;
}

class GroovieCondJumpInstruction : public GroovieJumpInstruction {
public:
	bool isCondJump() const { return true; }
};

class GroovieUncondJumpInstruction : public GroovieJumpInstruction {
public:
	bool isUncondJump() const { return true; }
};


// GroovieDisassembler

GroovieDisassembler::GroovieDisassembler(InstVec &insts, const std::vector<GroovieOpcode> &opcodes) :
	Reassembler(insts), _opcodes(opcodes), _firstBit(false) {
}

InstPtr GroovieDisassembler::readInstruction() {
	// Read the next opcode
	byte opcode;
	try {
		opcode = _f.readByte();
	} catch (Common::FileException e) {
		return NULL;
	}
	if (_f.eos())
		return NULL;

	return createInstruction(opcode);
}

InstPtr GroovieDisassembler::createInstruction(byte opcode) {
	// Extract the first bit
	_firstBit = opcode & 0x80;
	opcode &= 0x7F;

	// Verify it's a valid opcode
	if (opcode >= _opcodes.size())
		throw UnknownOpcodeException(_address, opcode);

	// Create the new instruction
	InstPtr i;
	switch (_opcodes[opcode].type) {
	case kUnaryOpPreInst:
		i = new UnaryOpPrefixStackInstruction();
		break;
	case kBinaryOpInst:
		i = new BinaryOpStackInstruction();
		break;
	case kKernelCallInst:
		i = new KernelCallStackInstruction();
		break;
	case kReturnInst:
		i = new ReturnInstruction();
		break;
	case kJumpInst:
		i = new GroovieUncondJumpInstruction();
		break;
	case kCondJumpInst:
		i = new GroovieCondJumpInstruction();
		break;
	case kCallInst:
		// TODO: mark beginning of functions?
		i = new KernelCallStackInstruction();
		break;
	}
	if (!i)
		return NULL;

	// Fill the instruction information
	i->_opcode = opcode;
	i->_address = _address++;
	i->_stackChange = 0;
	i->_name = _opcodes[opcode].name;

	// Handle special cases
	switch (opcode) {
	case 0x0B: // Input Loop Start
		// save its address so the Input Loop End can jump here
		_inputLoopStart = i->_address;
		break;
	case 0x13: // Input Loop End
		// Jump to the address of the Input Loop Start
		i->_params.push_back(new AddressValue(_inputLoopStart));
		break;
	}

	// Read the current instruction's parameters
	readParams(i, _opcodes[opcode].params);
	return i;
}

void GroovieDisassembler::readParams(InstPtr inst, const char *typeString) {
	while (*typeString) {
		inst->_params.push_back(readParameter(*typeString));
		typeString++;
	}
}

ValuePtr GroovieDisassembler::readParameter(char type) {
	ValuePtr retval = NULL;
	switch (type) {
	case '1': // 8 bits
		retval = new IntValue(_f.readByte(), false);
		_address++;
		break;
	case '2': // 16 bits
		retval = new IntValue(_f.readUint16LE(), false);
		_address += 2;
		break;
	case '3': // 8 or 16 bits
		if (_firstBit)
			retval = readParameter('1');
		else
			retval = readParameter('2');
		break;
	case '4': // 32 bits
		retval = new IntValue(_f.readUint32LE(), false);
		_address += 4;
		break;
	case '@': // Address
		retval = new AddressValue(_f.readUint16LE());
		_address += 2;
		if (retval->getUnsigned() > _maxTargetAddress)
			_maxTargetAddress = retval->getUnsigned();
		break;
	case 'A': // Array
		retval = readParameterArray();
		break;
	case 'S': // Script name
		retval = readParameterScriptName();
		break;
	case 'V': // Video name
		retval = readParameterVideoName();
		break;
	case 'C': // Indexed value
		retval = readParameterIndexed(false, true, true);
		break;
	default:
		std::cout << "  UNKNOWN param type: " << type << std::endl;
	}
	return retval;
}

ValuePtr GroovieDisassembler::readParameterIndexed(bool allow7C, bool limitVal, bool limitVar) {
	ValuePtr result;
	uint8 data = _f.readByte();
	_address++;

	if (limitVal) {
		_firstBit = data & 0x80;
		data &= 0x7F;
	}

	if (allow7C && (data == 0x7C)) {
		// Index a bidimensional array:
		// M[part1, part2]
		// M[0x19 + 10 * part1 + part2]

		ValuePtr part1 = readParameterIndexed(false, false, false);
		ValuePtr part2 = readParameterIndexed(false, true, true);

		ValueList idxs;
		idxs.push_back(part1);
		idxs.push_back(part2);
		result = new ArrayValue("M", idxs);
	} else if (data == 0x23) {
		// Index an array:
		// M[data - 0x61]

		data = _f.readByte();
		_address++;
		if (limitVar) {
			_firstBit = data & 0x80;
			data &= 0x7F;
		}

		ValueList idxs;
		idxs.push_back(new IntValue(data - 0x61, false));
		result = new ArrayValue("m", idxs);// TODO: can probably make writeParameterIndex smart enough to not need the lowercase m here
	} else {
		// Immediate value
		result = new IntValue(data - 0x30, true);
	}
	return result;
}

ValuePtr GroovieDisassembler::readParameterArray() {
	ValueList values;
	do {
		values.push_back(readParameterIndexed(true, true, true));
	} while (!_firstBit);

	return new ValuesArrayValue(values);
}

ValuePtr GroovieDisassembler::readParameterScriptName() {
	std::string result = "";
	byte c;
	_address++;
	while ((c = _f.readByte())) {
		result += (char)c;
		_address++;
	}

	// Return the read string
	return new StringValue(result);
}

ValuePtr GroovieDisassembler::readParameterVideoName() {
	ValueList values;

	std::string stringLiteral = "";
	byte data;
	_address++;
	while ((data = _f.readByte())) {
		if (data == 0x7C || data == 0x23) {
			// Check if it breaks a previous string literal
			if (!stringLiteral.empty()) {
				values.push_back(new StringValue(stringLiteral));
				stringLiteral = "";
			}

			ValueList idxs;
			if (data == 0x7C) {
				// Indexing a bidimensional array
				idxs.push_back(readParameterIndexed(false, false, false));
				idxs.push_back(readParameterIndexed(false, false, false));
				values.push_back(new ArrayValue("M", idxs));
			} else if (data == 0x23) {
				// Indexing an unidimensional array
				data = _f.readByte();
				_address++;
				idxs.push_back(new IntValue(data - 0x61, false));
				values.push_back(new ArrayValue("m", idxs));// TODO: can probably make writeParameterVideoName smart enough to not need the lowercase m here
			}
			// TODO BinaryOpValue: M[...] + 0x30
			//values.push_back(new ArrayValue("M", idxs));
		} else {
			// To lowercase?
			if (data >= 0x41 && data <= 0x5A) {
				data += 0x20;
			}
			// Append the current character at the end of the string
			stringLiteral += (char)data;
		}
		_address++;
	}

	// Check if there's a spare string literal
	if (!stringLiteral.empty())
		values.push_back(new StringValue(stringLiteral));

	// Return the array built of string parts
	return new ValuesArrayValue(values);
}

void GroovieDisassembler::doDisassemble() throw (UnknownOpcodeException) {
	// Prepare to read the input script
	_f.seek(0, SEEK_SET);

	// Reset the status
	_addressBase = 0;
	_address = _addressBase;
	_maxTargetAddress = 0;
	_inputLoopStart = 0;

	bool cont = true;
	while (cont) {
		InstPtr i = readInstruction();

		if (i)
			_insts.push_back(i);
		else
			cont = false;
	}

	// HACK: insert additional NOPs, since some jumps target beyond
	// the end of the script
	while (_maxTargetAddress >= _address)
		_insts.push_back(createInstruction(0));
}

void GroovieDisassembler::doAssembly(const std::string &label, std::string &line, const std::string &comment) throw(std::exception) {
	// find the longest matching instruction name, since we don't have a separator beteen the instruction name and the first argument
	GroovieOpcode inst;
	size_t instLen = 0;
	for(auto &i : _opcodes) {
		size_t len = strnlen(i.name, 1024);
		if(len > instLen) {
			if(line.substr(0, len) == i.name) {
				inst = i;
				instLen = len;
			}
		}
	}

	std::string arguments;
	if(line.length() > instLen)
		arguments = line.substr(instLen + 1);

	// build list of labels, parse arguments, and write bytes to _binary
	std::vector<byte> bytes;
	size_t jumpAddrStart;// where the address is stored so it can be overwritten when we have the full list of labels
	std::string jumpToLabel;

	_firstBit = false;
	jumpAddrStart = writeParams(bytes, inst.params, arguments, jumpToLabel);
	// use writeParams to guess _firstBit, then we write the opcode at the end
	bytes.insert(bytes.begin(), inst.opcode | (_firstBit<<7));
	jumpAddrStart++; // increment since we pushed a byte to the front

	addInstruction(bytes, inst.type, jumpAddrStart, 2, label, jumpToLabel);
}

size_t GroovieDisassembler::writeParams(std::vector<byte> &bytes, const char *typeString, const std::string &arguments, std::string &jumpToLabel) {
	size_t jumpAddrStart = 0;
	size_t argStart = 0;
	while (*typeString) {
		argStart = writeParameter(*typeString, bytes, arguments, argStart, jumpAddrStart, jumpToLabel);
		typeString++;
	}
	return jumpAddrStart;
}

size_t GroovieDisassembler::writeParameter(char type, std::vector<byte> &bytes, const std::string &arguments, size_t argStart, size_t &jumpAddrStart, std::string &jumpToLabel) {
	const size_t argEnd = getEndArgument(arguments, argStart);
	const size_t argLen = argEnd - argStart;
	std::string arg = arguments.substr(argStart, argLen);
	int i;
	uint16 i16;
	uint32 u32;

	switch (type) {
	case '1': // 8 bits
		i = std::stoi(arg);
		bytes.push_back(i);
		break;
	case '2': // 16 bits
		i16 = std::stoi(arg);
		i16 = TO_LE_16(i16);
		bytes.push_back(i16);
		bytes.push_back(i16 >> 8);
		break;
	case '3': // 8 or 16 bits
		i = std::stoi(arg);
		if(i <= 0xFF || _firstBit) {
			_firstBit = true;
			bytes.push_back(i);
		} else {
			_firstBit = false;
			i16 = TO_LE_16(i);
			bytes.push_back(i16);
			bytes.push_back(i16 >> 8);
		}
		break;
	case '4': // 32 bits
		u32 = std::stoul(arg);
		u32 = TO_LE_32(u32);
		bytes.push_back(u32);
		bytes.push_back(u32 >> 8);
		bytes.push_back(u32 >> 16);
		bytes.push_back(u32 >> 24);
		break;
	case '@': // Address
		jumpAddrStart = bytes.size();
		// if arg is in 0xF3DE format, convert to 0000f3de
		jumpToLabel = (boost::format("%08x") % std::stoul(arg, 0, 16)).str();
		bytes.push_back(0);
		bytes.push_back(0);
		break;
	case 'A': // Array
		// substring to remove the [ and ]
		writeParameterArray(bytes, arg.substr(1, arg.length()-2));
		break;
	case 'S': // Script name
		// ignore the quotes around it
		for(size_t j=1; j<arg.length()-1; j++)
			bytes.push_back(arg[j]);
		bytes.push_back(0);
		break;
	case 'V': // Video name
		// substring to remove the [ and ]
		writeParameterVideoName(bytes, arg.substr(1, arg.length()-2));
		break;
	case 'C': // Indexed value
		writeParameterIndexed(false, true, true, bytes, arg);
		break;
	default:
		std::cout << "writeParameter UNKNOWN param type: " << type << "\n";
		throw std::runtime_error(std::string() + "writeParameter UNKNOWN param type: " + type);
	}
	return argEnd + 2;
}

void GroovieDisassembler::splitArrayString(const std::string &arg, std::string &first, std::string &second) {
	size_t e = getEndArgument(arg, 2);
	first = arg.substr(2, e - 2);
	second = arg.substr(e + 2);
	second.pop_back();
}

void GroovieDisassembler::writeParameterVideoName(std::vector<byte> &bytes, const std::string &arg) {
	size_t s = 0, e = 0;
	while(e < arg.length()) {
		e = getEndArgument(arg, s);
		std::string a = arg.substr(s, e - s);
		switch(a[0]) {
		case '"':
			for(size_t i=1; i < a.length()-1; i++) {
				bytes.push_back(a[i]);
			}
			break;
		case 'm':
			a = a.substr(2);
			a.pop_back();
			bytes.push_back(0x23);
			bytes.push_back(std::stoul(a) + 0x61);
			break;
		case 'M':
			bytes.push_back(0x7C);
			std::string first;
			std::string second;
			splitArrayString(a, first, second);
			writeParameterIndexed(false, false, false, bytes, first);
			writeParameterIndexed(false, false, false, bytes, second);
			break;
		}
		s = e + 2;
	}

	bytes.push_back(0);
}

void GroovieDisassembler::writeParameterIndexed(bool allow7C, bool limitVal, bool limitVar, std::vector<byte> &bytes, const std::string &arg) {
	if (allow7C && arg[0] == 'M') {
		bytes.push_back(0x7C);
		std::string first;
		std::string second;
		splitArrayString(arg, first, second);
		writeParameterIndexed(false, false, false, bytes, first);
		writeParameterIndexed(false, true, true, bytes, second);
	} else if (arg[0] == 'm') {
		bytes.push_back(0x23);
		std::string a = arg.substr(2);
		a.pop_back();
		uint8 data = std::stoul(a);
		bytes.push_back(data + 0x61);
	} else {
		// Immediate value
		uint8 data = std::stoul(arg);
		bytes.push_back(data + 0x30);
	}
}

void GroovieDisassembler::writeParameterArray(std::vector<byte> &bytes, const std::string &arg) {
	size_t s = 0, e = 0;
	while(e < arg.length()) {
		e = getEndArgument(arg, s);
		std::string a = arg.substr(s, e - s);
		writeParameterIndexed(true, true, true, bytes, a);
		s = e + 2;
	}
	// terminate the array by using the first bit
	bytes[bytes.size() - 1] |= 0x80;
}
} // End of namespace Groovie
