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

GroovieDisassembler::GroovieDisassembler(InstVec &insts, const GroovieOpcode *opcodes) :
	Disassembler(insts), _opcodes(opcodes) {
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
	if (opcode > 0x59) // TODO: make it depend on the real number of opcodes
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
		inst->_params.push_back(readParameter(inst, *typeString));
		typeString++;
	}
}

ValuePtr GroovieDisassembler::readParameter(InstPtr inst, char type) {
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
			retval = readParameter(inst, '1');
		else
			retval = readParameter(inst, '2');
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
		result = new ArrayValue("M", idxs);
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
			} else if (data == 0x23) {
				// Indexing an unidimensional array
				data = _f.readByte();
				_address++;
				idxs.push_back(new IntValue(data - 0x61, false));
			}
			// TODO BinaryOpValue: M[...] + 0x30
			values.push_back(new ArrayValue("M", idxs));
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

} // End of namespace Groovie
