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

#include "value.h"

#include <boost/format.hpp>
#include <map>
#include <sstream>
#include <string>

static int dupindex = 0;
static std::map<std::string, int> binaryOpPrecedence;
static std::map<std::string, std::string> negateMap;

void initPrecedence() {
	binaryOpPrecedence["||"] = kLogicalOrPrecedence;
	binaryOpPrecedence["&&"] = kLogicalAndPrecedence;
	binaryOpPrecedence["|"] = kBitwiseOrPrecedence;
	binaryOpPrecedence["^"] = kBitwiseXorPrecedence;
	binaryOpPrecedence["&"] = kBitwiseAndPrecedence;
	binaryOpPrecedence["=="] = kEqualityOpPrecedence;
	binaryOpPrecedence["!="] = kEqualityOpPrecedence;
	binaryOpPrecedence["<"] = kRelationOpPrecedence;
	binaryOpPrecedence["<="] = kRelationOpPrecedence;
	binaryOpPrecedence[">="] = kRelationOpPrecedence;
	binaryOpPrecedence[">"] = kRelationOpPrecedence;
	binaryOpPrecedence["<<"] = kShiftOpPrecedence;
	binaryOpPrecedence[">>"] = kShiftOpPrecedence;
	binaryOpPrecedence["+"] = kAddOpPrecedence;
	binaryOpPrecedence["-"] = kAddOpPrecedence;
	binaryOpPrecedence["*"] = kMultOpPrecedence;
	binaryOpPrecedence["/"] = kMultOpPrecedence;
	binaryOpPrecedence["%"] = kMultOpPrecedence;
}

void initNegateMap() {
	negateMap["=="] = "!=";
	negateMap["!="] = "==";
	negateMap["<"] = ">=";
	negateMap["<="] = ">";
	negateMap[">="] = "<";
	negateMap[">"] = "<=";
}

bool Value::isInteger() {
	return false;
}

bool Value::isAddress() {
	return false;
}

bool Value::isSignedValue() throw(WrongTypeException) {
	throw WrongTypeException();
}

int32 Value::getSigned() throw(WrongTypeException) {
	throw WrongTypeException();
}

uint32 Value::getUnsigned() throw(WrongTypeException) {
	throw WrongTypeException();
}

ValuePtr Value::dup(std::ostream &output) {
	ValuePtr dupValue = new DupValue(++dupindex);
	output << dupValue << " = " << this << ";";
	return dupValue;
}

ValuePtr Value::negate() throw(WrongTypeException) {
	return new NegatedValue(this);
}

std::string Value::getString() const {
	std::stringstream s;
	print(s);
	return s.str();
}

int Value::precedence() const {
	return kNoPrecedence;
}

bool IntValue::isInteger() {
	return true;
}

bool IntValue::isSignedValue() throw(WrongTypeException) {
	return _isSigned;
}

int32 IntValue::getSigned() throw(WrongTypeException) {
	return _val;
}

uint32 IntValue::getUnsigned() throw(WrongTypeException) {
	return (uint32)_val;
}

ValuePtr IntValue::dup(std::ostream &output) {
	return new IntValue(_val, _isSigned);
}

std::ostream &IntValue::print(std::ostream &output) const {
	if (_isSigned)
		output << (int32)_val;
	else
		output << (uint32)_val;
	return output;
}

bool AddressValue::isAddress() {
	return true;
}

int32 AddressValue::getSigned() throw(WrongTypeException) {
	throw WrongTypeException();
}

ValuePtr AddressValue::dup(std::ostream &output) {
	return new AddressValue(_val);
}

std::ostream &AddressValue::print(std::ostream &output) const {
	return output << boost::format("0x%X") % _val;
}

bool RelAddressValue::isAddress() {
	return true;
}

uint32 RelAddressValue::getUnsigned() throw(WrongTypeException) {
	return _baseaddr + _val;
}

ValuePtr RelAddressValue::dup(std::ostream &output) {
	return new RelAddressValue(_baseaddr, _val);
}

std::ostream &RelAddressValue::print(std::ostream &output) const {
	if (_val < 0)
		return output << boost::format("-0x%X") % -_val;
	return output << boost::format("+0x%X") % _val;
}

ValuePtr DupValue::dup(std::ostream &output) {
	return this;
}

std::ostream &DupValue::print(std::ostream &output) const {
	return output << "temp" << _idx;
}

std::ostream &StringValue::print(std::ostream &output) const {
	return output << "\"" << _str << "\"";
}

std::ostream &VarValue::print(std::ostream &output) const {
	return output << _varName;
}

std::ostream &ArrayValue::print(std::ostream &output) const {
	output << _varName;
	for (ValueList::const_iterator i = _idxs.begin(); i != _idxs.end(); ++i)
		output << "[" << *i << "]";
	return output;
}

std::ostream &BinaryOpValue::print(std::ostream &output) const {
	if (_lhs->precedence() > precedence())
		output <<  "(" << _lhs << ")";
	else
		output << _lhs;
	output << " " << _op << " ";
	if (_rhs->precedence() > precedence())
		output << "(" << _rhs << ")";
	else
		output << _rhs;
	return output;
}

int BinaryOpValue::precedence() const {
	if (binaryOpPrecedence.empty())
		initPrecedence();
	return binaryOpPrecedence[_op];
}

ValuePtr BinaryOpValue::negate() throw(WrongTypeException) {
	if (negateMap.empty())
		initNegateMap();
	if (negateMap.find(_op) == negateMap.end())
		return Value::negate();
	else
		return new BinaryOpValue(_lhs, _rhs, negateMap[_op]);
}

std::ostream &UnaryOpValue::print(std::ostream &output) const {
	if (!_isPostfix)
		output << _op;
	if (_operand->precedence() > precedence())
		output << "(" << _operand << ")";
	else
		output << _operand;
	if (_isPostfix)
		output << _op;
	return output;
}

int UnaryOpValue::precedence() const {
	return kUnaryOpPrecedence;
}

ValuePtr NegatedValue::negate() throw(WrongTypeException) {
	return _operand;
}

std::ostream &CallValue::print(std::ostream &output) const {
	output << _funcName << "(";
	for (ValueList::const_iterator i = _args.begin(); i != _args.end(); ++i) {
		if (i != _args.begin())
			output << ", ";
		output << *i;
	}
	output << ")";
	return output;
}
