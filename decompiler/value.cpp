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

#include "value.h"

#include <boost/format.hpp>
#include <sstream>

static int dupindex = 0;

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
	return output << "(" << _lhs << " " << _op << " " << _rhs << ")";
}

std::ostream &UnaryOpValue::print(std::ostream &output) const {
	if (_isPostfix)
		return output << "(" << _operand << ")" << _op;
	else
		return output << _op << "(" << _operand << ")";
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
