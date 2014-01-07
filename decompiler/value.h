/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef VALUE_H
#define VALUE_H

#include <deque>
#include <exception>
#include <ostream>
#include <string>
#include <boost/intrusive_ptr.hpp>

#include "common/scummsys.h"
#include "refcounted.h"
#include "stack.h"
#include "wrongtype.h"

class Value;

const int kNoPrecedence = 0;          ///< Precedence value for individual values with no operations.
const int kUnaryOpPrecedence = 1;     ///< Precedence value for a unary operation. (!, -, ~, etc.)
const int kMultOpPrecedence = 2;      ///< Precedence value for multiplication, division, modulus (*, /, %)
const int kAddOpPrecedence = 3;       ///< Precedence value for addition and subtraction (+, -)
const int kShiftOpPrecedence = 4;     ///< precedence value for bit shifting (<<, >>)
const int kRelationOpPrecedence = 5;  ///< Precedence value for relative comparison (<, <=, >=, >)
const int kEqualityOpPrecedence = 6;  ///< Precedence value for equality comparisons (==, !=)
const int kBitwiseAndPrecedence = 7;  ///< Precedence value for bitwise AND (&)
const int kBitwiseXorPrecedence = 8;  ///< Precedence value for bitwise XOR (^)
const int kBitwiseOrPrecedence = 9;   ///< Precedence value for bitwise OR (|)
const int kLogicalAndPrecedence = 10; ///< Precedence value for logical AND (&&)
const int kLogicalOrPrecedence = 11;  ///< Precedence value for logical OR (||)

/**
 * Pointer to a Value.
 */
typedef boost::intrusive_ptr<Value> ValuePtr;

/**
 * Type representing a list of values, e.g. for indexes used to access an array.
 */
typedef std::deque<ValuePtr> ValueList;

/**
 * Type representing a stack.
 */
typedef Stack<ValuePtr> ValueStack;

/**
 * Class representing a value (stack entry, parameter, etc.)
 */
class Value : public RefCounted {
public:
	virtual ~Value() { }

	/**
	 * Return whether or not the Value is an integer.
	 *
	 * @return True if the Value is an integer, otherwise false.
	 */
	virtual bool isInteger();

	/**
	 * Return whether or not the Value is an address.
	 *
	 * @return True if the Value is an address, otherwise false.
	 */
	virtual bool isAddress();

	/**
	 * Returns whether or not any stored integer value is signed.
	 *
	 * @return True if the integer value is signed, false if it is not.
	 * @throws WrongTypeException if the value is not an integer.
	 */
	virtual bool isSignedValue() throw(WrongTypeException);

	/**
	 * Retrieves a signed integer representing the value, if possible.
	 *
	 * @return A signed integer representing the value, if possible.
	 * @throws WrongTypeException if the value is not an integer.
	 */
	virtual int32 getSigned() throw(WrongTypeException);

	/**
	 * Retrieves an unsigned integer representing the value, if possible.
	 *
	 * @return An unsigned integer representing the value, if possible.
	 * @throws WrongTypeException if the value is not an integer.
	 */
	virtual uint32 getUnsigned() throw(WrongTypeException);

	/**
	 * Print the value to an std::ostream.
	 *
	 * @param output The std::ostream to write to.
	 * @return The std::ostream used for output.
	 */
	virtual std::ostream &print(std::ostream &output) const = 0;

	/**
	 * Retrieves the string representation of the value.
	 *
	 * @return The string representation of the value.
	 */
	std::string getString() const;

	/**
	 * Duplicates a value.
	 *
	 * @param output The std::ostream to output any necessary assignment to.
	 * @return A Value corresponding to a duplicate of this entry.
	 */
	virtual ValuePtr dup(std::ostream &output);

	/**
	 * Negates a value.
	 *
	 * @return The current Value, only negated.
	 * @throws WrongTypeException if negation is not possible.
	 */
	virtual ValuePtr negate() throw(WrongTypeException);

	/**
	 * Operator precedence for this value.
	 * Lower values bind stronger, i.e. they are resolved earlier.
	 * In other words, if an operand has a higher precedence value than the
	 * operator, parentheses are not required for that operand.
	 */
	virtual int precedence() const;

	/**
	 * Output a value to an std::ostream.
	 *
	 * @param output The std::ostream to output to.
	 * @param value  Reference counted pointer to the value to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, Value *value) {
		return value->print(output);
	}

};

/**
 * Value containing an integer.
 */
class IntValue : public Value {
protected:
	const int32 _val;     ///< The value of the integer.
	const bool _isSigned; ///< True if the value is signed, false if it's not.

public:
	/**
	 * Constructor for IntValue.
	 *
	 * @param val The integer value to be contained.
	 * @param isSigned Whether or not the value is signed. This will affect output.
	 */
	IntValue(int32 val, bool isSigned) : _val(val), _isSigned(isSigned) { }

	/**
	 * Constructor for IntValue.
	 *
	 * @param val The integer value to be contained.
	 * @param isSigned Whether or not the value is signed. This will affect output.
	 */
	IntValue(uint32 val, bool isSigned) : _val(val), _isSigned(isSigned) { }

	bool isInteger();
	bool isSignedValue() throw(WrongTypeException);
	int32 getSigned() throw(WrongTypeException);
	uint32 getUnsigned() throw(WrongTypeException);

	ValuePtr dup(std::ostream &output);

	virtual std::ostream &print(std::ostream &output) const;
};

/**
 * Value containing an absolute address.
 */
class AddressValue : public IntValue {
public:
	/**
	 * Constructor for AddressValue.
	 *
	 * @param addr The absolute address represented by the value.
	 */
	AddressValue(uint32 addr) : IntValue(addr, false) { }

	bool isAddress();
	int32 getSigned() throw(WrongTypeException);

	ValuePtr dup(std::ostream &output);

	virtual std::ostream &print(std::ostream &output) const;
};

/**
 * Value containing a signed, relative address. When asking for unsigned integer value, exact address is returned; when printing or getting signed value, relative address is used.
 */
class RelAddressValue : public IntValue {
protected:
	const uint32 _baseaddr; ///< The base address for the offset.

public:
	/**
	 * Constructor for AddressValue.
	 *
	 * @param baseaddr The base address for the offset.
	 * @param offset The relative offset to the base address.
	 */
	RelAddressValue(uint32 baseaddr, int32 offset) : IntValue(offset, true), _baseaddr(baseaddr) { };

	bool isAddress();
	uint32 getUnsigned() throw(WrongTypeException);

	ValuePtr dup(std::ostream &output);

	virtual std::ostream &print(std::ostream &output) const;
};

/**
 * Duplicated value.
 */
class DupValue : public Value {
protected:
	const int _idx; ///< Index to distinguish multiple duplicated entries.

public:
	/**
	 * Constructor for DupEntry.
	 *
	 * @param idx Index to distinguish multiple duplicated entries.
	 */
	DupValue(int idx) : _idx(idx) { }

	ValuePtr dup(std::ostream &output);

	virtual std::ostream &print(std::ostream &output) const;
};

/**
 * String value.
 */
class StringValue : public Value {
protected:
	const std::string _str; ///< The string value.

public:
	/**
	 * Constructor for StringValue.
	 *
	 * @param str The string value.
	 */
	StringValue(std::string str) : _str(str) { }

	virtual std::ostream &print(std::ostream &output) const;
};

/**
 * Value representing a variable.
 */
class VarValue : public Value {
protected:
	const std::string _varName; ///< The variable name.

public:
	/**
	 * Constructor for VarValue.
	 *
	 * @param varName The variable name.
	 */
	VarValue(std::string varName) : _varName(varName) { }

	virtual std::ostream &print(std::ostream &output) const;
};

/**
 * Value representing array access.
 */
class ArrayValue : public VarValue {
protected:
	const ValueList _idxs; ///< std::deque of values representing the indexes used (left-to-right).

public:
	/**
	 * Constructor for ArrayValue.
	 *
	 * @param arrayName The name of the array.
	 * @param idxs std::deque of stack entries representing the indexes used (left-to-right).
	 */
	ArrayValue(std::string arrayName, ValueList idxs) : VarValue(arrayName), _idxs(idxs) { }

	virtual std::ostream &print(std::ostream &output) const;
};

/**
 * Value representing the result of a binary operation.
 */
class BinaryOpValue : public Value {
protected:
	const ValuePtr _lhs;   ///< Value representing the left side of the operator.
	const ValuePtr _rhs;   ///< Value representing the right side of the operator.
	const std::string _op; ///< The operator for this value.

public:
	/**
	 * Constructor for BinaryOpValue.
	 *
	 * @param lhs Value representing the left side of the operator.
	 * @param rhs Value representing the right side of the operator.
	 * @param op The operator for this value.
	 */
	BinaryOpValue(ValuePtr lhs, ValuePtr rhs, std::string op) : _lhs(lhs), _rhs(rhs), _op(op) { }

	virtual std::ostream &print(std::ostream &output) const;

	virtual ValuePtr negate() throw(WrongTypeException);

	virtual int precedence() const;
};

/**
 * Value representing the result of a unary operation.
 * Used as base class for prefix and postfix variants.
 */
class UnaryOpValue : public Value {
protected:
	const ValuePtr _operand; ///< Value representing the operand of the operation.
	const std::string _op;   ///< The operator for this value.
	const bool _isPostfix;   ///< Whether or not the operator should be postfixed to the operand.

public:
	/**
	 * Constructor for UnaryOpValue.
	 *
	 * @param operand Value representing the operand of the operation.
	 * @param op The operator for this value.
	 * @param isPostfix Whether or not the operator should be postfixed to the operand.
	 */
	UnaryOpValue(ValuePtr operand, std::string op, bool isPostfix) :
		_operand(operand), _op(op), _isPostfix(isPostfix) { }

	virtual std::ostream &print(std::ostream &output) const;

	virtual int precedence() const;
};

/**
 * Negated value.
 */
class NegatedValue : public UnaryOpValue {
public:
	/**
	 * Constructor for NegatedValue.
	 *
	 * @param val The value to negate.
	 */
	NegatedValue(ValuePtr val) : UnaryOpValue(val, "!", false) { }
	virtual ValuePtr negate() throw(WrongTypeException);
};

/**
 * Value representing a function call.
 */
class CallValue : public Value {
protected:
	const std::string _funcName; ///< The name of the function.
	const ValueList _args;       ///< std::deque of values representing the arguments used (stored left-to-right).

public:
	/**
	 * Constructor for CallValue.
	 *
	 * @param funcName The name of the function.
	 * @param args std::deque of values representing the arguments used.
	 */
	CallValue(std::string funcName, ValueList args) : _funcName(funcName), _args(args) { }

	virtual std::ostream &print(std::ostream &output) const;
};

#endif
