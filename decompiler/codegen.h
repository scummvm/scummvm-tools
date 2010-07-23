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

#include "graph.h"
#include "stack.h"

#include <ostream>
#include <utility>

#include <boost/intrusive_ptr.hpp>

#ifndef DEC_CODEGEN_H
#define DEC_CODEGEN_H

class Engine;

typedef int StackEntryType;

const StackEntryType seInt = 0;
const StackEntryType seVar = 1;
const StackEntryType seBinOp = 2;
const StackEntryType seUnaryOp = 3;
const StackEntryType seDup = 4;
const StackEntryType seArray = 5;
const StackEntryType seString = 6;
const StackEntryType seList = 7;
const StackEntryType seCall = 8;

class StackEntry;

/**
 * Pointer to a Group.
 */
typedef boost::intrusive_ptr<StackEntry> EntryPtr;

namespace boost {
inline void intrusive_ptr_add_ref(StackEntry *p);
inline void intrusive_ptr_release(StackEntry *p);
} // End of namespace boost

/**
 * Base class for stack entries.
 */
class StackEntry {
private:
	long _refCount; ///< Reference count used for boost::intrusive_ptr.
	friend void ::boost::intrusive_ptr_add_ref(StackEntry *p); ///< Allow access by reference counting methods in boost namespace.
	friend void ::boost::intrusive_ptr_release(StackEntry *p); ///< Allow access by reference counting methods in boost namespace.

public:
	const StackEntryType _type; ///< Type of the stack entry.

	/**
	 * Constructor for StackEntry.
	 *
	 * @param type The StackEntryType of the StackEntry.
	 */
	StackEntry(StackEntryType type) : _refCount(0), _type(type) { }

	virtual ~StackEntry() { }

	/**
	 * Print the stack entry to an std::ostream.
	 *
	 * @param output The std::ostream to write to.
	 * @return The std::ostream used for output.
	 */
	virtual std::ostream &print(std::ostream &output) const = 0;

	/**
	 * Duplicates a stack entry.
	 *
	 * @param output The std::ostream to output to.
	 * @return A StackEntry corresponding to a duplicate of this entry.
	 */
	virtual EntryPtr dup(std::ostream &output);

	/**
	 * Output a stack entry to an std::ostream.
	 *
	 * @param output The std::ostream to output to.
	 * @param entry  Reference counted pointer to the StackEntry to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, EntryPtr entry) {
		return entry->print(output);
	}
};

namespace boost {

/**
 * Add a reference to a pointer to a StackEntry.
 */
inline void intrusive_ptr_add_ref(StackEntry *p) {
	++(p->_refCount);
}

/**
 * Remove a reference from a pointer to a StackEntry.
 */
inline void intrusive_ptr_release(StackEntry *p) {
	if (--(p->_refCount) == 0)
		delete p;
}

} // End of namespace boost

/**
 * Stack entry containing an integer.
 */
class IntEntry : public StackEntry {
private:
	const int32 _val;     ///< The value of the integer.
	const bool _isSigned; ///< True if the value is signed, false if it's not.

public:
	/**
	 * Constructor for IntEntry.
	 *
	 * @param val The value contained in the stack entry.
	 * @param isSigned Whether or not the value is signed. This will affect output.
	 */
	IntEntry(int32 val, bool isSigned) : StackEntry(seInt), _val(val), _isSigned(isSigned) {
	}

	/**
	 * Constructor for IntEntry.
	 *
	 * @param val The value contained in the stack entry.
	 * @param isSigned Whether or not the value is signed. This will affect output.
	 */
	IntEntry(uint32 val, bool isSigned) : StackEntry(seInt), _val(val), _isSigned(isSigned) {
	}

	virtual std::ostream &print(std::ostream &output) const {
		if (_isSigned)
			output << _val;
		else
			output << (uint32)_val;
		return output;
	}

	virtual EntryPtr dup(std::ostream &output) {
		return new IntEntry(_val, _isSigned);
	}
};

/**
 * Stack entry containing a variable.
 */
class VarEntry : public StackEntry {
private:
	const std::string _varName; ///< The name of the variable.

public:
	/**
	 * Constructor for VarEntry.
	 *
	 * @param varName The name of the variable.
	 */
	VarEntry(std::string varName) : StackEntry(seVar), _varName(varName) { }

	virtual std::ostream &print(std::ostream &output) const {
		return output << _varName;
	}
};

/**
 * Stack entry containing a binary operation performed on two stack entries.
 */
class BinaryOpEntry : public StackEntry {
private:
	const EntryPtr _lhs; ///< Stack entry representing the left side of the operator.
	const EntryPtr _rhs; ///< Stack entry representing the right side of the operator.
	const std::string _op;  ///< The operator for this entry.

public:
	/**
	 * Constructor for BinaryOpEntry.
	 *
	 * @param lhs Stack entry representing the left side of the operator.
	 * @param rhs Stack entry representing the right side of the operator.
	 * @param op The operator for this entry.
	 */
	BinaryOpEntry(EntryPtr lhs, EntryPtr rhs, std::string op) :
		StackEntry(seBinOp), _lhs(lhs), _rhs(rhs), _op(op) {
	}

	virtual std::ostream &print(std::ostream &output) const {
		return output << "(" << _lhs << " " << _op << " " << _rhs << ")";
	}
};

/**
 * Stack entry containing a unary operation performed on a single stack entry.
 */
class UnaryOpEntry : public StackEntry {
private:
	const EntryPtr _operand; ///< The operand the operation is performed on.
	const std::string _op;      ///< The operator for this entry.

public:
	/**
	 * Constructor for UnaryOpEntry.
	 *
	 * @param operand Stack entry representing the operand of the operation.
	 * @param op The operator for this entry.
	 */
	UnaryOpEntry(EntryPtr operand, std::string op) :
		StackEntry(seUnaryOp), _operand(operand), _op(op) { }

	virtual std::ostream &print(std::ostream &output) const {
		return output << _op << _operand;
	}
};

/**
 * Duplicated stack entry.
 */
class DupEntry : public StackEntry {
private:
	const int _idx; ///< Index to distinguish multiple duplicated entries.

public:
	/**
	 * Constructor for DupEntry.
	 *
	 * @param idx Index to distinguish multiple duplicated entries.
	 */
	DupEntry(int idx) : StackEntry(seDup), _idx(idx) { }

	virtual std::ostream &print(std::ostream &output) const {
		return output << "dup[" << _idx << "]";
	}
};

/**
 * Type representing index list for an array.
 */
typedef std::deque<EntryPtr> EntryList;

/**
 * Stack entry representing array access.
 */
class ArrayEntry : public StackEntry {
private:
	const std::string _arrayName; ///< The name of the array.
	const EntryList _idxs;              ///< std::deque of stack entries representing the indexes used (left-to-right).

public:
	/**
	 * Constructor for ArrayEntry.
	 *
	 * @param arrayName The name of the array.
	 * @param idxs std::deque of stack entries representing the indexes used (left-to-right).
	 */
	ArrayEntry(std::string arrayName, std::deque<EntryPtr> idxs) : StackEntry(seArray), _arrayName(arrayName), _idxs(idxs) { }

	virtual std::ostream &print(std::ostream &output) const {
		output << _arrayName;
		for (EntryList::const_iterator i = _idxs.begin(); i != _idxs.end(); ++i)
			output << "[" << *i << "]";
		return output;
	}
};

/**
 * Entry containing a string.
 */
class StringEntry : public StackEntry {
private:
	const std::string _str; ///< The string in the entry.

public:
	/**
	 * Constructor for StringEntry.
	 *
	 * @param str The string in the entry.
	 */
	StringEntry(std::string str) : StackEntry(seString), _str(str) { }

	virtual std::ostream &print(std::ostream &output) const {
		return output << _str;
	}
};

/**
 * Entry representing a list.
 */
class ListEntry : public StackEntry {
private:
	const EntryList _items; ///< Vector containing the list items.

public:
	/**
	 * Constructor for ListEntry.
	 *
	 * @param items The items stored in the list.
	 */
	ListEntry(EntryList items) : StackEntry(seList), _items(items) { }

	virtual std::ostream &print(std::ostream &output) const {
		output << "[";
		for (EntryList::const_iterator i = _items.begin(); i != _items.end(); ++i) {
			if (i != _items.begin())
				output << ", ";
			output << *i;
		}
		output << "]";
		return output;
	}
};

/**
 * Stack entry representing a function call.
 */
class CallEntry : public StackEntry {
private:
	const std::string _funcName; ///< The name of the function.
	const EntryList _args;       ///< std::deque of stack entries representing the arguments used (stored left-to-right).

public:
	/**
	 * Constructor for CallEntry.
	 *
	 * @param funcName The name of the function.
	 * @param args std::deque of stack entries representing the arguments used.
	 */
	CallEntry(std::string funcName, EntryList args) : StackEntry(seCall), _funcName(funcName), _args(args) { }

	virtual std::ostream &print(std::ostream &output) const {
		output << _funcName << "(";
		for (EntryList::const_iterator i = _args.begin(); i != _args.end(); ++i) {
			if (i != _args.begin())
				output << ", ";
			output << *i;
		}
		output << ")";
		return output;
	}
};

/**
 * Type representing a stack.
 */
typedef Stack<EntryPtr> EntryStack;

const int kIndentAmount = 2; ///< How many spaces to use for each indent.

/**
 * Enumeration for the different argument/operand orderings.
 */
enum ArgOrder {
	kFIFO, ///< First argument is pushed to stack first.
	kLIFO  ///< First argument is pushed to stack last.
};

/**
 * Base class for code generators.
 */
class CodeGenerator {
private:
	Engine *_engine;           ///< Pointer to the Engine used for the script.
	Graph _g;                  ///< The annotated graph of the script.
	const ArgOrder _binOrder;  ///< Order of operands for binary operations.
	const ArgOrder _callOrder; ///< Order of operands for call arguments.

	/**
	 * Processes a GraphVertex.
	 *
	 * @param v The vertex to process.
	 */
	void process(GraphVertex v);

protected:
	std::ostream &_output; ///< The std::ostream to output the code to.
	EntryStack _stack;     ///< The stack currently being processed.
	uint _indentLevel;     ///< Indentation level.
	GroupPtr _curGroup;    ///< Pointer to the group currently being processed.
	EntryList _argList;    ///< Storage for lists of arguments to be built when processing function calls.

	/**
	 * Processes an instruction.
	 *
	 * @param inst The instruction to process.
	 */
	virtual void processInst(const Instruction inst) = 0;

	/**
	 * Indents a string according to the current indentation level.
	 *
	 * @param s The string to indent.
	 * @result The indented string.
	 */
	std::string indentString(std::string s);

	/**
	 * Adds a line of code to the current group.
	 *
	 * @param s The line to add.
	 * @param unindentBefore Whether or not to remove an indentation level before the line. Defaults to false.
	 * @param indentAfter Whether or not to add an indentation level after the line. Defaults to false.
	 */
	void addOutputLine(std::string s, bool unindentBefore = false, bool indentAfter = false);

	/**
	 * Generate an assignment statement.
	 *
	 * @param dst The variable being assigned to.
	 * @param src The value being assigned.
	 */
	void writeAssignment(EntryPtr dst, EntryPtr src);

	/**
	 * Process a single character of metadata.
	 *
	 * @param it The instruction being processed.
	 * @param c The character signifying the action to be taken.
	 */
	virtual void processSpecialMetadata(const Instruction inst, char c);

	/**
	 * Add an argument to the argument list.
	 *
	 * @param p The argument to add.
	 */
	void addArg(EntryPtr p);

public:
	virtual ~CodeGenerator() {};

	/**
	 * Constructor for CodeGenerator.
	 *
	 * @param engine Pointer to the Engine used for the script.
	 * @param output The std::ostream to output the code to.
	 * @param binOrder Order of arguments for binary operators.
	 * @param callOrder Order of arguments for function calls.
	 */
	CodeGenerator(Engine *engine, std::ostream &output, ArgOrder binOrder, ArgOrder callOrder);

	/**
	 * Generates code from the provided graph and outputs it to stdout.
	 *
	 * @param g The annotated graph of the script.
	 */
	void generate(const Graph &g);
};

#endif
