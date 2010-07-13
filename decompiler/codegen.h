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

#include <ostream>
#include <stack>

#ifndef DEC_CODEGEN_H
#define DEC_CODEGEN_H

class Engine;

typedef int StackEntryType;

const StackEntryType seInt = 0;
const StackEntryType seVar = 1;
const StackEntryType seBinOp = 2;
const StackEntryType seUnaryOp = 3;
const StackEntryType seDup = 4;

/**
 * Base class for stack entries.
 */
class StackEntry {
public:
	StackEntryType _type;

	virtual ~StackEntry() {}

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
	virtual StackEntry *dup(std::ostream &output);
	
	/**
	 * Output a stack entry to an std::ostream.
	 *
	 * @param output The std::ostream to output to.
	 * @param entry  The StackEntry to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, StackEntry *entry) {
		return entry->print(output);
	}
};

/**
 * Stack entry containing an integer.
 */
class IntEntry : public StackEntry {
private:
	int32 _val;     ///< The value of the integer.
	bool _isSigned; ///< True if the value is signed, false if it's not.

public:
	/**
	 * Constructor for IntEntry.
	 *
	 * @param val The value contained in the stack entry.
	 * @param isSigned Whether or not the value is signed. This will affect output.
	 */
	IntEntry(int32 val, bool isSigned) : _val(val), _isSigned(isSigned) {
		_type = seInt;
	}

	/**
	 * Constructor for IntEntry.
	 *
	 * @param val The value contained in the stack entry.
	 * @param isSigned Whether or not the value is signed. This will affect output.
	 */
	IntEntry(uint32 val, bool isSigned) : _isSigned(isSigned) {
		_val = (int32)val;
		_type = seInt;
	}	

	virtual std::ostream &print(std::ostream &output) const {
		if (_isSigned)
			output << _val;
		else
			output << (uint32)_val;
		return output;
	}

	virtual StackEntry *dup() { return new IntEntry(_val, _isSigned); }
};

/**
 * Stack entry containing a variable.
 */
class VarEntry : public StackEntry {
private:
	std::string _varName; ///< The name of the variable.

public:
	/**
	 * Constructor for VarEntry.
	 *
	 * @param varName The name of the variable.
	 */
	VarEntry(std::string varName) : _varName(varName) {
		_type = seVar;
	};

	virtual std::ostream &print(std::ostream &output) const { return output << _varName; }
};

/**
 * Stack entry containing a binary operation performed on two stack entries.
 */
class BinaryOpEntry : public StackEntry {
private:
	StackEntry *_lhs; ///< Stack entry representing the left side of the operator.
	StackEntry *_rhs; ///< Stack entry representing the right side of the operator.
	std::string _op;  ///< The operator for this entry.

public:
	/** 
	 * Constructor for BinaryOpEntry.
	 *
	 * @param lhs Stack entry representing the left side of the operator.
	 * @param rhs Stack entry representing the right side of the operator.
	 * @param op The operator for this entry.
	 */
	BinaryOpEntry(StackEntry *lhs, StackEntry *rhs, std::string op) : _lhs(lhs), _rhs(rhs), _op(op) {
		_type = seBinOp;
	}

	virtual std::ostream &print(std::ostream &output) const { return output << _lhs << " " << _op << " " << _rhs; }
};

/**
 * Stack entry containing a unary operation performed on a single stack entry.
 */
class UnaryOpEntry : public StackEntry {
private:
	StackEntry *_operand; ///< The operand the operation is performed on.
	std::string _op;      ///< The operator for this entry.

public:
	/**
	 * Constructor for UnaryOpEntry.
	 *
	 * @param operand Stack entry representing the operand of the operation.
	 * @param op The operator for this entry.
	 */
	UnaryOpEntry(StackEntry *operand, std::string op) : _operand(operand), _op(op) {
		_type = seUnaryOp;
	}

	virtual std::ostream &print(std::ostream &output) const { return output << _op << _operand; }
};

/**
 * Duplicated stack entry.
 */
class DupEntry : public StackEntry {
private:
	int _idx; ///< Index to distinguish multiple duplicated entries.

public:
	/**
	 * Constructor for DupEntry.
	 *
	 * @param idx Index to distinguish multiple duplicated entries.
	 */
	DupEntry(int idx) : _idx(idx) {
		_type = seDup;
	}
	virtual std::ostream &print(std::ostream &output) const { return output << "dup[" << _idx << "]"; }
};

/**
 * Base class for code generators.
 */
class CodeGenerator {
private:
	Engine *_engine; ///< Pointer to the Engine used for the script.
	Graph _g;        ///< The annotated graph of the script.

	/**
	 * Processes a GraphVertex.
	 *
	 * @param v The vertex to process.
	 */
	void process(GraphVertex v);

protected:
	std::ostream &_output; ///< The std::ostream to output the code to.

	/**
	 * Processes an instruction.
	 *
	 * @param inst The instruction to process.
	 */
	virtual void processInst(Instruction inst) = 0;

public:
	virtual ~CodeGenerator() {};

	/**
	 * Constructor for CodeGenerator.
	 *
	 * @param engine Pointer to the Engine used for the script.
 	 * @param output The std::ostream to output the code to.
	 */
	CodeGenerator(Engine *engine, std::ostream &output);

	/**
	 * Generates code from the provided graph and outputs it to stdout.
	 *
 	 * @param g The annotated graph of the script.
	 */
	void generate(const Graph &g);
};

#endif
