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
#include "value.h"

#include <ostream>
#include <utility>

#include <boost/intrusive_ptr.hpp>

#ifndef DEC_CODEGEN_H
#define DEC_CODEGEN_H

class Engine;

class Function;

/**
 * Type representing a stack.
 */
typedef Stack<ValuePtr> ValueStack;

const int kIndentAmount = 2; ///< How many spaces to use for each indent.

/**
 * Enumeration for the different argument/operand orderings.
 */
enum ArgOrder {
	kFIFOArgOrder, ///< First argument is pushed to stack first.
	kLIFOArgOrder  ///< First argument is pushed to stack last.
};

/**
 * Base class for code generators.
 */
class CodeGenerator {
private:
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
	Engine *_engine;        ///< Pointer to the Engine used for the script.
	std::ostream &_output;  ///< The std::ostream to output the code to.
	ValueStack _stack;      ///< The stack currently being processed.
	uint _indentLevel;      ///< Indentation level.
	GraphVertex _curVertex; ///< Graph vertex currently being processed.
	GroupPtr _curGroup;     ///< Pointer to the group currently being processed.
	ValueList _argList;     ///< Storage for lists of arguments to be built when processing function calls.

	/**
	 * Processes an instruction. Called by process() for each instruction.
	 * Call the base class implementation for opcodes you cannot handle yourself,
	 * or where the base class implementation is preferable.
	 *
	 * @param inst The instruction to process.
	 */
	virtual void processInst(const InstPtr inst);

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
	void writeAssignment(ValuePtr dst, ValuePtr src);

	/**
	 * Process a single character of metadata.
	 *
	 * @param inst The instruction being processed.
	 * @param c The character signifying the action to be taken.
	 * @param pos The position at which c occurred in the metadata.
	 */
	virtual void processSpecialMetadata(const InstPtr inst, char c, int pos);

	/**
	 * Add an argument to the argument list.
	 *
	 * @param p The argument to add.
	 */
	void addArg(ValuePtr p);

	/**
	 * Construct the signature for a function.
	 *
	 * @param func Reference to the function to construct the signature for.
	 */
	virtual std::string constructFuncSignature(const Function &func);

public:
	virtual ~CodeGenerator() { }

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
