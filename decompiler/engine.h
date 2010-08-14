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

#ifndef ENGINE_H
#define ENGINE_H

#include "disassembler.h"
#include "codegen.h"

#include <set>

/**
 * Structure representing a function.
 */
struct Function {
public:
	ConstInstIterator _startIt; ///< Iterator to of the first instruction in the function, if available.
	ConstInstIterator _endIt;   ///< Iterator to the instruction immediately after the function, similar to end() on STL containers. If _endIt == _startIt, the function endpoint is assumed to be unknown.
	std::string _name;          ///< Function name.
	GraphVertex _v;             ///< Graph vertex for the entry point to the function.
	uint32 _args;               ///< Number of arguments to the function.
	bool _retVal;               ///< Whether or not the function returns a value.
	std::string _metadata;      ///< Metadata for code generation.

	/**
	 * Parameterless constructor for Function. Required for use with STL, should not be called manually.
	 */
	Function() {
	}

	/**
	 * Constructor for Function.
	 *

	 * @param startIt Index of the first instruction in the function.
	 * @param endIt Index of the instruction immediately after the function, similar to end() on STL containers.
	 */
	Function(ConstInstIterator startIt, ConstInstIterator endIt) : _startIt(startIt), _endIt(endIt) {
	}
};

/**
 * Type representing a map of functions, indexed by starting address.
 */
typedef std::map<uint32, Function> FuncMap;

/**
 * Base class for engines.
 */
class Engine {
public:
	virtual ~Engine() {}

	/**
	 * Retrieve the disassembler for the engine.
	 *
	 * @param insts Reference to the std::vector to place the Instructions in.
	 * @return Pointer to a Disassembler for the engine.
	 */
	virtual Disassembler *getDisassembler(std::vector<Instruction> &insts) = 0;

	/**
	 * Decode a jump instruction to get the destination address.
	 *
	 * @param inst Instruction to decode.
	 * @return The destination address of the jump instruction.
	 */
	virtual uint32 getDestAddress(const Instruction &inst) const = 0;

	/**
	 * Decode a jump instruction to get the destination address.
	 *
	 * @param it Iterator pointing to the instruction to decode.
	 * @return The destination address of the jump instruction.
	 */
	uint32 getDestAddress(ConstInstIterator it) const;

	/**
	 * Retrieve the code generator for the engine.
	 *
	 * @param output The std::ostream to output the code to.
	 * @return Pointer to a CodeGenerator for the engine.
	 */
	virtual CodeGenerator *getCodeGenerator(std::ostream &output) = 0;

	/**
	 * Post-processing step after CFG analysis.
	 * @param insts Reference to the std::vector to place the Instructions in.
	 * @param g Graph generated from the CFG analysis.
	 */
	virtual void postCFG(std::vector<Instruction> &insts, Graph g) { }

	/**
	 * Whether or not code flow analysis is supported for this engine.
	 *
	 * @return True if supported, false if not. If false is returned, code flow analysis should not take place, and -D should be implied.
	 */
	virtual bool supportsCodeFlow() const { return true; }

	/**
	 * Whether or not code generation is supported for this engine.
	 *
	 * @return True if supported, false if not. If false is returned, code generation should not take place, and -G should be implied.
	 */
	virtual bool supportsCodeGen() const { return true; }

	/**
	 * Whether or not additional functions should be looked for during CFG analysis.
	 * Code that was normally unreachable will be treated as starting a new function.
	 * Note: You will need a post-processing step to add the necessary metadata to the functions.
	 *
	 * @return True if yes, false if no.
	 */
	virtual bool detectMoreFuncs() const { return false; }

	FuncMap _functions; ///< Map to functions in the current script, indexed by starting address.

	bool _isTalkie; ///< Whether or not the script is from a talkie version of the game.
};

#endif
