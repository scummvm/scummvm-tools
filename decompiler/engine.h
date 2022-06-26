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

#ifndef ENGINE_H
#define ENGINE_H

#include "disassembler.h"
#include "codegen.h"

#include <set>
#include <string>
#include <vector>

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
	virtual Disassembler *getDisassembler(InstVec &insts) = 0;

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
	virtual void postCFG(InstVec &insts, Graph g) { }

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

	/**
	 * Fill a vector with the names of all variants supported for this engine.
	 * If variants are not used by this engine, leave the vector empty (default implementation).
	 *
	 * @param variants Vector to add the supported variants to.
	 */
	virtual void getVariants(std::vector<std::string> &variants) const { };

	std::string _variant; ///< Engine variant to use for the script.

	/**
	 * Whether or not to use "pure" grouping during code flow analysis.
	 * With pure grouping, code flow analysis only looks at branches when merging.
	 * This method may be more appropriate for non-stack-based engines.
	 *
	 * @return True if pure grouping should be used, false if not.
	 */
	virtual bool usePureGrouping() const { return false; }
};

#endif
