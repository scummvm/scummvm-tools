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

#ifndef SCUMM_V6_CODEGEN_H
#define SCUMM_V6_CODEGEN_H

#include "../codegen.h"

namespace Scumm {

namespace v6 {

/**
 * SCUMM v6 list value.
 */
class ListValue : public Value {
protected:
	const ValueList _items; ///< The list items.

public:
	/**
	 * Constructor for ListValue.
	 *
	 * @param items The list items, stored left-to-right.
	 */
	ListValue(ValueList items) : _items(items) { }

	virtual std::ostream &print(std::ostream &output) const;
};

/**
 * SCUMMv6 code generator.
 */
class Scummv6CodeGenerator : public CodeGenerator {
public:
	/**
	 * Constructor for Scumm::v6::CodeGenerator.
	 *
	 * @param engine Pointer to the Engine used for the script.
	 * @param output The std::ostream to output the code to.
	 */
	Scummv6CodeGenerator(Engine *engine, std::ostream &output) : CodeGenerator(engine, output, kFIFOArgOrder, kFIFOArgOrder) {}

	/**
	 * Get the name associated with a variable ID.
	 *
	 * @param varID The ID to get the name for.
	 * @return Pointer to char array containing the name of the variable, or NULL if no name exists.
	 */
	const char *getVarName(uint16 varID);

	/**
	 * Decode a variable ID to a name.
	 *
	 * @param varID The ID to decode.
	 * @return The decoded variable name.
	 */
	std::string decodeVarName(uint16 varID);

	/**
	 * Decode an array ID to a name.
	 *
	 * @param arrID The ID to decode.
	 * @return The decoded array name.
	 */
	std::string decodeArrayName(uint16 arrID);

	/**
	 * Creates a ListValue from the stack.
	 *
	 * @return The ListValue created from the stack.
	 */
	ValuePtr createListValue();

	virtual void processSpecialMetadata(const InstPtr inst, char c, int pos);
};

} // End of namespace v6

} // End of namespace Scumm

#endif
