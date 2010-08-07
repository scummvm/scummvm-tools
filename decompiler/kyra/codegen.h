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

#ifndef DEC_KYRA_CODEGEN_H
#define DEC_KYRA_CODEGEN_H

#include "../codegen.h"

namespace Kyra {

/**
 * KYRA code generator.
 */
class CodeGenerator : public ::CodeGenerator {
private:
	int _stackOffset; ///< Running count of where in the stack to look for the next argument.
public:
	/**
	 * Constructor for Kyra::CodeGenerator.
	 *
	 * @param engine Pointer to the Engine used for the script.
	 * @param output The std::ostream to output the code to.
	 */
	CodeGenerator(Engine *engine, std::ostream &output) : ::CodeGenerator(engine, output, kLIFO, kLIFO) {}
protected:
	void processInst(const Instruction inst);
	virtual void processSpecialMetadata(const Instruction inst, char c);
	std::string constructFuncSignature(const Function &func);
};

} // End of namespace Kyra

#endif
