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

#ifndef KYRA_ENGINE_H
#define KYRA_ENGINE_H

#include "decompiler/engine.h"

#include <string>
#include <vector>

namespace Kyra {

/**
 * KYRA engine.
 */
class Kyra2Engine : public ::Engine {
public:
	::Disassembler *getDisassembler(std::vector<Instruction> &insts);
	uint32 getDestAddress(ConstInstIterator it) const;
	::CodeGenerator *getCodeGenerator(std::ostream &output);
	void postCFG(std::vector<Instruction> &insts, Graph g);
	bool detectMoreFuncs() const;

	std::vector<std::string> _textStrings; ///< Container for strings from the TEXT chunk.
};

} // End of namespace Kyra

#endif
