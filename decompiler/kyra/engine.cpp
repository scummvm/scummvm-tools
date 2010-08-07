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

#include "engine.h"
#include "disassembler.h"
#include "codegen.h"

#include <iostream>
#include <sstream>
#include <boost/format.hpp>

::Disassembler *Kyra::Engine::getDisassembler(std::vector<Instruction> &insts) {
	return new Disassembler(this, insts);
}

uint32 Kyra::Engine::getDestAddress(ConstInstIterator it) const {
	return it->_params[0].getUnsigned();
}

::CodeGenerator *Kyra::Engine::getCodeGenerator(std::ostream &output) {
	return new CodeGenerator(this, output);
}

void Kyra::Engine::postCFG(std::vector<Instruction> &insts, Graph g) {
	// Add metadata to functions
	for (FuncMap::iterator it = _functions.begin(); it != _functions.end(); ++it) {
		std::stringstream s;
		s << it->second._name << boost::format("sub0x%X") % it->second._startIt->_address;
		it->second._name = s.str();
		int maxArg = 0;
		for (ConstInstIterator instIt = it->second._startIt; instIt != it->second._endIt; ++instIt) {
			if (instIt->_name.compare("pushBPAdd") == 0) {
				if (maxArg < instIt->_params[0].getSigned()) {
					maxArg = instIt->_params[0].getSigned();
				}
			}
		}
		it->second._args = maxArg;
		it->second._retVal = true;
		std::stringstream md;
		md << "0" << std::string(maxArg, 'p');
		it->second._metadata = md.str();
	}
}

bool Kyra::Engine::detectMoreFuncs() {
	return true;
}
