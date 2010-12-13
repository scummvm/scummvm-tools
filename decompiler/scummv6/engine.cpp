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

std::ostream &Scumm::v6::Scummv6StringValue::print(std::ostream &output) const {
	return output << _str;
}

Disassembler *Scumm::v6::Scummv6Engine::getDisassembler(InstVec &insts) {
	return new Scummv6Disassembler(insts);
}

uint32 Scumm::v6::Scummv6Engine::getDestAddress(const InstPtr inst) const {
	switch(inst->_type) {
	case kJumpRelInstType:
	case kCondJumpRelInstType:
		return inst->_params[0]->getUnsigned();
	default:
		return 0;
	}
}

CodeGenerator *Scumm::v6::Scummv6Engine::getCodeGenerator(std::ostream &output) {
	return new Scummv6CodeGenerator(this, output);
}
