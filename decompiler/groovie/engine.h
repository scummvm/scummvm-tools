/* ScummVM Tools
 * Copyright (C) 2010 The ScummVM project
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

#ifndef GROOVIE_ENGINE_H
#define GROOVIE_ENGINE_H

#include "decompiler/engine.h"
#include "opcodes.h"

namespace Groovie {

class GroovieEngine : public Engine {
public:
	GroovieEngine();

	void getVariants(std::vector<std::string> &variants) const;

	Disassembler *getDisassembler(InstVec &insts);

	CodeGenerator *getCodeGenerator(std::ostream &output);
	bool supportsCodeGen() const { return false; }

private:
	const GroovieOpcode *getOpcodes() const;

	static const GroovieOpcode opcodesT7G[];
	static const GroovieOpcode opcodesV2[];
};

} // End of namespace Groovie

#endif // GROOVIE_ENGINE_H
