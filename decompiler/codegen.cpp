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

#include "codegen.h"

#include <iostream>

static int dupindex = 0;

EntryPtr StackEntry::dup(std::ostream &output) {
	if (_type == seDup)
		return this;

	EntryPtr dupEntry = new DupEntry(++dupindex);
	output << this << " = " << dupEntry;
	return dupEntry;
}

CodeGenerator::CodeGenerator(Engine *engine, std::ostream &output) : _output(output) {
	_engine = engine;
}

void CodeGenerator::generate(const Graph &g) {
	_g = g;
	// TODO
}

void CodeGenerator::process(GraphVertex v) {
	// TODO
}
