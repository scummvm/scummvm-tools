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

#include "control_flow.h"

#include <cstdio>

ControlFlow::ControlFlow(std::vector<Instruction> &insts, Engine *engine) {
	_engine = engine;
	GraphVertex last;
	bool addEdge = false;

	std::map<uint32, GraphVertex> addrMap;
	for (InstIterator it = insts.begin(); it != insts.end(); ++it) {
		GraphVertex cur = boost::add_vertex(_g);
		addrMap[it->_address] = cur;
		boost::put(boost::vertex_name, _g, cur, Group(it, it));

		if (addEdge)
			boost::add_edge(last, cur, _g);
		last = cur;
		addEdge = (it->_type != kJump && it->_type != kJumpRel);
	}

	for (InstIterator it = insts.begin(); it != insts.end(); ++it) {
		switch(it->_type) {
		case kJump:
		case kCondJump:
		case kJumpRel:
		case kCondJumpRel:
			boost::add_edge(addrMap[it->_address], addrMap[_engine->getDestAddress(it)], _g);
			break;
		default:
			break;
		}
	}
}

void ControlFlow::createGroups() {
}

const Graph &ControlFlow::analyze() {
	return _g;
}
