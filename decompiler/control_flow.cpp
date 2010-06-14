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

#include <iostream>
#include <stack>
#include <boost/format.hpp>

#define PUT(vertex, group) boost::put(boost::vertex_name, _g, vertex, group);
#define PUT_ID(vertex, id) boost::put(boost::vertex_index, _g, vertex, id);
#define GET(vertex) (boost::get(boost::vertex_name, _g, vertex))

ControlFlow::ControlFlow(std::vector<Instruction> &insts, Engine *engine) : _insts(insts) {
	_engine = engine;
	GraphVertex last;
	bool addEdge = false;
	int id = 0;

	for (InstIterator it = insts.begin(); it != insts.end(); ++it) {
		GraphVertex cur = boost::add_vertex(_g);
		_addrMap[it->_address] = cur;
		PUT(cur, Group(it, it));
		PUT_ID(cur, id);
		id++;

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
			boost::add_edge(find(it), find(_engine->getDestAddress(it)), _g);
			break;
		default:
			break;
		}
	}
}

GraphVertex ControlFlow::find(Instruction inst) {
	return _addrMap[inst._address];
}

GraphVertex ControlFlow::find(InstIterator it) {
	return _addrMap[it->_address];
}

GraphVertex ControlFlow::find(uint32 address) {
	std::map<uint32, GraphVertex>::iterator it = _addrMap.find(address);
	if (it == _addrMap.end())
		std::cerr << "Request for instruction at unknown address" << boost::format("0x%08x") % address;
	return it->second;
}

void ControlFlow::merge(GraphVertex g1, GraphVertex g2) {
	//Update property
	Group gr1 = GET(g1);
	Group gr2 = GET(g2);
	gr1._end = gr2._end;
	PUT(g1, gr1);

	//Update address map
	InstIterator it = gr2._start;
	do {
		_addrMap[it->_address] = g1;
		++it;
	} while (gr2._start != gr2._end && it != gr2._end);

	//Add outgoing edges from g2
	EdgeRange r = boost::out_edges(g2, _g);
	for (OutEdgeIterator e = r.first; e != r.second; e++) {
		boost::add_edge(g1, boost::target(*e, _g), _g);
	}
	
	//Remove edges to/from g2
	boost::clear_vertex(g2, _g);
	//Remove vertex	
	boost::remove_vertex(g2, _g);
}

void ControlFlow::createGroups() {
	InstIterator curInst, nextInst;
	nextInst = _insts.begin();
	nextInst++;
	int stackLevel = 0;
	int expectedStackLevel = 0;
	std::stack<uint32> s;
	for (curInst = _insts.begin(); nextInst != _insts.end(); curInst++, nextInst++) {
		GraphVertex cur = find(curInst);
		GraphVertex next = find(nextInst);

		if (in_degree(cur, _g) == 0 && out_degree(cur, _g) == 0)
			continue;

		//Check if we go below our expected stack level
		if (stackLevel == expectedStackLevel && curInst->_stackChange < 0) {
			expectedStackLevel = s.top();
			s.pop();
		}

		stackLevel += curInst->_stackChange;

		// Group ends after a jump
		if (curInst->_type == kJump || curInst->_type == kJumpRel || curInst->_type == kCondJump || curInst->_type == kCondJumpRel) {
			if (stackLevel != expectedStackLevel) {
				s.push(expectedStackLevel);
				expectedStackLevel = stackLevel;
			}
			continue;
		}

		// Group ends before target of a jump
		if (in_degree(next, _g) != 1) {
			if (stackLevel != expectedStackLevel) {
				s.push(expectedStackLevel);
				expectedStackLevel = stackLevel;
			}
			continue;
		}

		// Group ends when stack is balanced, unless just before conditional jump
		if (stackLevel == expectedStackLevel && nextInst->_type != kCondJump && nextInst->_type != kCondJumpRel) {
			continue;
		}
		merge(cur, next);
	}
}

const Graph &ControlFlow::analyze() {
	return _g;
}
