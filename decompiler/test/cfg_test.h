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

#include <cxxtest/TestSuite.h>

#include "decompiler/control_flow.h"
#include "decompiler/disassembler.h"
#include "decompiler/graph.h"
#include "decompiler/scummv6/engine.h"

#include <vector>
#define GET(vertex) (boost::get(boost::vertex_name, g, vertex))

class CFGTestSuite : public CxxTest::TestSuite {
public:
	void testUnreachable() {
		Scumm::v6::Engine *engine = new Scumm::v6::Engine();
		Disassembler *d = engine->getDisassembler();
		d->open("decompiler/test/unreachable.dmp");
		std::vector<Instruction> insts = d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		Graph g = c->getGraph();
		TS_ASSERT(boost::num_vertices(g) == 4);
		std::pair<VertexIterator, VertexIterator> range = boost::vertices(g);
		VertexIterator it = range.first;
		Group *gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 0);
		TS_ASSERT(boost::in_degree(*it, g) == 0 && boost::out_degree(*it, g) == 1);
		++it;
		gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 2);
		TS_ASSERT(boost::in_degree(*it, g) == 1 && boost::out_degree(*it, g) == 1);
		++it;
		gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 5);
		TS_ASSERT(boost::in_degree(*it, g) == 0 && boost::out_degree(*it, g) == 1);
		++it;
		gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 6);
		TS_ASSERT(boost::in_degree(*it, g) == 2 && boost::out_degree(*it, g) == 0);
	};

	void testBranching() {
		Scumm::v6::Engine *engine = new Scumm::v6::Engine();
		Disassembler *d = engine->getDisassembler();
		d->open("decompiler/test/branches.dmp");
		std::vector<Instruction> insts = d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		Graph g = c->getGraph();
		TS_ASSERT(boost::num_vertices(g) == 4);
		std::pair<VertexIterator, VertexIterator> range = boost::vertices(g);
		VertexIterator it = range.first;
		Group *gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 0);
		TS_ASSERT(boost::in_degree(*it, g) == 0 && boost::out_degree(*it, g) == 1);
		++it;
		gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 2);
		TS_ASSERT(boost::in_degree(*it, g) == 1 && boost::out_degree(*it, g) == 2);
		++it;
		gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 5);
		TS_ASSERT(boost::in_degree(*it, g) == 1 && boost::out_degree(*it, g) == 1);
		++it;
		gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 6);
		TS_ASSERT(boost::in_degree(*it, g) == 2 && boost::out_degree(*it, g) == 0);
	}

	void testGrouping() {
		Scumm::v6::Engine *engine = new Scumm::v6::Engine();
		Disassembler *d = engine->getDisassembler();
		d->open("decompiler/test/branches.dmp");
		std::vector<Instruction> insts = d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->getGraph();
		TS_ASSERT(boost::num_vertices(g) == 3);
		std::pair<VertexIterator, VertexIterator> range = boost::vertices(g);
		VertexIterator it = range.first;
		Group *gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 0);
		TS_ASSERT(gr->_end->_address == 2);		
		TS_ASSERT(boost::in_degree(*it, g) == 0 && boost::out_degree(*it, g) == 2);
		++it;
		gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 5);
		TS_ASSERT(boost::in_degree(*it, g) == 1 && boost::out_degree(*it, g) == 1);
		++it;
		gr = GET(*it);
		TS_ASSERT(gr->_start->_address == 6);
		TS_ASSERT(boost::in_degree(*it, g) == 2 && boost::out_degree(*it, g) == 0);
	}
};
