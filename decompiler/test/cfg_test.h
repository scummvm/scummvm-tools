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
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			switch (gr->_start->_address) {
			case 0:
				TS_ASSERT(boost::in_degree(*it, g) == 0 && boost::out_degree(*it, g) == 1);
				break;
			case 2:
				TS_ASSERT(boost::in_degree(*it, g) == 1 && boost::out_degree(*it, g) == 1);
				break;
			case 5:
				TS_ASSERT(boost::in_degree(*it, g) == 0 && boost::out_degree(*it, g) == 1);
				break;
			case 6:
				TS_ASSERT(boost::in_degree(*it, g) == 2 && boost::out_degree(*it, g) == 0);
				break;
			default:
				TS_ASSERT(false);
			}
		}
		delete c;
		delete engine;
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
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			switch (gr->_start->_address) {
			case 0:
				TS_ASSERT(boost::in_degree(*it, g) == 0 && boost::out_degree(*it, g) == 1);
				break;
			case 2:
				TS_ASSERT(boost::in_degree(*it, g) == 1 && boost::out_degree(*it, g) == 2);
				break;
			case 5:
				TS_ASSERT(boost::in_degree(*it, g) == 1 && boost::out_degree(*it, g) == 1);
				break;
			case 6:
				TS_ASSERT(boost::in_degree(*it, g) == 2 && boost::out_degree(*it, g) == 0);
				break;
			default:
				TS_ASSERT(false);
			}
		}
		delete c;
		delete engine;
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
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			switch (gr->_start->_address) {
			case 0:
				TS_ASSERT(gr->_end->_address == 2);		
				TS_ASSERT(boost::in_degree(*it, g) == 0 && boost::out_degree(*it, g) == 2);
				break;
			case 5:
				TS_ASSERT(boost::in_degree(*it, g) == 1 && boost::out_degree(*it, g) == 1);
				break;
			case 6:
				TS_ASSERT(boost::in_degree(*it, g) == 2 && boost::out_degree(*it, g) == 0);
				break;
			default:
				TS_ASSERT(false);
				break;
			}
		}
		delete c;
		delete engine;
	}

	void testShortCircuitDetection() {
		Scumm::v6::Engine *engine = new Scumm::v6::Engine();
		Disassembler *d = engine->getDisassembler();
		d->open("decompiler/test/short-circuit.dmp");
		std::vector<Instruction> insts = d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->getGraph();
		TS_ASSERT(boost::num_vertices(g) == 3);
		delete c;
		delete engine;
	}

	void testWhileDetection() {
		Scumm::v6::Engine *engine = new Scumm::v6::Engine();
		Disassembler *d = engine->getDisassembler();
		d->open("decompiler/test/while.dmp");
		std::vector<Instruction> insts = d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			if (gr->_start->_address == 0)
				TS_ASSERT(gr->_type == kWhileCond);
		}
		delete c;
		delete engine;
	}

	void testDoWhileDetection() {
		Scumm::v6::Engine *engine = new Scumm::v6::Engine();
		Disassembler *d = engine->getDisassembler();
		d->open("decompiler/test/while.dmp");
		std::vector<Instruction> insts = d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			if (gr->_start->_address == 3)
				TS_ASSERT(gr->_type == kDoWhileCond);
		}
		delete c;
		delete engine;
	}

	void testBreakDetection() {
		Scumm::v6::Engine *engine = new Scumm::v6::Engine();
		Disassembler *d = engine->getDisassembler();
		d->open("decompiler/test/break-while.dmp");
		std::vector<Instruction> insts = d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			if (gr->_start->_address == 0x14)
				TS_ASSERT(gr->_type == kBreak);
		}
		delete c;

		d = engine->getDisassembler();
		d->open("decompiler/test/break-do-while.dmp");
		insts = d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			if (gr->_start->_address == 0xA)
				TS_ASSERT(gr->_type == kBreak);
		}
		delete c;

		d = engine->getDisassembler();
		d->open("decompiler/test/break-do-while2.dmp");
		insts = d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			if (gr->_start->_address == 0xD)
				TS_ASSERT(gr->_type == kBreak);
		}
		delete c;
		delete engine;
	}

	void testContinueDetection() {
		Scumm::v6::Engine *engine = new Scumm::v6::Engine();
		Disassembler *d = engine->getDisassembler();
		d->open("decompiler/test/continue-while.dmp");
		std::vector<Instruction> insts = d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			if (gr->_start->_address == 0x14)
				TS_ASSERT(gr->_type == kContinue);
		}
		delete c;

		d = engine->getDisassembler();
		d->open("decompiler/test/continue-do-while.dmp");
		insts = d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			if (gr->_start->_address == 0xA)
				TS_ASSERT(gr->_type == kContinue);
		}
		delete c;

		d = engine->getDisassembler();
		d->open("decompiler/test/continue-do-while2.dmp");
		insts = d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			Group *gr = GET(*it);
			if (gr->_start->_address == 0xD)
				TS_ASSERT(gr->_type == kContinue);
		}
		delete c;
		delete engine;
	}
};
