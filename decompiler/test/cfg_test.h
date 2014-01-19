/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
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

#include <cxxtest/TestSuite.h>

#include "decompiler/control_flow.h"
#include "decompiler/disassembler.h"
#include "decompiler/graph.h"
#include "decompiler/scummv6/engine.h"
#include "decompiler/kyra/engine.h"

#include <vector>
#define GET(vertex) (boost::get(boost::vertex_name, g, vertex))

class CFGTestSuite : public CxxTest::TestSuite {
public:
	void testUnreachable() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/unreachable.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		Graph g = c->getGraph();
		TS_ASSERT(boost::num_vertices(g) == 4);
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			switch ((*gr->_start)->_address) {
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
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/branches.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		Graph g = c->getGraph();
		TS_ASSERT(boost::num_vertices(g) == 4);
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			switch ((*gr->_start)->_address) {
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
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/branches.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->getGraph();
		TS_ASSERT(boost::num_vertices(g) == 3);
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			switch ((*gr->_start)->_address) {
			case 0:
				TS_ASSERT((*gr->_end)->_address == 2);
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
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/short-circuit.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->getGraph();
		TS_ASSERT(boost::num_vertices(g) == 3);
		delete c;
		delete engine;
	}

	void testWhileDetection() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/while.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0)
				TS_ASSERT(gr->_type == kWhileCondGroupType);
		}
		delete c;
		delete engine;
	}

	void testDoWhileDetection() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/while.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 3)
				TS_ASSERT(gr->_type == kDoWhileCondGroupType);
		}
		delete c;
		delete engine;
	}

	void testBreakDetection() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/break-while.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x14)
				TS_ASSERT(gr->_type == kBreakGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/break-do-while.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0xA)
				TS_ASSERT(gr->_type == kBreakGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/break-do-while2.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0xD)
				TS_ASSERT(gr->_type == kBreakGroupType);
		}
		delete c;
		delete engine;
	}

	void testContinueDetection() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/continue-while.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x14)
				TS_ASSERT(gr->_type == kContinueGroupType);
			if ((*gr->_start)->_address == 0x1a)
				TS_ASSERT(gr->_type == kNormalGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/continue-do-while.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0xA)
				TS_ASSERT(gr->_type == kContinueGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/continue-do-while2.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0xD)
				TS_ASSERT(gr->_type == kContinueGroupType);
		}
		delete c;
		delete engine;
	}

	void testIfDetection() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/if.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x0)
				TS_ASSERT(gr->_type == kIfCondGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/break-do-while.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x0)
				TS_ASSERT(gr->_type == kIfCondGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/break-do-while2.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x3)
				TS_ASSERT(gr->_type == kIfCondGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/continue-do-while.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x0)
				TS_ASSERT(gr->_type == kIfCondGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/continue-do-while2.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x3)
				TS_ASSERT(gr->_type == kIfCondGroupType);
		}
		delete c;
		delete engine;
	}

	void testElseDetection() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/if-else.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x10) {
				TS_ASSERT(gr->_startElse);
				TS_ASSERT(gr->_endElse.size() == 1 && gr->_endElse[0] == gr);
			}
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/if-no-else.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x0)
				TS_ASSERT(gr->_type == kIfCondGroupType);
			TS_ASSERT(!gr->_startElse);
		}

		delete c;
		delete engine;
	}

	void testNestedLoops() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/do-while-in-while.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x0)
				TS_ASSERT(gr->_type == kWhileCondGroupType);
			if ((*gr->_start)->_address == 0xd)
				TS_ASSERT(gr->_type == kDoWhileCondGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/nested-do-while.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x6)
				TS_ASSERT(gr->_type == kDoWhileCondGroupType);
			if ((*gr->_start)->_address == 0x10)
				TS_ASSERT(gr->_type == kDoWhileCondGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/nested-while.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x0)
				TS_ASSERT(gr->_type == kWhileCondGroupType);
			if ((*gr->_start)->_address == 0xa)
				TS_ASSERT(gr->_type == kWhileCondGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/nested-while2.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x0)
				TS_ASSERT(gr->_type == kWhileCondGroupType);
			if ((*gr->_start)->_address == 0xd)
				TS_ASSERT(gr->_type == kWhileCondGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/while-in-do-while.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x0)
				TS_ASSERT(gr->_type == kWhileCondGroupType);
			if ((*gr->_start)->_address == 0x10)
				TS_ASSERT(gr->_type == kDoWhileCondGroupType);
		}
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/while-in-do-while2.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			if ((*gr->_start)->_address == 0x3)
				TS_ASSERT(gr->_type == kWhileCondGroupType);
			if ((*gr->_start)->_address == 0x13)
				TS_ASSERT(gr->_type == kDoWhileCondGroupType);
		}
		delete c;
		delete engine;
	}

	// This test requires script-30.dmp from Sam & Max: Hit The Road.
	// 6e48faca13e1f6df9341567608962744 *script-30.dmp
	void testSamAndMaxScript30() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/script-30.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		VertexRange range = boost::vertices(g);
		for (VertexIterator it = range.first; it != range.second; ++it) {
			GroupPtr gr = GET(*it);
			switch ((*gr->_start)->_address) {
			case 0x6:
				TS_ASSERT(gr->_type == kWhileCondGroupType);
				TS_ASSERT(!gr->_startElse);
				TS_ASSERT(gr->_endElse.empty());
				break;
			case 0x19:
			case 0x3A:
			case 0x4F:
			case 0x68:
			case 0x74: // Allow inclusion of the pop instruction immediately before
			case 0x75:
			case 0x92:
				TS_ASSERT(gr->_type == kIfCondGroupType);
				TS_ASSERT(!gr->_startElse);
				TS_ASSERT(gr->_endElse.empty());
				break;
			case 0x8B:
				TS_ASSERT(gr->_type == kNormalGroupType);
				TS_ASSERT(gr->_startElse);
				TS_ASSERT(gr->_endElse.size() == 1 && (*gr->_endElse[0]->_start)->_address == 0x8B);
				break;
			case 0x91:
				TS_ASSERT(gr->_type == kNormalGroupType || gr->_type == kIfCondGroupType); // Allow inclusion of the pop instruction immediately before
				TS_ASSERT(gr->_startElse);
				TS_ASSERT(gr->_endElse.empty());
				break;
			case 0xA6:
				TS_ASSERT(gr->_type == kNormalGroupType);
				TS_ASSERT(!gr->_startElse);
				TS_ASSERT(gr->_endElse.size() == 1 && (*gr->_endElse[0]->_start)->_address == 0x91);
				break;
			default:
				TS_ASSERT(gr->_type == kNormalGroupType);
				TS_ASSERT(!gr->_startElse);
				TS_ASSERT(gr->_endElse.empty());
				break;
			}
		}
		delete c;
		delete engine;
	}

	// This test requires _START04.EMC from the CD demo of
	// Legend of Kyrandia: Hand of Fate, found in MISC_EMC.PAK.
	// Extract using extract_kyra from the scummvm-tools-cli bundle.
	// ba2821ac6da96394ce0af75a3cbe48eb *_START04.EMC
	void testFunctionDetection() {
		InstVec insts;
		Kyra::Kyra2Engine *engine = new Kyra::Kyra2Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/_START04.EMC");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		TS_ASSERT(engine->_functions.size() == 15);
		FuncMap::iterator it = engine->_functions.begin();
		TS_ASSERT(it->first == 0x0);
		++it;
		TS_ASSERT(it->first == 0x7E);
		++it;
		TS_ASSERT(it->first == 0xFC);
		++it;
		TS_ASSERT(it->first == 0x100);
		++it;
		TS_ASSERT(it->first == 0x1F4);
		++it;
		TS_ASSERT(it->first == 0x1F8);
		++it;
		TS_ASSERT(it->first == 0x276);
		++it;
		TS_ASSERT(it->first == 0x278);
		++it;
		TS_ASSERT(it->first == 0x2DE);
		++it;
		TS_ASSERT(it->first == 0x2E0);
		++it;
		TS_ASSERT(it->first == 0x30C);
		++it;
		TS_ASSERT(it->first == 0x30E);
		++it;
		TS_ASSERT(it->first == 0x33A);
		++it;
		TS_ASSERT(it->first == 0x33C);
		++it;
		TS_ASSERT(it->first == 0x33E);
		++it;

		delete c;
		delete engine;
	}
};
