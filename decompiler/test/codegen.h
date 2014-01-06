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

#include <cxxtest/TestSuite.h>

#include "decompiler/control_flow.h"
#include "decompiler/disassembler.h"
#include "decompiler/graph.h"
#include "decompiler/codegen.h"
#include "decompiler/scummv6/engine.h"
#include "decompiler/kyra/engine.h"

#include <vector>
#define GET(vertex) (boost::get(boost::vertex_name, g, vertex))

#include <streambuf>
#include <ostream>

// Define an ostream which doesn't output anything to avoid clutter
// Source: http://groups.google.com/group/comp.lang.c++/msg/4a81a74500f9f4d3?hl=en
template<class cT, class traits = std::char_traits<cT> >
class basic_nullbuf: public std::basic_streambuf<cT, traits> {
	typename traits::int_type overflow(typename traits::int_type c)
	{
		return traits::not_eof(c);
	}
};

template<class cT, class traits = std::char_traits<cT> >
class basic_onullstream: public std::basic_ostream<cT, traits> {
public:
  basic_onullstream():
	std::basic_ios<cT, traits>(&m_sbuf),
	std::basic_ostream<cT, traits>(&m_sbuf)
	{
		init(&m_sbuf);
	}

private:
	basic_nullbuf<cT, traits> m_sbuf;
};

typedef basic_onullstream<char> onullstream;

std::string removeSpaces(std::string s) {
  size_t found;
  while ((found = s.find(' '))!=std::string::npos)
    s = s.erase(found,1);
	return s;
}

typedef std::vector<std::string>::iterator CodeIterator;

class CodeGenTestSuite : public CxxTest::TestSuite {
public:
	void testContinue() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/continue-do-while.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		onullstream ns;
		CodeGenerator *cg = engine->getCodeGenerator(ns);
		cg->generate(g);

		VertexIterator v = boost::vertices(g).first;
		std::vector<std::string> output, expected;
		expected.push_back("do{");
		expected.push_back("if(18 != var321) {");
		expected.push_back("continue;");
		expected.push_back("}");
		expected.push_back("VAR_CHARSET_MASK--;");
		expected.push_back("} while (42 == VAR_CHARSET_MASK)");
		expected.push_back("stopObjectCodeA();");
		GroupPtr gr = GET(*v);
		// Find first node
		while (gr->_prev != NULL)
			gr = gr->_prev;
		// Copy out all lines of code
		while (gr != NULL) {
			for (std::vector<CodeLine>::iterator it = gr->_code.begin(); it != gr->_code.end(); ++it)
				output.push_back(it->_line);
			gr = gr->_next;
		}
		TS_ASSERT(output.size() == expected.size());
		CodeIterator it, it2;
		for (it = output.begin(), it2 = expected.begin(); it != output.end() && it2 != expected.end(); ++it, ++it2) {
			TS_ASSERT(removeSpaces(*it).compare(removeSpaces(*it2)) == 0);
		}

		delete cg;
		delete c;
		delete engine;
	}

	void testBreak() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/break-while.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		onullstream ns;
		CodeGenerator *cg = engine->getCodeGenerator(ns);
		cg->generate(g);

		VertexIterator v = boost::vertices(g).first;
		std::vector<std::string> output, expected;
		expected.push_back("while (42 != VAR_CHARSET_MASK) {");
		expected.push_back("if (18 != var321) {");
		expected.push_back("break;");
		expected.push_back("}");
		expected.push_back("VAR_CHARSET_MASK--;");
		expected.push_back("}");
		expected.push_back("stopObjectCodeA();");
		GroupPtr gr = GET(*v);
		// Find first node
		while (gr->_prev != NULL)
			gr = gr->_prev;
		// Copy out all lines of code
		while (gr != NULL) {
			for (std::vector<CodeLine>::iterator it = gr->_code.begin(); it != gr->_code.end(); ++it)
				output.push_back(it->_line);
			gr = gr->_next;
		}
		TS_ASSERT(output.size() == expected.size());
		CodeIterator it, it2;
		for (it = output.begin(), it2 = expected.begin(); it != output.end() && it2 != expected.end(); ++it, ++it2) {
			TS_ASSERT(removeSpaces(*it).compare(removeSpaces(*it2)) == 0);
		}

		delete cg;
		delete c;
		delete engine;
	}

	void testElse() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/if-else.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		onullstream ns;
		CodeGenerator *cg = engine->getCodeGenerator(ns);
		cg->generate(g);

		VertexIterator v = boost::vertices(g).first;
		std::vector<std::string> output, expected;
		expected.push_back("if (42 != VAR_CHARSET_MASK) {");
		expected.push_back("VAR_CHARSET_MASK--;");
		expected.push_back("} else {");
		expected.push_back("VAR_CHARSET_MASK++;");
		expected.push_back("}");
		expected.push_back("stopObjectCodeA();");
		GroupPtr gr = GET(*v);
		// Find first node
		while (gr->_prev != NULL)
			gr = gr->_prev;
		// Copy out all lines of code
		while (gr != NULL) {
			for (std::vector<CodeLine>::iterator it = gr->_code.begin(); it != gr->_code.end(); ++it)
				output.push_back(it->_line);
			gr = gr->_next;
		}
		TS_ASSERT(output.size() == expected.size());
		CodeIterator it, it2;
		for (it = output.begin(), it2 = expected.begin(); it != output.end() && it2 != expected.end(); ++it, ++it2) {
			TS_ASSERT(removeSpaces(*it).compare(removeSpaces(*it2)) == 0);
		}

		delete cg;
		delete c;
		delete engine;
	}

	// This test requires script-30 and script-48.dmp from Sam & Max: Hit The Road.
	// 6e48faca13e1f6df9341567608962744 *script-30.dmp
	// afd7dc5d377894b3b9d0504927adf1b1 *script-48.dmp
	void testCoalescing() {
		InstVec insts;
		Scumm::v6::Scummv6Engine *engine = new Scumm::v6::Scummv6Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/script-30.dmp");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		onullstream ns;
		CodeGenerator *cg = engine->getCodeGenerator(ns);
		cg->generate(g);

		VertexIterator v = boost::vertices(g).first;
		GroupPtr gr = GET(*v);
		// Find first node
		while (gr->_prev != NULL)
			gr = gr->_prev;
		// Find vertex to test
		while ((*gr->_start)->_address != 0x91)
			gr = gr->_next;

		TS_ASSERT(gr->_code.size() == 2);
		TS_ASSERT(removeSpaces(gr->_code[0]._line).compare("}else{") == 0);
		TS_ASSERT(removeSpaces(gr->_code[1]._line).substr(0, 2).compare("if") == 0);

		delete cg;
		delete c;
		delete engine;

		insts.clear();
		engine = new Scumm::v6::Scummv6Engine();
		d = engine->getDisassembler(insts);
		d->open("decompiler/test/script-48.dmp");
		d->disassemble();
		delete d;
		c = new ControlFlow(insts, engine);
		c->createGroups();
		g = c->analyze();
		cg = engine->getCodeGenerator(ns);
		cg->generate(g);

		v = boost::vertices(g).first;
		gr = GET(*v);
		// Find first node
		while (gr->_prev != NULL)
			gr = gr->_prev;
		// Find vertex to test
		while ((*gr->_start)->_address != 0x191)
			gr = gr->_next;

		TS_ASSERT(gr->_code.size() == 1);
		TS_ASSERT(removeSpaces(gr->_code[0]._line).substr(0, 7).compare("}elseif") == 0);

		delete cg;
		delete c;
		delete engine;
	}

	// This test requires _START04.EMC from the CD demo of
	// Legend of Kyrandia: Hand of Fate, found in MISC_EMC.PAK.
	// Extract using extract_kyra from the scummvm-tools-cli bundle.
	// ba2821ac6da96394ce0af75a3cbe48eb *_START04.EMC
	void testKyra2Start04CodeGen() {
		InstVec insts;
		Kyra::Kyra2Engine *engine = new Kyra::Kyra2Engine();
		Disassembler *d = engine->getDisassembler(insts);
		d->open("decompiler/test/_START04.EMC");
		d->disassemble();
		delete d;
		ControlFlow *c = new ControlFlow(insts, engine);
		c->createGroups();
		Graph g = c->analyze();
		engine->postCFG(insts, g);
		onullstream ns;
		CodeGenerator *cg = engine->getCodeGenerator(ns);
		cg->generate(g);

		VertexIterator v = boost::vertices(g).first;
		std::vector<std::string> output, expected;
		expected.push_back("auto_sub0x278(param1, param2, param3, param4) {");
		expected.push_back("if (var1 > param1 && var1 < param3 && var2 > param2 && var2 < param4) {");
		expected.push_back("retval = o1_queryGameFlag(3);");
		expected.push_back("if (retval) {");
		expected.push_back("retval = o2_drawBox(param1, param2, param3, param4, 199);");
		expected.push_back("}");
		expected.push_back("retval = 1;");
		expected.push_back("return;");
		expected.push_back("}");
		expected.push_back("retval = o1_queryGameFlag(3);");
		expected.push_back("if (retval) {");
		expected.push_back("retval = o2_drawBox(param1, param2, param3, param4, 132);");
		expected.push_back("}");
		expected.push_back("retval = 0;");
		expected.push_back("return;");
		expected.push_back("}");

		GroupPtr gr = GET(*v);
		// Find first node
		while (gr->_prev != NULL)
			gr = gr->_prev;

		// Find right starting node
		while ((*gr->_start)->_address != 0x278)
			gr = gr->_next;

		// Copy out all lines of code from function
		while ((*gr->_start)->_address <= 0x2DC) {
			for (std::vector<CodeLine>::iterator it = gr->_code.begin(); it != gr->_code.end(); ++it) {
				if (it->_line.compare("") != 0)
					output.push_back(it->_line);
			}
			gr = gr->_next;
		}
		TS_ASSERT(output.size() == expected.size());
		CodeIterator it, it2;
		for (it = output.begin(), it2 = expected.begin(); it != output.end() && it2 != expected.end(); ++it, ++it2) {
			TS_ASSERT(removeSpaces(*it).compare(removeSpaces(*it2)) == 0);
		}

		delete cg;
		delete c;
		delete engine;
	}
};
