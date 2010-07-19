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
#include <set>

#include <boost/format.hpp>

#define GET(vertex) (boost::get(boost::vertex_name, _g, vertex))
#define GET_EDGE(edge) (boost::get(boost::edge_attribute, _g, edge))

static int dupindex = 0;

EntryPtr StackEntry::dup(std::ostream &output) {
	if (_type == seDup)
		return this;

	EntryPtr dupEntry = new DupEntry(++dupindex);
	output << dupEntry << " = " << this;
	return dupEntry;
}

std::string CodeGenerator::indentString(std::string s) {
	std::stringstream stream;
	std::string indent(kIndentAmount, ' ');
	for (uint i = 0; i < _indentLevel; i++)
		stream << indent;
	stream << s;
	return stream.str();
}

CodeGenerator::CodeGenerator(Engine *engine, std::ostream &output) : _output(output) {
	_engine = engine;
}

typedef std::pair<GraphVertex, Stack> DFSEntry;

void CodeGenerator::generate(const Graph &g) {
	_g = g;

	// Find entry point
	// FIXME: For simplicity, we simply treat the first group as the entry point, because that's how SCUMM works.
	// This should be changed later to allow for functions etc.
	VertexRange vr = boost::vertices(_g);
	GraphVertex entryPoint = *(vr.first);
	GroupPtr p = GET(entryPoint);
	while (p->_prev != NULL)
		p = p->_prev;
	entryPoint = p->_vertex;

	// DFS from entry point to process each vertex
	std::stack<DFSEntry> dfsStack;
	std::set<GraphVertex> seen;
	dfsStack.push(DFSEntry(entryPoint, Stack()));
	seen.insert(entryPoint);
	while (!dfsStack.empty()) {
		DFSEntry e = dfsStack.top();
		GroupPtr tmp = GET(e.first);
		dfsStack.pop();
		_stack = e.second;
		GraphVertex v = e.first;
		process(v);
		OutEdgeRange r = boost::out_edges(v, _g);
		for (OutEdgeIterator i = r.first; i != r.second; ++i) {
			GraphVertex target = boost::target(*i, _g);
			if (seen.find(target) == seen.end()) {
				dfsStack.push(DFSEntry(target, _stack));
				seen.insert(target);
			}
		}
	}

	// Print output
	p = GET(entryPoint);
	while (p->_next != NULL) {
		for (std::vector<std::string>::iterator it = p->_code.begin(); it != p->_code.end(); ++it)
			_output << *it << std::endl;
		p = p->_next;
	}
}

void CodeGenerator::addOutputLine(std::string s) {
	_curGroup->_code.push_back(indentString(s));
}

void CodeGenerator::writeAssignment(EntryPtr dst, EntryPtr src) {
	std::stringstream s;
	s << dst << " = " << src << ";";
	addOutputLine(s.str());
}

void CodeGenerator::process(GraphVertex v) {
	// TODO: Add keyword output
	_curGroup = GET(v);
	ConstInstIterator it = _curGroup->_start;
	do {
		switch (it->_type) {
		// We handle plain dups here because their behavior should be identical across instruction sets and this prevents implementation error.
		case kDup:
		{
			std::stringstream s;
			EntryPtr p = _stack.top()->dup(s);
			if (s.str().length() > 0)
				addOutputLine(s.str());
			_stack.pop();
			_stack.push(p);
			_stack.push(p);
			break;
		}
		default:
			processInst(*it);
		}
	} while (it++ != _curGroup->_end);
}
