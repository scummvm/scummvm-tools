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
#include "engine.h"

#include <iostream>
#include <set>

#include <boost/format.hpp>

#define GET(vertex) (boost::get(boost::vertex_name, _g, vertex))
#define GET_EDGE(edge) (boost::get(boost::edge_attribute, _g, edge))

static int dupindex = 0;

std::ostream &IntEntry::print(std::ostream &output) const {
	if (_isSigned)
		output << _val;
	else
		output << (uint32)_val;
	return output;
}

std::ostream &VarEntry::print(std::ostream &output) const {
	return output << _varName;
}

std::ostream &BinaryOpEntry::print(std::ostream &output) const {
	return output << "(" << _lhs << " " << _op << " " << _rhs << ")";
}

std::ostream &UnaryOpEntry::print(std::ostream &output) const {
	return output << _op << "(" << _operand << ")";
}

std::ostream &DupEntry::print(std::ostream &output) const {
	return output << "temp" << _idx;
}

std::ostream &ArrayEntry::print(std::ostream &output) const {
	output << _arrayName;
	for (EntryList::const_iterator i = _idxs.begin(); i != _idxs.end(); ++i)
		output << "[" << *i << "]";
	return output;
}

std::ostream &StringEntry::print(std::ostream &output) const {
	return output << _str;
}

std::ostream &ListEntry::print(std::ostream &output) const {
	output << "[";
	for (EntryList::const_iterator i = _items.begin(); i != _items.end(); ++i) {
		if (i != _items.begin())
			output << ", ";
		output << *i;
	}
	output << "]";
	return output;
}

std::ostream &CallEntry::print(std::ostream &output) const {
	output << _funcName << "(";
	for (EntryList::const_iterator i = _args.begin(); i != _args.end(); ++i) {
		if (i != _args.begin())
			output << ", ";
		output << *i;
	}
	output << ")";
	return output;
}

EntryPtr StackEntry::dup(std::ostream &output) {
	if (_type == seDup)
		return this;

	EntryPtr dupEntry = new DupEntry(++dupindex);
	output << dupEntry << " = " << (EntryPtr)this << ";";
	return dupEntry;
}

EntryPtr IntEntry::dup(std::ostream &output) {
	return new IntEntry(_val, _isSigned);
}

std::string CodeGenerator::indentString(std::string s) {
	std::stringstream stream;
	stream << std::string(kIndentAmount * _indentLevel, ' ') << s;
	return stream.str();
}

CodeGenerator::CodeGenerator(Engine *engine, std::ostream &output, ArgOrder binOrder, ArgOrder callOrder) : _output(output), _binOrder(binOrder), _callOrder(callOrder) {
	_engine = engine;
	_indentLevel = 0;
}

typedef std::pair<GraphVertex, EntryStack> DFSEntry;

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
	Stack<DFSEntry> dfsStack;
	std::set<GraphVertex> seen;
	dfsStack.push(DFSEntry(entryPoint, EntryStack()));
	seen.insert(entryPoint);
	while (!dfsStack.empty()) {
		DFSEntry e = dfsStack.pop();
		GroupPtr tmp = GET(e.first);
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
	// TODO: Proper indenting
	p = GET(entryPoint);
	while (p != NULL) {
		for (std::vector<CodeLine>::iterator it = p->_code.begin(); it != p->_code.end(); ++it) {
			if (it->_unindentBefore && _indentLevel != 0)
				_indentLevel--;
			_output << boost::format("%08X: %s") % p->_start->_address % indentString(it->_line) << std::endl;
			if (it->_indentAfter)
				_indentLevel++;
		}
		p = p->_next;
	}
}

void CodeGenerator::addOutputLine(std::string s, bool unindentBefore, bool indentAfter) {
	_curGroup->_code.push_back(CodeLine(s, unindentBefore, indentAfter));
}

void CodeGenerator::writeAssignment(EntryPtr dst, EntryPtr src) {
	std::stringstream s;
	s << dst << " = " << src << ";";
	addOutputLine(s.str());
}

void CodeGenerator::process(GraphVertex v) {
	_curGroup = GET(v);

	// Check if we should add else start
	if (_curGroup->_startElse)
		addOutputLine("} else {", true, true);

	// Check ingoing edges to see if we want to add any extra output
	InEdgeRange ier = boost::in_edges(v, _g);
	for (InEdgeIterator ie = ier.first; ie != ier.second; ++ie) {
		GraphVertex in = boost::source(*ie, _g);
		GroupPtr inGroup = GET(in);
		if (inGroup == _curGroup->_prev)
			continue;
		switch (inGroup->_type) {
		case kDoWhileCond:
			addOutputLine("do {", false, true);
			break;
		case kIfCond:
			if (!_curGroup->_startElse)
				addOutputLine("}", true, false);
			break;
		case kWhileCond:
			addOutputLine("}", true, false);
			break;
		default:
			break;
		}
	}

	ConstInstIterator it = _curGroup->_start;
	do {
		if (it->_codeGenData.find("\xC0") == 0)
			processInst(*it);
		else {
			switch (it->_type) {
			// We handle plain dups here because their behavior should be identical across instruction sets and this prevents implementation error.
			case kDup:
				{
					std::stringstream s;
					EntryPtr p = _stack.pop()->dup(s);
					if (s.str().length() > 0)
						addOutputLine(s.str());
					_stack.push(p);
					_stack.push(p);
					break;
				}
			case kUnaryOp:
				//TODO: Allow operator to be placed on either side of operand
				_stack.push(new UnaryOpEntry(_stack.pop(), it->_codeGenData));
				break;
			case kBinaryOp:
			case kComparison:
				{
					EntryPtr op1 = _stack.pop();
					EntryPtr op2 = _stack.pop();
					if (_binOrder == kFIFO)
						_stack.push(new BinaryOpEntry(op2, op1, it->_codeGenData));
					else if (_binOrder == kLIFO)
						_stack.push(new BinaryOpEntry(op1, op2, it->_codeGenData));
					break;
				}
			case kCondJump:
			case kCondJumpRel:
				{
					processInst(*it);
					std::stringstream s;
					switch (_curGroup->_type) {
					case kIfCond:
						if (_curGroup->_startElse && _curGroup->_code.size() == 1) {
							_curGroup->_code.clear();
							_curGroup->_coalescedElse = true;
							s << "} else ";
						}
						s << "if (" << _stack.pop() << ") {";
						addOutputLine(s.str(), _curGroup->_coalescedElse, true);
						break;
					case kWhileCond:
						s << "while (" << _stack.pop() << ") {";
						addOutputLine(s.str(), false, true);
						break;
					case kDoWhileCond:
						s << "} while (" << _stack.pop() << ")";
						addOutputLine(s.str(), true, false);
						break;
					default:
						break;
					}
				}
				break;
			case kJump:
			case kJumpRel:
				switch (_curGroup->_type) {
				case kBreak:
					addOutputLine("break;");
					break;
				case kContinue:
					addOutputLine("continue;");
					break;
				default:
					{
						bool printJump = true;
						OutEdgeRange r = boost::out_edges(v, _g);
						for (OutEdgeIterator e = r.first; e != r.second && printJump; ++e) {
							// Don't output jump to next vertex
							if (boost::target(*e, _g) == _curGroup->_next->_vertex) {
								printJump = false;
								break;
							}

							// Don't output jump if next vertex starts an else block
							if (_curGroup->_next->_startElse) {
								printJump = false;
								break;
							}

							OutEdgeRange targetR = boost::out_edges(boost::target(*e, _g), _g);
							for (OutEdgeIterator targetE = targetR.first; targetE != targetR.second; ++targetE) {
								// Don't output jump to while loop that has jump to next vertex
								if (boost::target(*targetE, _g) == _curGroup->_next->_vertex)
									printJump = false;
							}
						}
						if (printJump) {
							std::stringstream s;
							s << boost::format("jump %X;") % _engine->getDestAddress(it);
							addOutputLine(s.str());
						}
					}
					break;
				}
				break;
			case kSpecial:
				{
					_argList.clear();
					bool returnsValue = (it->_codeGenData.find("r") == 0);
					std::string metadata = (!returnsValue ? it->_codeGenData : it->_codeGenData.substr(1) );
					for (size_t i = 0; i < metadata.length(); i++)
						processSpecialMetadata(*it, metadata[i]);
					_stack.push(new CallEntry(it->_name, _argList));
					if (!returnsValue) {
						std::stringstream stream;
						stream << _stack.pop() << ";";
						addOutputLine(stream.str());
					}
					break;
				}
			default:
				processInst(*it);
				break;
			}
		}
	} while (it++ != _curGroup->_end);

	// Add else end if necessary
	if (_curGroup->_endElse != NULL && !_curGroup->_endElse->_coalescedElse)
		addOutputLine("}", true, false);
}

void CodeGenerator::addArg(EntryPtr p) {
	if (_callOrder == kFIFO)
		_argList.push_front(p);
	else if (_callOrder == kLIFO)
		_argList.push_back(p);
}

void CodeGenerator::processSpecialMetadata(const Instruction inst, char c) {
	switch (c) {
		case 'p':
			addArg(_stack.pop());
			break;
		default:
			std::cerr << boost::format("Unknown character in metadata: %c\n") % c ;
			break;
	}
}
