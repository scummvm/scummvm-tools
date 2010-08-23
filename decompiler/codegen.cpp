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

#include <algorithm>
#include <iostream>
#include <set>
#include <boost/format.hpp>

#define GET(vertex) (boost::get(boost::vertex_name, _g, vertex))
#define GET_EDGE(edge) (boost::get(boost::edge_attribute, _g, edge))

static int dupindex = 0;

int32 IntEntry::getValue() {
	return _val;
}

bool IntEntry::getSigned() {
	return _isSigned;
}

std::ostream &IntEntry::print(std::ostream &output) const {
	if (_isSigned)
		output << (int32)_val;
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
	if (_isPostfix)
		return output << "(" << _operand << ")" << _op;
	else
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
	if (_type == kDupStackEntry)
		return this;

	EntryPtr dupEntry = new DupEntry(++dupindex);
	output << dupEntry << " = " << (EntryPtr)this << ";";
	return dupEntry;
}

EntryPtr IntEntry::dup(std::ostream &output) {
	return new IntEntry(_val, _isSigned);
}

std::string CodeGenerator::constructFuncSignature(const Function &func) {
	return "";
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








	for (FuncMap::iterator fn = _engine->_functions.begin(); fn != _engine->_functions.end(); ++fn)
	{
		_indentLevel = 0;
		while (!_stack.empty())
			_stack.pop();
		GraphVertex entryPoint = fn->second._v;
		std::string funcSignature = constructFuncSignature(fn->second);
		bool printFuncSignature = !funcSignature.empty();
		if (printFuncSignature) {
			_curGroup = GET(entryPoint);
			if (!(fn == _engine->_functions.begin()))
				addOutputLine("");
			addOutputLine(funcSignature, false, true);
		}








		GroupPtr lastGroup = GET(entryPoint);









		// DFS from entry point to process each vertex
		Stack<DFSEntry> dfsStack;
		std::set<GraphVertex> seen;
		dfsStack.push(DFSEntry(entryPoint, EntryStack()));
		seen.insert(entryPoint);
		while (!dfsStack.empty()) {
			DFSEntry e = dfsStack.pop();
			GroupPtr tmp = GET(e.first);
			if (tmp->_start->_address > lastGroup->_start->_address)
				lastGroup = tmp;
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












		if (printFuncSignature) {
			_curGroup = lastGroup;
			addOutputLine("}", true, false);
		}





		// Print output
		GroupPtr p = GET(entryPoint);
		while (p != NULL) {
			for (std::vector<CodeLine>::iterator it = p->_code.begin(); it != p->_code.end(); ++it) {
				if (it->_unindentBefore) {
					assert(_indentLevel > 0);
					_indentLevel--;



























				}
				_output << boost::format("%08X: %s") % p->_start->_address % indentString(it->_line) << std::endl;
				if (it->_indentAfter)
					_indentLevel++;
			}
			p = p->_next;
		}

		if (_indentLevel != 0)
			std::cerr << boost::format("WARNING: Indent level for function at %d ended at %d\n") % fn->first % _indentLevel;
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
	_curVertex = v;
	_curGroup = GET(v);

	// Check if we should add else start
	if (_curGroup->_startElse)
		addOutputLine("} else {", true, true);

	// Check ingoing edges to see if we want to add any extra output
	InEdgeRange ier = boost::in_edges(v, _g);
	for (InEdgeIterator ie = ier.first; ie != ier.second; ++ie) {
		GraphVertex in = boost::source(*ie, _g);
		GroupPtr inGroup = GET(in);

		if (!boost::get(boost::edge_attribute, _g, *ie)._isJump || inGroup->_stackLevel == -1)
			continue;

		switch (inGroup->_type) {
		case kDoWhileCondGroupType:
			addOutputLine("do {", false, true);
			break;
		case kIfCondGroupType:
			if (!_curGroup->_startElse)
				addOutputLine("}", true, false);
			break;
		case kWhileCondGroupType:
			addOutputLine("}", true, false);
			break;
		default:
			break;
		}
	}

	ConstInstIterator it = _curGroup->_start;
	do {
		processInst(*it);
	} while (it++ != _curGroup->_end);

	// Add else end if necessary
	for (ElseEndIterator elseIt = _curGroup->_endElse.begin(); elseIt != _curGroup->_endElse.end(); ++elseIt) {
		if (!(*elseIt)->_coalescedElse)
			addOutputLine("}", true, false);
	}
}

void CodeGenerator::processInst(const Instruction inst) {
	switch (inst._type) {
	// We handle plain dups here because their behavior should be identical across instruction sets and this prevents implementation error.
	case kDupInstType:
		{
			std::stringstream s;
			EntryPtr p = _stack.pop()->dup(s);
			if (s.str().length() > 0)
				addOutputLine(s.str());
			_stack.push(p);
			_stack.push(p);
			break;
		}
	case kUnaryOpPreInstType:
	case kUnaryOpPostInstType:
		_stack.push(new UnaryOpEntry(_stack.pop(), inst._codeGenData, inst._type == kUnaryOpPostInstType));
		break;
	case kBinaryOpInstType:
		{
			EntryPtr op1 = _stack.pop();
			EntryPtr op2 = _stack.pop();
			if (_binOrder == kFIFOArgOrder)
				_stack.push(new BinaryOpEntry(op2, op1, inst._codeGenData));
			else if (_binOrder == kLIFOArgOrder)
				_stack.push(new BinaryOpEntry(op1, op2, inst._codeGenData));
			break;
		}
	case kCondJumpInstType:
	case kCondJumpRelInstType:
		{
			std::stringstream s;
			switch (_curGroup->_type) {
			case kIfCondGroupType:
				if (_curGroup->_startElse && _curGroup->_code.size() == 1) {
					OutEdgeRange oer = boost::out_edges(_curVertex, _g);
					bool coalesceElse = false;
					for (OutEdgeIterator oe = oer.first; oe != oer.second; ++oe) {
						GroupPtr oGr = GET(boost::target(*oe, _g))->_prev;
						if (std::find(oGr->_endElse.begin(), oGr->_endElse.end(), _curGroup.get()) != oGr->_endElse.end())
							coalesceElse = true;
					}
					if (coalesceElse) {
						_curGroup->_code.clear();
						_curGroup->_coalescedElse = true;
						s << "} else ";
					}
				}
				s << "if (" << _stack.pop() << ") {";
				addOutputLine(s.str(), _curGroup->_coalescedElse, true);
				break;
			case kWhileCondGroupType:
				s << "while (" << _stack.pop() << ") {";
				addOutputLine(s.str(), false, true);
				break;
			case kDoWhileCondGroupType:
				s << "} while (" << _stack.pop() << ")";
				addOutputLine(s.str(), true, false);
				break;
			default:
				break;
			}
		}
		break;
	case kJumpInstType:
	case kJumpRelInstType:
		switch (_curGroup->_type) {
		case kBreakGroupType:
			addOutputLine("break;");
			break;
		case kContinueGroupType:
			addOutputLine("continue;");
			break;
		default:
			{
				bool printJump = true;
				OutEdgeRange r = boost::out_edges(_curVertex, _g);
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
					s << boost::format("jump 0x%X;") % _engine->getDestAddress(inst);
					addOutputLine(s.str());
				}
			}
			break;
		}
		break;
	case kReturnInstType:
		// TODO: Allow specification of return value as part of return statement
		addOutputLine("return;");
		break;
	case kSpecialCallInstType:
		{
			_argList.clear();
			bool returnsValue = (inst._codeGenData.find("r") == 0);
			std::string metadata = (!returnsValue ? inst._codeGenData : inst._codeGenData.substr(1));
			for (size_t i = 0; i < metadata.length(); i++)
				processSpecialMetadata(inst, metadata[i], i);
			_stack.push(new CallEntry(inst._name, _argList));
			if (!returnsValue) {
				std::stringstream stream;
				stream << _stack.pop() << ";";
				addOutputLine(stream.str());
			}
			break;
		}
	default:
		{
			std::stringstream s;
			s << boost::format("WARNING: Unknown opcode %X at address %08X") % inst._opcode % inst._address;
			addOutputLine(s.str());
		}
		break;
	}
}

void CodeGenerator::addArg(EntryPtr p) {
	if (_callOrder == kFIFOArgOrder)
		_argList.push_front(p);
	else if (_callOrder == kLIFOArgOrder)
		_argList.push_back(p);
}

void CodeGenerator::processSpecialMetadata(const Instruction &inst, char c, int pos) {
	switch (c) {
	case 'p':
		addArg(_stack.pop());
		break;
	default:
		std::cerr << boost::format("WARNING: Unknown character in metadata: %c\n") % c ;
		break;
	}
}

