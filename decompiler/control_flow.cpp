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
#include "stack.h"

#include <algorithm>
#include <iostream>
#include <set>

#include <boost/format.hpp>

#define PUT(vertex, group) boost::put(boost::vertex_name, _g, vertex, group);
#define PUT_EDGE(edge, isJump) boost::put(boost::edge_attribute, _g, edge, isJump);
#define PUT_ID(vertex, id) boost::put(boost::vertex_index, _g, vertex, id);
#define GET(vertex) (boost::get(boost::vertex_name, _g, vertex))
#define GET_EDGE(edge) (boost::get(boost::edge_attribute, _g, edge))

ControlFlow::ControlFlow(const InstVec &insts, Engine *engine) : _insts(insts) {
	_engine = engine;

	// Automatically add a function if we're not supposed to look for more functions and no functions are defined
	// This avoids a special case for when no real functions exist in the script
	if (engine->_functions.empty() && !_engine->detectMoreFuncs())
		engine->_functions[insts.begin()->_address] = Function(insts.begin(), insts.end());

	GroupPtr prev = NULL;
	int id = 0;
	// Create vertices
	for (ConstInstIterator it = insts.begin(); it != insts.end(); ++it) {
		GraphVertex cur = boost::add_vertex(_g);
		_addrMap[it->_address] = cur;
		PUT(cur, new Group(cur, it, it, prev));
		PUT_ID(cur, id);
		id++;

		// Add reference to vertex if function starts here
		if (_engine->_functions.find(it->_address) != _engine->_functions.end())
			_engine->_functions[it->_address]._v = cur;

		prev = GET(cur);
	}

	// Add regular edges
	FuncMap::iterator fn;
	GraphVertex last;
	bool addEdge = false;
	prev = NULL;
	for (ConstInstIterator it = insts.begin(); it != insts.end(); ++it) {
		if (_engine->_functions.find(it->_address) != _engine->_functions.end()) {
			addEdge = false;
		}

		GraphVertex cur = find(it);
		if (addEdge) {
			GraphEdge e = boost::add_edge(last, cur, _g).first;
			PUT_EDGE(e, false);
		}

		last = cur;
		addEdge = (it->_type != kJumpInstType && it->_type != kJumpRelInstType && it->_type != kReturnInstType);
		prev = GET(cur);

	}

	// Add jump edges
	for (ConstInstIterator it = insts.begin(); it != insts.end(); ++it) {
		switch(it->_type) {
		case kJumpInstType:
		case kCondJumpInstType:
		case kJumpRelInstType:
		case kCondJumpRelInstType: {
			GraphEdge e = boost::add_edge(find(it), find(_engine->getDestAddress(it)), _g).first;
			PUT_EDGE(e, true);
			break;
			}
		default:
			break;
		}
	}
}

GraphVertex ControlFlow::find(const Instruction &inst) {
	return _addrMap[inst._address];
}

GraphVertex ControlFlow::find(ConstInstIterator it) {
	return _addrMap[it->_address];
}

GraphVertex ControlFlow::find(uint32 address) {
	std::map<uint32, GraphVertex>::iterator it = _addrMap.find(address);
	if (it == _addrMap.end())
		std::cerr << "Request for instruction at unknown address" << boost::format("0x%08x") % address;
	return it->second;
}

void ControlFlow::merge(GraphVertex g1, GraphVertex g2) {
	// Update property
	GroupPtr gr1 = GET(g1);
	GroupPtr gr2 = GET(g2);
	gr1->_end = gr2->_end;
	PUT(g1, gr1);

	// Update address map
	ConstInstIterator it = gr2->_start;
	do {
		_addrMap[it->_address] = g1;
		++it;
	} while (gr2->_start != gr2->_end && it != gr2->_end);

	// Add outgoing edges from g2
	OutEdgeRange r = boost::out_edges(g2, _g);
	for (OutEdgeIterator e = r.first; e != r.second; ++e) {
		GraphEdge newE = boost::add_edge(g1, boost::target(*e, _g), _g).first;
		PUT_EDGE(newE, GET_EDGE(*e));
	}

	// Update _next pointer
	gr1->_next = gr2->_next;
	if (gr2->_next != NULL)
		gr2->_next->_prev = gr2->_prev;

	// Remove edges to/from g2
	boost::clear_vertex(g2, _g);
	// Remove vertex
	boost::remove_vertex(g2, _g);
}

typedef std::pair<GraphVertex, int> LevelEntry;

void ControlFlow::setStackLevel(GraphVertex g, int level) {
	Stack<LevelEntry> levelStack;
	std::set<GraphVertex> seen;
	levelStack.push(LevelEntry(g, level));
	seen.insert(g);
	while (!levelStack.empty()) {
		LevelEntry e = levelStack.pop();
		GroupPtr gr = GET(e.first);
		if (gr->_stackLevel != -1) {
			if (gr->_stackLevel != e.second)
				std::cerr << boost::format("WARNING: Inconsistency in expected stack level for instruction at address 0x%08x (current: %d, requested: %d)\n") % gr->_start->_address % gr->_stackLevel % e.second;
			continue;
		}
		gr->_stackLevel = e.second;

		OutEdgeRange r = boost::out_edges(e.first, _g);
		for (OutEdgeIterator oe = r.first; oe != r.second; ++oe) {
			GraphVertex target = boost::target(*oe, _g);
			if (seen.find(target) == seen.end()) {
				levelStack.push(LevelEntry(target, e.second + gr->_start->_stackChange));
				seen.insert(target);
			}
		}
	}
}

void ControlFlow::detectFunctions() {
	uint32 nextFunc = 0;
	for (ConstInstIterator it = _insts.begin(); it != _insts.end(); ++it) {
		GraphVertex v = find(it);
		GroupPtr gr = GET(v);

		if (it->_address < nextFunc)
			continue;

		bool functionExists = false;
		bool detectEndPoint = false;
		for (FuncMap::iterator fn = _engine->_functions.begin(); fn != _engine->_functions.end(); ++fn) {
			if (fn->first == it->_address) {
				if (fn->second._endIt == _insts.end()) {
					return;
				}
				if (fn->second._startIt == fn->second._endIt) {
					// We already know this is an entry point, we only need to detect the end point
					detectEndPoint = true;
					break;
				}
				nextFunc = fn->second._endIt->_address;
				functionExists = true;
			}
		}

		if (functionExists)
			continue;

		bool isEntryPoint = true;
		if (!detectEndPoint) {
			InEdgeRange ier = boost::in_edges(v, _g);
			for (InEdgeIterator e = ier.first; e != ier.second; ++e) {
				// If an ingoing edge exists from earlier in the code, this is not a function entry point
				if (GET(boost::source(*e, _g))->_start->_address < gr->_start->_address)
					isEntryPoint = false;
			}
		}

		if (isEntryPoint) {
			// Detect end point
			Stack<GraphVertex> stack;
			std::set<GraphVertex> seen;
			stack.push(v);
			GroupPtr endPoint = gr;
			while (!stack.empty()) {
				v = stack.pop();
				GroupPtr tmp = GET(v);
				if (tmp->_start->_address > endPoint->_start->_address)
					endPoint = tmp;
				OutEdgeRange r = boost::out_edges(v, _g);
				for (OutEdgeIterator i = r.first; i != r.second; ++i) {
					GraphVertex target = boost::target(*i, _g);
					if (seen.find(target) == seen.end()) {
						stack.push(target);
						seen.insert(target);
					}
				}
			}

			ConstInstIterator endInst;
			if (endPoint->_next) {
				endInst = endPoint->_next->_start;
				nextFunc = endInst->_address;
			} else {
				endInst = _insts.end();
			}
			Function f;
			if (detectEndPoint) {
				f = _engine->_functions[gr->_start->_address];
				f._endIt = endInst;
			} else {
				f = Function(gr->_start, endInst);
				f._name = "auto_";
			}
			f._v = find(it);
			_engine->_functions[gr->_start->_address] = f;
			if (!endPoint->_next)
				return;
		}
	}
}

void ControlFlow::createGroups() {
	if (!_engine->_functions.empty() && GET(_engine->_functions.begin()->second._v)->_stackLevel != -1)
		return;

	// Detect more functions
	if (_engine->detectMoreFuncs())
		detectFunctions();

	for (FuncMap::iterator fn = _engine->_functions.begin(); fn != _engine->_functions.end(); ++fn)
		setStackLevel(fn->second._v, 0);
	ConstInstIterator curInst, nextInst;
	nextInst = _insts.begin();
	nextInst++;
	int stackLevel = 0;
	int expectedStackLevel = 0;
	for (curInst = _insts.begin(); nextInst != _insts.end(); ++curInst, ++nextInst) {
		GraphVertex cur = find(curInst);
		GraphVertex next = find(nextInst);

		GroupPtr grCur = GET(cur);
		GroupPtr grNext = GET(next);

		// Don't process unreachable code
		if (grCur->_stackLevel < 0) {
			stackLevel = grNext->_stackLevel;
			continue;
		}

		expectedStackLevel = grCur->_stackLevel;
		// If expected stack level decreases in next vertex, then use next vertex level as expected level
		if (expectedStackLevel > grNext->_stackLevel && grNext->_stackLevel >= 0) {
			expectedStackLevel = grNext->_stackLevel;
			// Also set the stack level of the current group to remember that we expect it to be lower
			grCur->_stackLevel = expectedStackLevel;
		}

		stackLevel += curInst->_stackChange;

		// For stack operations, the new stack level becomes the expected stack level starting from the next group
		if (curInst->_type == kStackInstType) {
			expectedStackLevel = stackLevel;
			grNext->_stackLevel = stackLevel;
		}

		// Group ends after a jump
		if (curInst->_type == kJumpInstType || curInst->_type == kJumpRelInstType || curInst->_type == kCondJumpInstType || curInst->_type == kCondJumpRelInstType) {
			stackLevel = grNext->_stackLevel;
			continue;
		}

		// Group ends with a return
		if (curInst->_type == kReturnInstType) {
			stackLevel = grNext->_stackLevel;
			continue;
		}

		// Group ends before target of a jump
		if (in_degree(next, _g) != 1) {
			stackLevel = grNext->_stackLevel;
			continue;
		}

		// If group has no instructions with stack effect >= 0, don't merge on balanced stack
		bool forceMerge = true;
		ConstInstIterator it = grCur->_start;
		do {
			if (it->_stackChange >= 0)
				forceMerge = false;
			++it;
		} while (grCur->_start != grCur->_end && it != grCur->_end);

		// Group ends when stack is balanced, unless just before conditional jump
		if (stackLevel == expectedStackLevel && !forceMerge && nextInst->_type != kCondJumpInstType && nextInst->_type != kCondJumpRelInstType) {
			continue;
		}

		// All checks passed, merge groups
		merge(cur, next);
	}

	// FIXME: The short-circuit detection is disabled because short-circuited groups require some special handling
	// in the code generation. It's not entirely clear how to handle it properly, though: you need to deduce which
	// effect is created by the conditional jumps in the middle of a block, which seems to get fairly complex when
	// you have multiple groups that are merged by the short-circuit detection.
	//detectShortCircuit();
}

void ControlFlow::detectShortCircuit() {
	ConstInstIterator lastInst = _insts.end();
	--lastInst;
	GraphVertex cur = find(lastInst);
	GroupPtr gr = GET(cur);
	while (gr->_prev != NULL) {
		bool doMerge = false;
		cur = find(gr->_start);
		GraphVertex prev = find(gr->_prev->_start);
		// Block is candidate for short-circuit merging if it and the preceding block both end with conditional jumps
		if (out_degree(cur, _g) == 2 && out_degree(prev, _g) == 2) {
			doMerge = true;
			OutEdgeRange rCur = boost::out_edges(cur, _g);
			std::vector<GraphVertex> succs;

			// Find possible target vertices
			for (OutEdgeIterator it = rCur.first; it != rCur.second; ++it) {
				succs.push_back(boost::target(*it, _g));
			}

			// Check if vertex would add new targets - if yes, don't merge
			OutEdgeRange rPrev = boost::out_edges(prev, _g);
			for (OutEdgeIterator it = rPrev.first; it != rPrev.second; ++it) {
				GraphVertex target = boost::target(*it, _g);
				doMerge &= (std::find(succs.begin(), succs.end(), target) != succs.end() || target == cur);
			}

			if (doMerge) {
				gr = gr->_prev;
				merge(prev, cur);
				continue;
			}
		}
		gr = gr->_prev;
	}
}

const Graph &ControlFlow::analyze() {
	detectDoWhile();
	detectWhile();
	detectBreak();
	detectContinue();
	detectIf();
	detectElse();
	return _g;
}

void ControlFlow::detectWhile() {
	VertexRange vr = boost::vertices(_g);
	for (VertexIterator v = vr.first; v != vr.second; ++v) {
		GroupPtr gr = GET(*v);
		// Undetermined block that ends with conditional jump
		if (out_degree(*v, _g) == 2 && gr->_type == kNormalGroupType) {
			InEdgeRange ier = boost::in_edges(*v, _g);
			bool isWhile = false;
			for (InEdgeIterator e = ier.first; e != ier.second; ++e) {
				GroupPtr sourceGr = GET(boost::source(*e, _g));
				// Block has ingoing edge from block later in the code that isn't a do-while condition
				if (sourceGr->_start->_address > gr->_start->_address && sourceGr->_type != kDoWhileCondGroupType)
					isWhile = true;
			}
			if (isWhile)
				gr->_type = kWhileCondGroupType;
		}
	}
}

void ControlFlow::detectDoWhile() {
	VertexRange vr = boost::vertices(_g);
	for (VertexIterator v = vr.first; v != vr.second; ++v) {
		GroupPtr gr = GET(*v);
		// Undetermined block that ends with conditional jump...
		if (out_degree(*v, _g) == 2 && gr->_type == kNormalGroupType) {
			OutEdgeRange oer = boost::out_edges(*v, _g);
			for (OutEdgeIterator e = oer.first; e != oer.second; ++e) {
				GroupPtr targetGr = GET(boost::target(*e, _g));
				// ...to earlier in code
				if (targetGr->_start->_address < gr->_start->_address)
					gr->_type = kDoWhileCondGroupType;
			}
		}
	}
}

void ControlFlow::detectBreak() {
	VertexRange vr = boost::vertices(_g);
	for (VertexIterator v = vr.first; v != vr.second; ++v) {
		GroupPtr gr = GET(*v);
		// Undetermined block with unconditional jump...
		if (gr->_type == kNormalGroupType && (gr->_end->_type == kJumpInstType || gr->_end->_type == kJumpRelInstType) && out_degree(*v, _g) == 1) {
			OutEdgeIterator oe = boost::out_edges(*v, _g).first;
			GraphVertex target = boost::target(*oe, _g);
			GroupPtr targetGr = GET(target);
			// ...to somewhere later in the code...
			if (gr->_start->_address >= targetGr->_start->_address)
				continue;
			InEdgeRange ier = boost::in_edges(target, _g);
			for (InEdgeIterator ie = ier.first; ie != ier.second; ++ie) {
				GroupPtr sourceGr = GET(boost::source(*ie, _g));
				// ...to block immediately after a do-while condition, or to jump target of a while condition
				if ((targetGr->_prev == sourceGr && sourceGr->_type == kDoWhileCondGroupType) || sourceGr->_type == kWhileCondGroupType) {
					if (validateBreakOrContinue(gr, sourceGr))
						gr->_type = kBreakGroupType;
				}
			}
		}
	}
}

void ControlFlow::detectContinue() {
	VertexRange vr = boost::vertices(_g);
	for (VertexIterator v = vr.first; v != vr.second; ++v) {
		GroupPtr gr = GET(*v);
		// Undetermined block with unconditional jump...
		if (gr->_type == kNormalGroupType && (gr->_end->_type == kJumpInstType || gr->_end->_type == kJumpRelInstType) && out_degree(*v, _g) == 1) {
			OutEdgeIterator oe = boost::out_edges(*v, _g).first;
			GraphVertex target = boost::target(*oe, _g);
			GroupPtr targetGr = GET(target);
			// ...to a while or do-while condition...
			if (targetGr->_type == kWhileCondGroupType || targetGr->_type == kDoWhileCondGroupType) {
				bool isContinue = true;
				// ...unless...
				OutEdgeRange toer = boost::out_edges(target, _g);
				bool afterJumpTargets = true;
				for (OutEdgeIterator toe = toer.first; toe != toer.second; ++toe) {
					// ...it is targeting a while condition which jumps to the next sequential group
					if (targetGr->_type == kWhileCondGroupType && GET(boost::target(*toe, _g)) == gr->_next)
						isContinue = false;
					// ...or the instruction is placed after all jump targets from condition
					if (GET(boost::target(*toe, _g))->_start->_address > gr->_start->_address)
						afterJumpTargets = false;
				}
				if (afterJumpTargets)
					isContinue = false;

				if (isContinue && validateBreakOrContinue(gr, targetGr))
					gr->_type = kContinueGroupType;
			}
		}
	}
}

bool ControlFlow::validateBreakOrContinue(GroupPtr gr, GroupPtr condGr) {
	GroupPtr from, to, cursor;

	if (condGr->_type == kDoWhileCondGroupType) {
		to = condGr;
		from = gr;
	}	else {
		to = gr;
		from = condGr->_next;
	}

	GroupType ogt = (condGr->_type == kDoWhileCondGroupType ? kWhileCondGroupType : kDoWhileCondGroupType);
	// Verify that destination deals with innermost while/do-while
	for (cursor = from; cursor->_next != NULL && cursor != to; cursor = cursor->_next) {
		if (cursor->_type == condGr->_type) {
			OutEdgeRange oerValidate = boost::out_edges(find(cursor->_start), _g);
			for (OutEdgeIterator oeValidate = oerValidate.first; oeValidate != oerValidate.second; ++oeValidate) {
				GraphVertex vValidate = boost::target(*oeValidate, _g);
				GroupPtr gValidate = GET(vValidate);
				// For all other loops of same type found in range, all targets must fall within that range
				if (gValidate->_start->_address < from->_start->_address || gValidate->_start->_address > to->_start->_address )
					return false;

				InEdgeRange ierValidate = boost::in_edges(vValidate, _g);
				for (InEdgeIterator ieValidate = ierValidate.first; ieValidate != ierValidate.second; ++ieValidate) {
					GroupPtr igValidate = GET(boost::source(*ieValidate, _g));
					// All loops of other type going into range must be placed within range
					if (igValidate->_type == ogt && (igValidate->_start->_address < from->_start->_address || igValidate->_start->_address > to->_start->_address ))
					return false;
				}
			}
		}
	}
	return true;
}

void ControlFlow::detectIf() {
	VertexRange vr = boost::vertices(_g);
	for (VertexIterator v = vr.first; v != vr.second; ++v) {
		GroupPtr gr = GET(*v);
		// if: Undetermined block with conditional jump
		if (gr->_type == kNormalGroupType && (gr->_end->_type == kCondJumpInstType || gr->_end->_type == kCondJumpRelInstType)) {
			gr->_type = kIfCondGroupType;
		}
	}
}

void ControlFlow::detectElse() {
	VertexRange vr = boost::vertices(_g);
	for (VertexIterator v = vr.first; v != vr.second; ++v) {
		GroupPtr gr = GET(*v);
		if (gr->_type == kIfCondGroupType) {
			OutEdgeRange oer = boost::out_edges(*v, _g);
			GraphVertex target;
			uint32 maxAddress = 0;
			GroupPtr targetGr;
			// Find jump target
			for (OutEdgeIterator oe = oer.first; oe != oer.second; ++oe) {
				targetGr = GET(boost::target(*oe, _g));
				if (targetGr->_start->_address > maxAddress) {
					target = boost::target(*oe, _g);
					maxAddress = targetGr->_start->_address;
				}
			}
			targetGr = GET(target);
			// else: Jump target of if immediately preceded by an unconditional jump...
			if (targetGr->_prev->_end->_type != kJumpInstType && targetGr->_prev->_end->_type != kJumpRelInstType)
				continue;
			// ...which is not a break or a continue...
			if (targetGr->_prev->_type == kContinueGroupType || targetGr->_prev->_type == kBreakGroupType)
				continue;
			// ...to later in the code
			OutEdgeIterator toe = boost::out_edges(find(targetGr->_prev->_start->_address), _g).first;
			GroupPtr targetTargetGr = GET(boost::target(*toe, _g));
			if (targetTargetGr->_start->_address > targetGr->_end->_address) {
				if (validateElseBlock(gr, targetGr, targetTargetGr)) {
					targetGr->_startElse = true;
					targetTargetGr->_prev->_endElse.push_back(targetGr.get());
				}
			}
		}
	}
}

bool ControlFlow::validateElseBlock(GroupPtr ifGroup, GroupPtr start, GroupPtr end) {
	for (GroupPtr cursor = start; cursor != end; cursor = cursor->_next) {
		if (cursor->_type == kIfCondGroupType || cursor->_type == kWhileCondGroupType || cursor->_type == kDoWhileCondGroupType) {
			// Validate outgoing edges of conditions
			OutEdgeRange oer = boost::out_edges(find(cursor->_start), _g);
			for (OutEdgeIterator oe = oer.first; oe != oer.second; ++oe) {
				GraphVertex target = boost::target(*oe, _g);
				GroupPtr targetGr = GET(target);
				// Each edge from condition must not leave the range [start, end]
				if (start->_start->_address > targetGr->_start->_address || targetGr->_start->_address > end->_start->_address)
					return false;
			}
		}

		// If previous group ends an else, that else must start inside the range
		for (ElseEndIterator it = cursor->_prev->_endElse.begin(); it != cursor->_prev->_endElse.end(); ++it)
		{
			if ((*it)->_start->_address < start->_start->_address)
				return false;
		}

		// Unless group is a simple unconditional jump...
		if (cursor->_start->_type == kJumpInstType || cursor->_start->_type == kJumpRelInstType)
			continue;

		// ...validate ingoing edges
		InEdgeRange ier = boost::in_edges(find(cursor->_start), _g);
		for (InEdgeIterator ie = ier.first; ie != ier.second; ++ie) {
			GraphVertex source = boost::source(*ie, _g);
			GroupPtr sourceGr = GET(source);

			// Edges going to conditions...
			if (sourceGr->_type == kIfCondGroupType || sourceGr->_type == kWhileCondGroupType || sourceGr->_type == kDoWhileCondGroupType) {
				// ...must not come from outside the range [start, end]...
				if (start->_start->_address > sourceGr->_start->_address || sourceGr->_start->_address > end->_start->_address) {
					// ...unless source is simple unconditional jump...
					if (sourceGr->_start->_type == kJumpInstType || sourceGr->_start->_type == kJumpRelInstType)
						continue;
					// ...or the edge is from the if condition associated with this else
					if (ifGroup == sourceGr)
						continue;
					return false;
				}
			}
		}
	}
	return true;
}
