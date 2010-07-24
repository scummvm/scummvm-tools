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

#include <algorithm>
#include <iostream>
#include <stack>

#include <boost/format.hpp>

#define PUT(vertex, group) boost::put(boost::vertex_name, _g, vertex, group);
#define PUT_EDGE(edge, isJump) boost::put(boost::edge_attribute, _g, edge, isJump);
#define PUT_ID(vertex, id) boost::put(boost::vertex_index, _g, vertex, id);
#define GET(vertex) (boost::get(boost::vertex_name, _g, vertex))
#define GET_EDGE(edge) (boost::get(boost::edge_attribute, _g, edge))

ControlFlow::ControlFlow(const std::vector<Instruction> &insts, Engine *engine) : _insts(insts) {
	_engine = engine;
	GraphVertex last;
	bool addEdge = false;
	int id = 0;
	GroupPtr prev = NULL;

	for (ConstInstIterator it = insts.begin(); it != insts.end(); ++it) {
		GraphVertex cur = boost::add_vertex(_g);
		_addrMap[it->_address] = cur;
		PUT(cur, new Group(cur, it, it, prev));
		PUT_ID(cur, id);
		id++;

		if (addEdge) {
			GraphEdge e = boost::add_edge(last, cur, _g).first;
			PUT_EDGE(e, false);
		}
		last = cur;
		addEdge = (it->_type != kJump && it->_type != kJumpRel);
		prev = GET(cur);
	}

	for (ConstInstIterator it = insts.begin(); it != insts.end(); ++it) {
		switch(it->_type) {
		case kJump:
		case kCondJump:
		case kJumpRel:
		case kCondJumpRel: {
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

void ControlFlow::setStackLevel(GraphVertex g, int level) {
	GroupPtr gr = GET(g);
	if (gr->_stackLevel != -1) {
		if (gr->_stackLevel != level)
			std::cerr << boost::format("WARNING: Inconsistency in expected stack level for instruction at address 0x%08x (current: %d, requested: %d)\n") % gr->_start->_address % gr->_stackLevel % level;
		return;
	}
	gr->_stackLevel = level;
	PUT(g, gr)

	if (boost::out_degree(g, _g) == 0)
		return;

	OutEdgeRange r = boost::out_edges(g, _g);
	for (OutEdgeIterator e = r.first; e != r.second; ++e) {
		setStackLevel(boost::target(*e, _g), level + gr->_start->_stackChange);
	}
}

void ControlFlow::createGroups() {
	if (GET(find(_insts.begin()))->_stackLevel != -1)
		return;
	setStackLevel(find(_insts.begin()), 0);
	ConstInstIterator curInst, nextInst;
	nextInst = _insts.begin();
	nextInst++;
	int stackLevel = 0;
	int expectedStackLevel = 0;
	std::stack<uint32> s;
	for (curInst = _insts.begin(); nextInst != _insts.end(); ++curInst, ++nextInst) {
		GraphVertex cur = find(curInst);
		GraphVertex next = find(nextInst);

		if (in_degree(cur, _g) == 0 && out_degree(cur, _g) == 0)
			continue;
		GroupPtr grCur = GET(cur);
		GroupPtr grNext = GET(next);
		expectedStackLevel = grCur->_stackLevel;

		if (expectedStackLevel > grNext->_stackLevel)
			expectedStackLevel = grNext->_stackLevel;

		stackLevel += curInst->_stackChange;

		// Group ends after a jump
		if (curInst->_type == kJump || curInst->_type == kJumpRel || curInst->_type == kCondJump || curInst->_type == kCondJumpRel) {
			stackLevel = grNext->_stackLevel;
			continue;
		}

		// Group ends before target of a jump
		if (in_degree(next, _g) != 1) {
			stackLevel = grNext->_stackLevel;
			continue;
		}

		// Group ends when stack is balanced, unless just before conditional jump
		if (stackLevel == expectedStackLevel && nextInst->_type != kCondJump && nextInst->_type != kCondJumpRel) {
			continue;
		}

		// All checks passed, merge groups
		merge(cur, next);
	}

	detectShortCircuit();
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
	return _g;
}

void ControlFlow::detectWhile() {
	VertexRange vr = boost::vertices(_g);
	for (VertexIterator v = vr.first; v != vr.second; ++v) {
		GroupPtr gr = GET(*v);
		// Undetermined block that ends with conditional jump
		if (out_degree(*v, _g) == 2 && gr->_type == kNormal) {
			InEdgeRange ier = boost::in_edges(*v, _g);
			bool isWhile = false;
			for (InEdgeIterator e = ier.first; e != ier.second; ++e) {
				GroupPtr sourceGr = GET(boost::source(*e, _g));
				// Block has ingoing edge from block later in the code that isn't a do-while condition
				if (sourceGr->_start->_address > gr->_start->_address && sourceGr->_type != kDoWhileCond)
					isWhile = true;
			}
			if (isWhile)
				gr->_type = kWhileCond;
		}
	}
}

void ControlFlow::detectDoWhile() {
	VertexRange vr = boost::vertices(_g);
	for (VertexIterator v = vr.first; v != vr.second; ++v) {
		GroupPtr gr = GET(*v);
		// Undetermined block that ends with conditional jump...
		if (out_degree(*v, _g) == 2 && gr->_type == kNormal) {
			OutEdgeRange oer = boost::out_edges(*v, _g);
			for (OutEdgeIterator e = oer.first; e != oer.second; ++e) {
				GroupPtr targetGr = GET(boost::target(*e, _g));
				// ...to earlier in code
				if (targetGr->_start->_address < gr->_start->_address)
					gr->_type = kDoWhileCond;
			}
		}
	}
}

void ControlFlow::detectBreak() {
	VertexRange vr = boost::vertices(_g);
	for (VertexIterator v = vr.first; v != vr.second; ++v) {
		GroupPtr gr = GET(*v);
		// Undetermined block with unconditional jump...
		if (gr->_type == kNormal && (gr->_end->_type == kJump || gr->_end->_type == kJumpRel) && out_degree(*v, _g) == 1) {
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
				if ((targetGr->_prev == sourceGr && sourceGr->_type == kDoWhileCond) || sourceGr->_type == kWhileCond) {
					if (validateBreakOrContinue(gr, sourceGr))
						gr->_type = kBreak;
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
		if (gr->_type == kNormal && (gr->_end->_type == kJump || gr->_end->_type == kJumpRel) && out_degree(*v, _g) == 1) {
			OutEdgeIterator oe = boost::out_edges(*v, _g).first;
			GraphVertex target = boost::target(*oe, _g);
			GroupPtr targetGr = GET(target);
			// ...to a while or do-while condition...
			if (targetGr->_type == kWhileCond || targetGr->_type == kDoWhileCond) {
				bool isContinue = true;
				// ...unless it is targeting a while condition...
				if (targetGr->_type == kWhileCond) {
					OutEdgeRange toer = boost::out_edges(target, _g);
					for (OutEdgeIterator toe = toer.first; toe != toer.second; ++toe) {
						// ...which jumps to the next sequential group
						if (GET(boost::target(*toe, _g)) == gr->_next)
							isContinue = false;
					}
				}
	
				if (isContinue && validateBreakOrContinue(gr, targetGr))
					gr->_type = kContinue;
			}
		}
	}
}

bool ControlFlow::validateBreakOrContinue(GroupPtr gr, GroupPtr condGr) {
	GroupPtr from, to, cursor;

	if (condGr->_type == kDoWhileCond) {
		to = condGr;
		from = gr;
	}	else {
		to = gr;
		from = condGr->_next;
	}

	GroupType ogt = (condGr->_type == kDoWhileCond ? kWhileCond : kDoWhileCond);
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
		if (gr->_type == kNormal && out_degree(*v, _g) == 2) {
			gr->_type = kIfCond;

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
			if (targetGr->_prev->_end->_type != kJump && targetGr->_prev->_end->_type != kJumpRel)
				continue;
			// ...which is not a break or a continue...
			if (targetGr->_prev->_type == kContinue || targetGr->_prev->_type == kBreak)
				continue;
			// ...to later in the code
			OutEdgeIterator toe = boost::out_edges(find(targetGr->_prev->_start->_address), _g).first;
			GroupPtr targetTargetGr = GET(boost::target(*toe, _g));
			if (targetTargetGr->_start->_address > targetGr->_end->_address) {
				targetGr->_startElse = true;
				targetTargetGr->_prev->_endElse = targetGr.get();
			}
		}
	}
}
