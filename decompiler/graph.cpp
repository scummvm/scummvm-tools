#include <algorithm>

#include "graph.h"

using namespace std;


Block *dominatorIntersect(Block *u, Block *v);
std::list<Block*> inPostOrder(std::list<Block*> &blocks);
int orderVisit(Block *u, int number);
bool postOrderCompare(Block *a, Block *b);



ControlFlowGraph::ControlFlowGraph() : _entry() {
}


ControlFlowGraph::~ControlFlowGraph() {
	foreach (Block *u, _blocks)
		delete u;
}


void ControlFlowGraph::addEdge(Block *from, Block *to) {
	from->_out.push_back(to);
	to->_in.push_back(from);
}


void ControlFlowGraph::assignDominators() {
	std::list<Block*> blocks = inPostOrder(_blocks);
	blocks.reverse();
	blocks.remove(_entry);
	_entry->_dominator = _entry;
	for (bool changed = true; changed; ) {
		changed = false;
		foreach (Block *u, blocks) {
			std::list<Block*>::iterator it = u->_in.begin();
			Block *dom = *it++;
			for (; it != u->_in.end(); it++)
				if ((*it)->_dominator)
					dom = dominatorIntersect(*it, dom);
			if (u->_dominator != dom) {
				changed = true;
				u->_dominator = dom;
			}
		}
	}
	_entry->_dominator = 0;
}


// entry node is an interval header
// a node belongs to an interval, if all its immediate predecessors belong the given interval
// otherwise it is an interval header
void ControlFlowGraph::assignIntervals() {
	std::list<Block*> intervals;
	intervals.push_back(_entry);
	foreach (Block *interval, intervals) {
		interval->_interval = interval;
		for (bool added = true; added; ) {
			added = false;
			foreach (Block *m, _blocks) {
				bool allPredInInterval = true;
				foreach (Block *p, m->_in)
					allPredInInterval &= p->_interval == interval;
				if (!m->_interval && allPredInInterval) {
					added = true;
					m->_interval = interval;
				}
			}
		}
		foreach (Block *m, _blocks) {
			bool anyPredInInterval = false;
			foreach (Block *p, m->_in)
				anyPredInInterval |= p->_interval == interval;
			if (!m->_interval && anyPredInInterval)
				intervals.push_back(m);
		}
	}
}


Block *dominatorIntersect(Block *u, Block *v) {
	while (u != v) {
		while (u->_number < v->_number)
			u = u->_dominator;
		while (v->_number < u->_number)
			v = v->_dominator;
	}
	return u;
}


// a derived graph, given set of intervals, is a graph in which
// all intervals have been collapsed to a single node, and edge
// exists between nodes if there are edges crossing corresponding
// intervals in the original graph
void ControlFlowGraph::extendIntervals() {
	ControlFlowGraph d;
	std::map<Block*, Block*> trans;
	foreach (Block *interval, intervals()) {
		trans[interval] = d.addBlock(interval->_instructions.begin(), interval->_instructions.end());
		trans[interval]->_primitive = interval;
	}
	foreach (Block *interval, intervals())
		foreach (Block *u, interval->_in)
		if (u->_interval != interval)
			d.addEdge(trans[u->_interval], trans[interval]);
	d.setEntry(_entry->_instructions.front()->_addr);
	d.assignIntervals();
	foreach (Block *du, d._blocks)
		foreach (Block *v, _blocks)
		if (v->_interval == du->_primitive)
			v->_interval = du->_interval->_primitive;
}


string ControlFlowGraph::graphvizEscapeLabel(const std::string &s) {
	std::string ret;
	foreach (char c, s) {
		if (c == '\n' || c == '"' || c == '\\')
			ret.push_back('\\');
		ret.push_back(c == '\n' ? 'l' : c);   // align lines to the left
	}
	return ret;
}


string ControlFlowGraph::graphvizToString(const std::string &fontname, int fontsize) {
	std::stringstream ret;
	ret << "digraph G {" << std::endl;
	foreach (Block *interval, intervals()) {
		ret << "subgraph " << '"' << "cluster_" << interval << '"' << " {" << std::endl;
		ret << "style=dotted;" << std::endl;
		foreach (Block *u, _blocks)
			if (u->_interval == interval) {
				ret << '"' << u << "\"[";
				if (fontname != "")
					ret << "fontname=" << '"' << fontname << "\",";
				if (fontsize != 0)
					ret << "fontsize=" << fontsize << ",";
				ret	<< "shape=box,label=\"<number=" << u->_number;
				if (u->_dominator)
					ret	<< ", dom=" << u->_dominator->_number;
				if (u->_loopFollow)
					ret << ", loop_type=" << (u->_loopType == PRE_TESTED ? "pre_tested" : u->_loopType == POST_TESTED ? "post_tested" : "endless");
				ret << ">\\n" << graphvizEscapeLabel(u->toString()) << "\"];" << std::endl;
			}
		ret << "}" << std::endl;
	}
	foreach (Block *u, _blocks) {
		bool hadFollow = false;
		foreach (Block *v, u->_out) {
			hadFollow |= v == u->_loopFollow;
		    ret << '"' << u << "\" -> \"" << v << '"' << (v == u->_loopFollow ? "[color=blue]" : "") << ";" << std::endl;
		}
		if (u->_loopFollow && !hadFollow)
		    ret << '"' << u << "\" -> \"" << u->_loopFollow << '"' << "[color=blue,style=dashed];" << std::endl;
		if (u->_ifFollow)
		    ret << '"' << u << "\" -> \"" << u->_ifFollow << '"' << "[color=red,style=dashed];" << std::endl;
	}
	ret << "}" << std::endl;
	return ret.str();
}


void ControlFlowGraph::ifStruct() {
	std::list<Block*> unresolved;
	foreach (Block *u, inPostOrder(_blocks))
		// TODO how will this work with 2-way head and 2-way latch loops - on latch node? how are loops going to be structured anyway
		if (u->_out.size() == 2 && !((u->_loopLatch && u->_loopType == PRE_TESTED) || u->_loopHead)) {
			Block *follow = 0;
			// find the deepest node with immediate dominator u
			foreach (Block *v, _blocks)
				if (v->_dominator == u && v->_in.size() >= 2 && (!follow || v->_number < follow->_number))
					follow = v;
			unresolved.push_back(u);
			if (follow) {
				foreach (Block *v, unresolved)
					v->_ifFollow = follow;
				unresolved.clear();
			}
		}
}


std::list<Block*> inPostOrder(std::list<Block*> &blocks) {
	std::list<Block*> ret(blocks);
	ret.sort(postOrderCompare);
	return ret;
}


list<Block*> ControlFlowGraph::intervals() {
	std::list<Block*> ret;
	assignIntervals();
	foreach (Block *u, _blocks)
		if (u->_interval == u)
			ret.push_back(u);
	return ret;
}


bool ControlFlowGraph::isReducible() {
	for (size_t size = _blocks.size()+1; size > intervals().size(); size = intervals().size(), extendIntervals())
		;
	return intervals().size() == 1;
}


Block *ControlFlowGraph::loopFollow(Block *head, Block *latch) {
	if (head->_loopType == PRE_TESTED)
		return head->outEdgeOutsideLoop(head);
	if (head->_loopType == POST_TESTED)
		return latch->outEdgeOutsideLoop(head);
	// ENDLESS
	Block *ret = 0;
	foreach (Block *u, _blocks)
		if (u->inLoop(head) && u->outEdgeOutsideLoop(head) && (!ret || ret->_number < u->outEdgeOutsideLoop(head)->_number))
			ret = u->outEdgeOutsideLoop(head);
	return ret;
}

// for each set of 'growing' intervals in derived sequence of graphs,
// every interval header is a potential loop header
// we check for back edges that don't belong to loops discovered earlier
// (inner loops)
void ControlFlowGraph::loopStruct() {
	for (size_t size = _blocks.size()+1; size > intervals().size(); size = intervals().size(), extendIntervals())
		foreach (Block *interval, intervals()) {
			foreach (Block *latch, interval->_in) {
				if (latch->_interval == interval && !latch->_loopHead) {
					interval->_loopLatch = latch;
					interval->_loopType = loopType(interval, latch);
					interval->_loopFollow = loopFollow(interval, latch);
					latch->_loopHead = interval;
				}
			}
		}
}

LoopType ControlFlowGraph::loopType(Block *head, Block *latch) {
	if (head->_out.size() == 1 && latch->_out.size() == 1)
		return ENDLESS;
	if (head->_out.size() == 1 && latch->_out.size() == 2)
		return POST_TESTED;
	if (head->_out.size() == 2 && latch->_out.size() == 1)
		return PRE_TESTED;
	// head->_out.size() == 2 && latch->_out.size() == 2
	if (!head->outEdgeOutsideLoop(head))
		return POST_TESTED;
	else
		return PRE_TESTED;
}


void ControlFlowGraph::orderBlocks() {
	assert(_entry);
		orderVisit(_entry, 0);
}


int orderVisit(Block *u, int number) {
	u->_number = -1;
	foreach (Block *v, u->_out)
		if (!v->_number)
			number = orderVisit(v, number);
	u->_number = ++number;
	return number;
}


bool postOrderCompare(Block *a, Block *b) {
	return a->_number < b->_number;
}


void ControlFlowGraph::removeJumpsToJumps() {
	for (bool changed = true; changed; ) {
		changed = false;
		foreach (Block *u, _blocks) {
			foreach (Block *v, u->_out) {
				Jump *jump = dynamic_cast<Jump*>(v->_instructions.front());
				if (jump && !dynamic_cast<CondJump*>(jump) && jump->target() != jump->_addr) {
					changed = true;
					replaceEdges(u, v, _targets[jump->target()]);
				}
			}
		}
	}
}


// there are no paths that lead from entries to an unreachable node
// but unreachable node can still have outgoing edge to the main part
// of the graph, and we need to remove it from predecessor lists
void ControlFlowGraph::removeUnreachableBlocks() {
	foreach (Block *u, _blocks)
		if (!u->_number) {
			foreach (Block *v, u->_out)
				v->_in.remove(u);
		}
	for (list<Block*>::iterator it = _blocks.begin(); it != _blocks.end(); )
		if ((*it)->_number)
			it++;
		else {
			delete *it;
			it = _blocks.erase(it);
		}
}


void ControlFlowGraph::replaceEdges(Block *from, Block *oldTo, Block *newTo) {
	size_t n = count(oldTo->_in.begin(), oldTo->_in.end(), from);
	oldTo->_in.remove(from);
	fill_n(back_inserter(newTo->_in), n, from);
	foreach (Block *&block, from->_out)
		if (block == oldTo)
			block = newTo;
}


void ControlFlowGraph::setEntry(address_t entry) {
	foreach (Block *block, _blocks)
		if (block->_instructions.front()->_addr == entry)
			_entry = block;
}
