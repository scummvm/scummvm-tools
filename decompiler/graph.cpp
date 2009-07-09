#include "graph.h"

#include <algorithm>

#include <boost/foreach.hpp>
#include <boost/utility.hpp>

using namespace boost;
using namespace std;

#ifndef foreach
#define foreach BOOST_FOREACH
#endif


void componentVisit(Block *u, Block *head) {
	if (u->_component)
		return;
	u->_component = head;
	foreach (Block *v, u->_in)
		componentVisit(v, head);
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


bool postOrderCompare(Block *a, Block *b) {
	return a->_number < b->_number;
}

list<Block*> inPostOrder(list<Block*> &blocks) {
	list<Block*> ret(blocks);
	ret.sort(postOrderCompare);
	return ret;
}


int orderVisit(Block *u, int number) {
	u->_number = -1;
	foreach (Block *v, u->_out)
		if (!v->_number)
			number = orderVisit(v, number);
	u->_number = ++number;
	return number;
}



ControlFlowGraph::ControlFlowGraph() : _entry() {
}


ControlFlowGraph::~ControlFlowGraph() {
	foreach (Block *u, _blocks)
		delete u;
}


Block *ControlFlowGraph::addBlock(list<Instruction*>::iterator first, list<Instruction*>::iterator last) {
	Block* block = new Block;
	_blocks.push_back(block);
	copy(first, last, back_inserter(block->_instructions));
	return block;
}


void ControlFlowGraph::addBlocksFromScript(list<Instruction*>::iterator scriptBegin, list<Instruction*>::iterator scriptEnd) {
	Jump *jump;
	for (list<Instruction*>::iterator it = scriptBegin; it != scriptEnd; it++)
		if ((jump = dynamic_cast<Jump*>(*it))) {
			_targets[jump->target()] = 0;
			if (next(it) != scriptEnd)
				_targets[(*next(it))->_addr] = 0;
		}
	list<Instruction*>::iterator first = scriptBegin;
	for (list<Instruction*>::iterator last = scriptBegin; last != scriptEnd; last++) {
		if (next(last) == scriptEnd || contains(_targets, (*next(last))->_addr)) {
			_targets[(*first)->_addr] = addBlock(first, next(last));
			first = next(last);
		}
	}
	foreach (Block *block, _blocks) {
		if ((jump = dynamic_cast<Jump*>(block->_instructions.back())))
			addEdge(block, _targets[jump->target()]);
		map<address_t, Block*>::iterator succ = next(_targets.find(block->_instructions.front()->_addr));
		if (succ != _targets.end() && (!jump || dynamic_cast<CondJump*>(jump)))
			addEdge(block, succ->second);
	}
}


void ControlFlowGraph::addEdge(Block *from, Block *to) {
	from->_out.push_back(to);
	to->_in.push_back(from);
}


void ControlFlowGraph::assignComponents() {
	orderBlocks();
	list<Block*> blocks = inPostOrder(_blocks);
	blocks.reverse();
	foreach (Block *u, blocks)
		componentVisit(u, u);
}


void ControlFlowGraph::assignDominators() {
	list<Block*> blocks = inPostOrder(_blocks);
	blocks.reverse();
	blocks.remove(_entry);
	_entry->_dominator = _entry;
	for (bool changed = true; changed; ) {
		changed = false;
		foreach (Block *u, blocks) {
			list<Block*>::iterator it = u->_in.begin();
			while (!(*it)->_dominator)
				it++;
			Block *dom = *it++; // first processed predecessor
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
	list<Block*> intervals;
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


// a derived graph, given set of intervals, is a graph in which
// all intervals have been collapsed to a single node, and edge
// exists between nodes if there are edges crossing corresponding
// intervals in the original graph
void ControlFlowGraph::extendIntervals() {
	ControlFlowGraph d;
	map<Block*, Block*> trans;
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


string graphvizEscapeLabel(const string &s) {
	string ret;
	foreach (char c, s) {
		if (c == '\n' || c == '"' || c == '\\')
			ret.push_back('\\');
		ret.push_back(c == '\n' ? 'l' : c);   // align lines to the left
	}
	return ret;
}

list<Block*> ControlFlowGraph::components() {
	list<Block*> ret;
	assignComponents();
	foreach (Block *u, _blocks)
		if (u->_component == u)
			ret.push_back(u);
	return ret;
}

string ControlFlowGraph::graphvizToString(const string &fontname, int fontsize) {
	stringstream ret;
	ret << "digraph G {" << endl;
	foreach (Block *interval, intervals()) {
		ret << "subgraph " << '"' << "cluster_" << interval << '"' << " {" << endl;
		ret << "style=dotted;" << endl;
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
				ret << ">\\n" << graphvizEscapeLabel(u->toString()) << "\"];" << endl;
			}
		ret << "}" << endl;
	}
	foreach (Block *u, _blocks)
		foreach (Block *v, u->_out)
		    ret << '"' << u << "\" -> \"" << v << '"' << ";" << endl;
	ret << "}" << endl;
	return ret.str();
}


list<Block*> ControlFlowGraph::intervals() {
	list<Block*> ret;
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


void ControlFlowGraph::orderBlocks() {
	assert(_entry);
	if (!_entry->_number)
		orderVisit(_entry, 0);
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


void ControlFlowGraph::removeUnreachableBlocks() {
	foreach (Block *u, _blocks)
		if (!u->_number) {
			foreach (Block *v, u->_out)
				v->_in.remove(u);
			foreach (Block *v, u->_in)
				v->_out.remove(u);
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
