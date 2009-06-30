#ifndef GRAPH_H
#define GRAPH_H

#include "misc.h"
#include <cassert>
#include <list>
#include <map>
#include <set>
#include <sstream>
#include <iostream>


enum LoopType {
	PRE_TESTED,
	POST_TESTED,
	ENDLESS
};


struct Block : boost::noncopyable {

	Block *_interval;
	Block *_loopFollow;
	Block *_loopHead;
	Block *_loopLatch;
	Block *_primitive;
	LoopType _loopType;
	int _number;
	list<Block*> _in;
	list<Block*> _out;
	list<Instruction*> _instructions;

	string toString() {
		ostringstream ret;
		foreach (Instruction *instruction, _instructions)
			ret << instruction->toString();
		return ret.str();
	}

	Block() : _interval(), _number(), _loopHead(), _loopFollow() {
	}

	~Block() {
	}
};


struct ControlFlowGraph : boost::noncopyable {

	Block *_entry;
	std::list<Block*> _blocks;
	map<address_t, Block*> _targets;

	ControlFlowGraph() : _entry() {
	}

	~ControlFlowGraph() {
		foreach (Block *u, _blocks)
			delete u;
	}

	template<typename Iterator>
	void addBlocksFromScript(Iterator scriptBegin, Iterator scriptEnd) {
		Jump *jump;
		for (Iterator it = scriptBegin; it != scriptEnd; it++)
			if ((jump = dynamic_cast<Jump*>(*it))) {
				_targets[jump->target()] = 0;
				if (next(it) != scriptEnd)
					_targets[(*next(it))->_addr] = 0;
			}
		Iterator first = scriptBegin;
		for (Iterator last = scriptBegin; last != scriptEnd; last++) {
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

	void setEntry(address_t entry) {
		foreach (Block *block, _blocks)
			if (block->_instructions.front()->_addr == entry)
				_entry = block;
	}

	template<typename Iterator>
	Block *addBlock(Iterator first, Iterator last) {
		Block* block = new Block;
		_blocks.push_back(block);
		copy(first, last, back_inserter(block->_instructions));
		return block;
	}

	void addEdge(Block *from, Block *to) {
		from->_out.push_back(to);
		to->_in.push_back(from);
	}

	void removeJumpsToJumps() {
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

	void replaceEdges(Block *from, Block *oldTo, Block *newTo) {
		size_t n = count(oldTo->_in.begin(), oldTo->_in.end(), from);
		oldTo->_in.remove(from);
		fill_n(back_inserter(newTo->_in), n, from);
		foreach (Block *&block, from->_out)
			if (block == oldTo)
				block = newTo;
	}

	// to be called after order blocks
	void removeUnreachableBlocks() {
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

	// assign block numbers in post-order
	void orderBlocks() {
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

	std::list<Block*> intervals() const {
		std::list<Block*> ret;
		assignIntervals();
		foreach (Block *u, _blocks)
			if (u->_interval == u)
				ret.push_back(u);
		return ret;
	}

	bool inLoop(Block *head, Block *latch, Block *u) {
		return u->_interval == head && latch->_number <= u->_number && u->_number < head->_number;
	}

	LoopType loopType(Block *head, Block *latch) {
		if (head->_out.size() == 1 && latch->_out.size() == 1)
			return ENDLESS;
		if (head->_out.size() == 1 && latch->_out.size() == 2)
			return POST_TESTED;
		if (head->_out.size() == 2 && latch->_out.size() == 1)
			return PRE_TESTED;
		// head->_out.size() == 2 && latch->_out.size() == 2
		if (inLoop(head, latch, head->_out.front()))
			return POST_TESTED;
		else
			return PRE_TESTED;
	}

	Block *loopFollow(Block *head, Block *latch) {
		if (head->_loopType == PRE_TESTED)
			return head->_out.front();
		if (head->_loopType == POST_TESTED)
			return latch->_out.back();
		// ENDLESS
		Block *ret = 0;
		foreach (Block *u, _blocks)
			if (inLoop(head, latch, u) && u->_out.size() == 2 && (!ret || ret->_number < u->_out.back()->_number))
				ret = u->_out.back();
		return ret;
	}

	void loopStruct() {
		for (size_t size = _blocks.size()+1; size > intervals().size(); size = intervals().size(), extendIntervals())
			foreach (Block *interval, intervals()) {
				foreach (Block *latch, interval->_in) {
					if (latch->_interval == interval && !latch->_loopHead) {
						foreach (Block *u, _blocks)
							if (inLoop(interval, latch, u))
								u->_loopHead = interval;
						interval->_loopLatch = latch; // TODO do we need this?
						interval->_loopType = loopType(interval, latch);
						interval->_loopFollow = loopFollow(interval, latch);
					}
				}
			}
	}

	void extendIntervals() {
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
		d.intervals();
		foreach (Block *du, d._blocks)
			foreach (Block *v, _blocks)
				if (v->_interval == du->_primitive)
					v->_interval = du->_interval->_primitive;
	}

	std::string graphvizToString(const std::string &fontname="", int fontsize=0) const {
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
					ret	<< "shape=box,label=\"<number: " << u->_number;
					if (u->_loopFollow)
						ret << ", loop_type=" << (u->_loopType == PRE_TESTED ? "pre_tested" : u->_loopType == POST_TESTED ? "post_tested" : "endless");
					ret << ">\\n" << graphvizEscapeLabel(u->toString()) << "\"];" << std::endl;
				}
			ret << "}" << std::endl;
		}
		foreach (Block *u, _blocks)
			foreach (Block *v, u->_out)
				ret << '"' << u << "\" -> \"" << v << '"' << (v == u->_loopFollow ? "[color=blue]" : "") << ";" << std::endl;
		ret << "}" << std::endl;
		return ret.str();
	}

	void assignIntervals() const {
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

	static std::string graphvizEscapeLabel(const std::string &s) {
		std::string ret;
		foreach (char c, s) {
			if (c == '\n' || c == '"' || c == '\\')
				ret.push_back('\\');
			ret.push_back(c == '\n' ? 'l' : c);   // align lines to the left
		}
		return ret;
	}
};

#endif
