#ifndef GRAPH_H
#define GRAPH_H

#include "misc.h"

#include <cassert>

#include <list>
#include <map>
#include <set>
#include <sstream>

#include <boost/foreach.hpp>
#ifndef foreach
#define foreach BOOST_FOREACH
#endif

#include <iostream>



enum LoopType {
	PRE_TESTED,
	POST_TESTED,
	ENDLESS
};


template<typename Data>
struct Graph : boost::noncopyable {

	struct Node : boost::noncopyable {

		Data _data;

		const std::list<Node*> &out() const {
			return _out;
		}

		// wouldn't be needed if Graph<X> and Graph<Y> weren't totally alien classes
		Node *interval() const {
			return _interval;
		}

	private:

		friend class Graph;

		Node *_interval;
		std::list<Node*> _in;
		std::list<Node*> _out;

		int _number;

		Node *_loopHead;
		Node *_loopLatch;
		Node *_loopFollow;
		LoopType _loopType;

		Node(const Data &data) : _data(data), _interval(), _number(), _loopHead(), _loopFollow() {
		}

		~Node() {
		}
	};

	std::list<Node*> _nodes;
	Node *_entry;
	int _currentNumber;

	Graph() : _entry(), _currentNumber() {
	}

	~Graph() {
		foreach (Node *u, _nodes)
			delete u;
	}

	void setEntry(Node *entry) {
		_entry = entry;
	}

	Node *addNode(const Data &data) {
		Node* node = new Node(data);
		_nodes.push_back(node);
		if (!_entry)
			_entry = node;
		return node;
	}

	void addEdge(Node *from, Node *to) {
		from->_out.push_back(to);
		to->_in.push_back(from);
	}

	void replaceEdges(Node *from, Node *oldTo, Node *newTo) {
		size_t n = count(oldTo->_in.begin(), oldTo->_in.end(), from);
		oldTo->_in.remove(from);
		fill_n(back_inserter(newTo->_in), n, from);
		foreach (Node *&node, from->_out)
			if (node == oldTo)
				node = newTo;
	}

	// to be called after order nodes
	void removeUnreachableNodes() {
		for (typename std::list<Node*>::iterator uit = _nodes.begin(); uit != _nodes.end(); )
			if ((*uit)->_number)
				uit++;
			else {
				foreach (Node *v, (*uit)->_out)
					v->_in.remove(*uit);
				delete *uit;
				uit = _nodes.erase(uit);
			}
	}

	// assign node numbers in post-order
	void orderNodes() {
		assert(_entry);
		orderVisit(_entry, 0);
	}

	int orderVisit(Node *u, int number) {
		u->_number = -1;
		foreach (Node *v, u->_out)
			if (!v->_number)
				number = orderVisit(v, number);
		u->_number = ++number;
		return number;
	}

	std::list<Node*> intervals() const {
		std::list<Node*> ret;
		assignIntervals();
		foreach (Node *u, _nodes)
			if (u->_interval == u)
				ret.push_back(u);
		return ret;
	}

	bool inLoop(Node *head, Node *latch, Node *u) {
		return u->_interval == head && latch->_number <= u->_number && u->_number < head->_number;
	}

	LoopType loopType(Node *head, Node *latch) {
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

	Node *loopFollow(Node *head, Node *latch) {
		if (head->_loopType == PRE_TESTED)
			return head->_out.front();
		if (head->_loopType == POST_TESTED)
			return latch->_out.back();
		// ENDLESS
		Node *ret = 0;
		foreach (Node *u, _nodes)
			if (inLoop(head, latch, u) && u->_out.size() == 2 && (!ret || ret->_number < u->_out.back()->_number))
				ret = u->_out.back();
		return ret;
	}

	void loopStruct() {
		for (size_t size = _nodes.size()+1; size > intervals().size(); size = intervals().size(), extendIntervals())
			foreach (Node *interval, intervals()) {
				foreach (Node *latch, interval->_in) {
					if (latch->_interval == interval && !latch->_loopHead) {
						foreach (Node *u, _nodes)
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
		Graph<Node*> d;
		std::map<Node*, typename Graph<Node*>::Node*> trans;
		foreach (Node *interval, intervals())
			trans[interval] = d.addNode(interval);
		foreach (Node *interval, intervals())
			foreach (Node *u, interval->_in)
				if (u->_interval != interval)
					d.addEdge(trans[u->_interval], trans[interval]);
		d.setEntry(trans[_entry]);
		d.intervals();
		foreach (typename Graph<Node*>::Node *du, d._nodes)
			foreach (Node *v, _nodes)
				if (v->_interval == du->_data)
					v->_interval = du->interval()->_data;
	}

	template<typename Printer>   // printer is a functor taking Data and returning a string
	std::string graphvizPrint(Printer printer, const std::string &fontname="Courier", int fontsize=14) const {
		std::stringstream ret;
		ret << "digraph G {" << std::endl;
		foreach (Node *interval, intervals()) {
			ret << "subgraph " << '"' << "cluster_" << interval << '"' << " {" << std::endl;
			ret << "style=dotted;" << std::endl;
			foreach (Node *u, _nodes)
				if (u->_interval == interval) {
					ret << '"' << u << '"'
						<< "[";
					if (fontname != "")
						ret << "fontname=" << '"' << fontname << '"' << ","
							<< "fontsize=" << fontsize << ",";
					ret	<< "shape=box,"
						<< "label=" << '"'
						<< "<number: " << u->_number;
					if (u->_loopFollow)
						ret << ", loop_type=" << (u->_loopType == PRE_TESTED ? "pre_tested" : u->_loopType == POST_TESTED ? "post_tested" : "endless");
					ret << ">\\n"
						<< graphvizEscapeLabel(printer(u->_data))
						<< '"'
						<< "];" << std::endl;
				}
			ret << "}" << std::endl;
		}
		foreach (Node *u, _nodes) {
			foreach (Node *v, u->_out)
				ret << '"' << u << '"'
					<< " -> "
					<< '"' << v << '"'
				    << (v == u->_loopFollow ? "[color=blue]" : "")
					<< ";" << std::endl;
		}
		ret << "}" << std::endl;
		return ret.str();
	}

private:

	void assignIntervals() const {
		std::list<Node*> intervals;
		intervals.push_back(_entry);
		foreach (Node *interval, intervals) {
			interval->_interval = interval;
			for (bool added = true; added; ) {
				added = false;
				foreach (Node *m, _nodes) {
					bool allPredInInterval = true;
					foreach (Node *p, m->_in)
						allPredInInterval &= p->_interval == interval;
					if (!m->_interval && allPredInInterval) {
						added = true;
						m->_interval = interval;
					}
				}
			}
			foreach (Node *m, _nodes) {
				bool anyPredInInterval = false;
				foreach (Node *p, m->_in)
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
