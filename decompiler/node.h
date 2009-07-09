#ifndef NODE_H
#define NODE_H

#include "instruction.h"

#include <list>

#include <boost/utility.hpp>


struct Node : boost::noncopyable {

	bool _visited;
	Node *_dominator;       // immediate dominator
	Node *_interval;        // header node of the interval this node belongs to
	Node *_primitive;       // interval header of the graph from which this graph has been derived
	Node *_component;
	int _number;             // number in post-order
	std::list<Node*> _in;
	std::list<Node*> _out;
	std::list<Instruction*> _instructions;

	Node();
	~Node();
	std::string toString();
};

#endif
