#ifndef BLOCK_H
#define BLOCK_H

#include "instruction.h"

#include <list>

#include <boost/utility.hpp>


struct Block : boost::noncopyable {

	bool _visited;
	Block *_dominator;       // immediate dominator
	Block *_interval;        // header node of the interval this block belongs to
	Block *_primitive;       // interval header of the graph from which this graph has been derived
	Block *_component;
	int _number;             // number in post-order
	std::list<Block*> _in;
	std::list<Block*> _out;
	std::list<Instruction*> _instructions;

	Block();
	~Block();
	std::string toString();
};

#endif
