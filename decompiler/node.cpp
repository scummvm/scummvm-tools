#include "node.h"

#include <boost/foreach.hpp>

using namespace boost;
using namespace std;

#ifndef foreach
#define foreach BOOST_FOREACH
#endif


Node::Node() : _interval(), _number(), _dominator(), _component() {
}


Node::~Node() {
}

string Node::toString() {
	ostringstream ret;
	foreach (Instruction *instruction, _instructions)
		ret << instruction->toString();
	return ret.str();
}
