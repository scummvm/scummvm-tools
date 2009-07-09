#include "block.h"

#include <boost/foreach.hpp>

using namespace boost;
using namespace std;

#ifndef foreach
#define foreach BOOST_FOREACH
#endif


Block::Block() : _interval(), _number(), _dominator(), _component() {
}


Block::~Block() {
}

string Block::toString() {
	ostringstream ret;
	foreach (Instruction *instruction, _instructions)
		ret << instruction->toString();
	return ret.str();
}
