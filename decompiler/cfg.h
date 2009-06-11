#ifndef CFG_H
#define CFG_H

#include <set>
#include <vector>


#include <cstdio>

using namespace std;


#include "instruction.h"
#include "misc.h"


struct Node {

	uint32 _id;
	static uint32 _g_id;
	vector<Node*> _in, _out;

	Node() {
		_id = _g_id++;
	}

	void removeInEdge(Node *from) {
		for (vector<Node*>::iterator it = _in.begin(); it != _in.end(); it++)
			if (*it == from) {
				_in.erase(it);
				return;
			}
	}

	virtual ~Node() {
	};
};

uint32 Node::_g_id = 0;


struct BasicBlock : public Node {

	index_t _start, _end;

	BasicBlock(index_t start, index_t end) : Node(), _start(start), _end(end) {
	}

	void printHeader(Script &script) {
		printf("%d (%04x..%04x)", _id, script[_start]->_addr-8, script[_end-1]->_addr-8);
	}

	virtual void print(Script &script) {
		printf("=== ");
		printHeader(script);
		printf(" in(");
		for (unsigned i = 0; i < _in.size(); i++)
			printf("%d%s", _in[i]->_id, i == _in.size()-1 ? "" : ",");
		printf(") out(");
		for (unsigned i = 0; i < _out.size(); i++)
			printf("%d%s", _out[i]->_id, i == _out.size()-1 ? "" : ",");
		printf("):\n");
		for (unsigned i = _start; i < _end; i++)
			script.print(i);
		printf("===\n\n");
	}
};


struct CFG {

	vector<BasicBlock*> _blocks;
	Script &_script;

	void printBasicBlocks() {
		for (uint32 i = 0; i < _blocks.size(); i++)
			_blocks[i]->print(_script);
	}

	void printDot() {
		printf("digraph G {\n");
		for (uint32 i = 0; i < _blocks.size(); i++) {
			BasicBlock *bb = _blocks[i];
			for (uint32 j = 0; j < bb->_out.size(); j++) {
				printf("\""); bb->printHeader(_script); printf("\"");
				printf(" -> ");
				printf("\""); ((BasicBlock*)bb->_out[j])->printHeader(_script); printf("\"");
				printf(" %s\n", j == 0 ? "[style=bold]" : "");
			}
			printf("\""); bb->printHeader(_script); printf("\"\n");
		}
		printf("}\n");
	}

	BasicBlock *blockByStart(index_t idx) {
		for (uint32 i = 0; i < _blocks.size(); i++)
			if (_blocks[i]->_start == idx)
				return _blocks[i];
		return 0;
	}

	BasicBlock *blockByEnd(index_t idx) {
		for (uint32 i = 0; i < _blocks.size(); i++)
			if (_blocks[i]->_end == idx)
				return _blocks[i];
		return 0;
	}

	void addEdge(BasicBlock *from, BasicBlock *to) {
		from->_out.push_back(to);
		to->_in.push_back(from);
	}

	void removeJumpsToJumps() {
		for (bool changed = true; changed; ) {
			changed = false;
			for (uint32 i = 0; i < _blocks.size(); i++)
				for (uint32 j = 0; j < _blocks[i]->_out.size(); j++) {
					BasicBlock *bbout = (BasicBlock*) _blocks[i]->_out[j];
					Jump *jump = dynamic_cast<Jump*>(_script[bbout->_start]);
					if (jump && !dynamic_cast<CondJump*>(jump)) {
						changed = true;
						BasicBlock *newtgt = blockByStart(_script.index(jump->target()));
						bbout->removeInEdge(_blocks[i]);
						newtgt->_in.push_back(_blocks[i]);
						_blocks[i]->_out[j] = newtgt;
					}
				}
		}
	}

	// TODO
	// after this _blocks[i]->_id == i no longer holds
	// also it won't work for more than one graph so we need a better way
	// (move to _blocks being a list and map<id,Block*> for quick access?)
	void removeDeadBlocks() {
		set<uint32> visited;
		vector<uint32> stack;
		visited.insert(0);
		stack.push_back(0);
		while (!stack.empty()) {
			uint32 id = stack.back();
			stack.pop_back();
			Node *node = _blocks[id];
			for (uint32 i = 0; i < node->_out.size(); i++)
				if (visited.find(node->_out[i]->_id) == visited.end()) {
					visited.insert(node->_out[i]->_id);
					stack.push_back(node->_out[i]->_id);
				}
		}
		for (vector<BasicBlock*>::iterator it = _blocks.begin(); it != _blocks.end(); )
			if (visited.find((*it)->_id) == visited.end())
				it = _blocks.erase(it);
			else
				it++;
	}

	CFG(Script &script) : _script(script) {
		Jump *j;
		set<address_t> targets;
		targets.insert(0);
		for (index_t i = 0; i < script.size(); i++) {
			if ((j = dynamic_cast<Jump*>(script[i]))) {
				targets.insert(script.index(j->_addr+j->_offset));
				if (dynamic_cast<CondJump*>(script[i]) && i != script.size()-1)
					targets.insert(i+1);
			}
		}
		index_t bbstart = 0;
		for (index_t i = 0; i < script.size(); i++)
			if (targets.find(i+1) != targets.end() || dynamic_cast<Jump*>(script[i])) {
				_blocks.push_back(new BasicBlock(bbstart, i+1));
				bbstart = i+1;
			}
		if (bbstart != script.size())
			_blocks.push_back(new BasicBlock(bbstart, script.size()));
		for (index_t i = 0; i < script.size(); i++) {
			BasicBlock *bb = blockByEnd(i+1);
			if ((j = dynamic_cast<Jump*>(script[i])))
				addEdge(bb, blockByStart(script.index(j->target())));
			if (targets.find(i+1) != targets.end() || dynamic_cast<CondJump*>(script[i]))
				addEdge(bb, blockByStart(i+1));
		}
	};

};


#endif
