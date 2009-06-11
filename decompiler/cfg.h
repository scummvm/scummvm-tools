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
	virtual ~Node() {
	};
};

uint32 Node::_g_id = 0;


struct BasicBlock : public Node {
	index_t _start, _end;
	BasicBlock(index_t start, index_t end) : Node(), _start(start), _end(end) {
	}

	void printInsns(vector<Instruction*> &v) {
		printHeader(v);
		printf(" in(");
		for (unsigned i = 0; i < _in.size(); i++)
			printf("%d%s", _in[i]->_id, i == _in.size()-1 ? "" : ",");
		printf(") out(");
		printOuts();
		printf("):\n");
		for (unsigned i = _start; i < _end; i++) {
			if (i >= 1 && v[i]->_addr == v[i-1]->_addr)
				printf("         |           %s", v[i]->_description.c_str());
			else
				printf("[d] %04x | [r] %04x: %s", v[i]->_addr-8, v[i]->_addr, v[i]->_description.c_str());
			Jump *j = dynamic_cast<Jump*>(v[i]);
			if (j) {
				uint32 jaddr = j->_addr+j->_offset;
				printf(" ([d] %04x | [r] %04x)", jaddr-8, jaddr);
			}
			printf("\n");
		}
	}
	virtual void printOuts() {
	};
	void printHeader(vector<Instruction*> &v) {
		printf("%d (%04x..%04x)", _id, v[_start]->_addr-8, v[_end-1]->_addr-8);
	}
	virtual void print(vector<Instruction*> &v) {
		printf("=== ");
		printInsns(v);
		printf("===\n\n");
	}

};


struct CFG {

	vector<BasicBlock*> _blocks;
	Script &_script;

	void printBasicBlocks() {
		for (uint32 i = 0; i < _blocks.size(); i++)
			_blocks[i]->print(_script._instructions);
	}

	void printDot() {
		printf("digraph G {\n");
		vector<Instruction*> *_v = &_script._instructions;
		for (uint32 i = 0; i < _blocks.size(); i++) {
			BasicBlock *bb = _blocks[i];
			for (uint32 j = 0; j < bb->_out.size(); j++) {
				printf("\""); bb->printHeader(*_v); printf("\"");
				printf(" -> ");
				printf("\""); ((BasicBlock*)bb->_out[j])->printHeader(*_v); printf("\"");
				printf(" %s\n", j == 0 ? "[style=bold]" : "");
			}
			printf("\""); bb->printHeader(*_v); printf("\"\n");
		}
		printf("}\n");
	}

	BasicBlock *blockByStart(index_t idx) {
		for (index_t i = 0; i < _blocks.size(); i++)
			if (_blocks[i]->_start == idx)
				return _blocks[i];
		return 0;
	}

	BasicBlock *blockByEnd(index_t idx) {
		for (index_t i = 0; i < _blocks.size(); i++)
			if (_blocks[i]->_end == idx)
				return _blocks[i];
		return 0;
	}

	void addEdge(BasicBlock *from, BasicBlock *to) {
		from->_out.push_back(to);
		to->_in.push_back(from);
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
