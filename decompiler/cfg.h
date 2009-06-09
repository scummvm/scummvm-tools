#ifndef CFG_H
#define CFG_H

#include <vector>

#include <cstdio>

using namespace std;


#include "instruction.h"
#include "misc.h"


struct BasicBlock {
	static uint32 _g_id;
	uint32 _id;
	uint32 _start, _end;
	vector<BasicBlock*> _in;
	BasicBlock(uint32 start, uint32 end) : _start(start), _end(end) {
		_id = _g_id++;
	}
	void printInsns(vector<Instruction*> &v) {
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
	virtual void print(vector<Instruction*> &v) = 0;
	virtual ~BasicBlock() {
	}
};

uint32 BasicBlock::_g_id = 0;

struct BB2Way : public BasicBlock {
	BasicBlock *_out1, *_out2;
	BB2Way(uint32 start, uint32 end) : BasicBlock(start, end) {
	}
	void print(vector<Instruction*> &v) {
		printf("=== BB2Way %d [%d,%d)", _id, _start, _end);
		printInsns(v);
		printf("===\n\n");
	}
	void printOuts() {
		printf("%d,%d", _out1->_id, _out2->_id);
	}
};

struct BBFall : public BasicBlock {
	BasicBlock *_out;
	BBFall(uint32 start, uint32 end) : BasicBlock(start, end) {
	}
	void print(vector<Instruction*> &v) {
		printf("=== BBFall #%d [%d,%d)", _id, _start, _end);
		printInsns(v);
		printf("===\n\n");
	}
	void printOuts() {
		printf("%d", _out->_id);
	}
};

struct BBEnd : public BasicBlock {
	BBEnd(uint32 start, uint32 end) : BasicBlock(start, end) {
	}
	void print(vector<Instruction*> &v) {
		printf("=== BBEnd #%d [%d,%d)", _id, _start, _end);
		printInsns(v);
		printf("===\n\n");
	}
};


struct CFG {

	vector<BasicBlock*> _blocks;
	vector<uint32> _targets;

	bool isTarget(uint32 addr) {
		for (uint32 i = 0; i < _targets.size(); i++)
			if (_targets[i] == addr)
				return true;
		return false;
	}

	BasicBlock *blockByStartIdx(uint32 idx) {
		for (uint32 i = 0; i < _blocks.size(); i++)
			if (_blocks[i]->_start == idx)
				return _blocks[i];
		return 0;
	}

	BasicBlock *blockByEndIdx(uint32 idx) {
		for (uint32 i = 0; i < _blocks.size(); i++)
			if (_blocks[i]->_end == idx)
				return _blocks[i];
		return 0;
	}

	CFG(vector<Instruction*> &v) {
		Script s(v);
		_targets.push_back(0);
		for (uint32 i = 0; i < v.size(); i++) {
			Jump *j = dynamic_cast<Jump*>(v[i]);
			if (j) {
				_targets.push_back(s.findIdx(j->_addr+j->_offset));
				if (dynamic_cast<CondJump*>(v[i]) && i != v.size()-1)
					_targets.push_back(s.findIdx(v[i+1]->_addr));
			}
		}
		uint32 bbstart = 0;
		for (uint32 i = 0; i < v.size(); i++)
			if (dynamic_cast<CondJump*>(v[i])) {
				_blocks.push_back(new BB2Way(bbstart, i+1));
				bbstart = i+1;
			}
			else if (dynamic_cast<Jump*>(v[i])) {
				_blocks.push_back(new BBFall(bbstart, i+1));
				bbstart = i+1;
			} else if (isTarget(i+1)) {
				_blocks.push_back(new BBFall(bbstart, i+1));
				bbstart = i+1;
			}
		if (bbstart != v.size())
			_blocks.push_back(new BBEnd(bbstart, v.size()));
		for (uint32 i = 0; i < v.size(); i++) {
			Jump *j = dynamic_cast<Jump*>(v[i]);
			CondJump *cj = dynamic_cast<CondJump*>(v[i]);
			if (cj) {
				BB2Way *bb2way = dynamic_cast<BB2Way*>(blockByEndIdx(i+1));
				bb2way->_out1 = blockByStartIdx(s.findIdx(cj->_addr+cj->_offset));
				bb2way->_out2 = blockByStartIdx(s.findIdx(v[i+1]->_addr));
			}
			else if (j) {
				BBFall *bbfall = dynamic_cast<BBFall*>(blockByEndIdx(i+1));
				bbfall->_out = blockByStartIdx(s.findIdx(j->_addr+j->_offset));
			} else if (isTarget(i+1)) {
				BBFall *bbfall = dynamic_cast<BBFall*>(blockByEndIdx(i+1));
				bbfall->_out = blockByStartIdx(s.findIdx(v[i+1]->_addr));
			}
			if (cj) {
				BasicBlock *bb1 = blockByStartIdx(s.findIdx(cj->_addr+cj->_offset));
				BasicBlock *bb2 = blockByStartIdx(s.findIdx(v[i+1]->_addr));
				bb1->_in.push_back(blockByEndIdx(i+1));
				bb2->_in.push_back(blockByEndIdx(i+1));
			} else if (j) {
				BasicBlock *bb1 = blockByStartIdx(s.findIdx(j->_addr+j->_offset));
				bb1->_in.push_back(blockByEndIdx(i+1));
			} else if (isTarget(i+1)) {
				BasicBlock *bb2 = blockByStartIdx(s.findIdx(v[i+1]->_addr));
				bb2->_in.push_back(blockByEndIdx(i+1));
			}
		}
	};

};


#endif
