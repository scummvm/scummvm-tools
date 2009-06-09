#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <string>
#include <cstdio>

#include "misc.h"


struct Instruction {
	string _description;
	uint32 _addr;
	Instruction(string description, uint32 addr) : _description(description), _addr(addr) {
	}
	virtual ~Instruction() {
	}
};


struct Jump : public Instruction {
	int16 _offset;
	Jump(string description, uint32 addr, int16 offset) : Instruction(description, addr), _offset(offset) {
	}
};

struct CondJump : public Jump {
	CondJump(string description, uint32 addr, int16 offset) : Jump(description, addr, offset) {
	}
};


struct Script {

	vector<Instruction*> _v;

	Script(vector<Instruction*> v) : _v(v) {
	}

	uint32 findIdx(uint32 addr) {
		for (uint32 i = 0; i < _v.size(); i++)
			if (_v[i]->_addr == addr)
				return i;
		printf("!!! no instruction with address %x (%d)\n", addr, addr);
		return -1;
	}

};


#endif
