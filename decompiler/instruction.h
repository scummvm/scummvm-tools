#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <string>

#include "misc.h"

struct Instruction {
	string _description;
	uint32 _addr;
	Instruction(string description, uint32 addr) : _description(description), _addr(addr) {}
};

#endif
