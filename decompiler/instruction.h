#ifndef DEC_INSTRUCTION_H
#define DEC_INSTRUCTION_H

#include <stdint.h>

/**
 * Enumeration for categorizing the different kinds of instructions.
 */
enum InstType { 
	ARITHMETIC,
	BOOLEAN,
	COMPARISON,
	COND_JUMP,
	JUMP,
	LOAD,
	SPECIAL,
	STORE
};

/**
 * Structure for representing an instruction.
 */
struct Instruction {
	uint32_t _address; ///<The instruction address.
	uint32_t _opcode; ///<The instruction opcode.
	InstType _type; ///<The instruction type.
	void* _params[16]; ///<Array of pointers to point to the parameters used for the instruction.
};

#endif
