#ifndef DEC_DISASSEMBLER_H
#define DEC_DISASSEMBLER_H

#include "instruction.h"
#include "common/file.h"

struct Opcode {
	uint32_t _opcode;
	InstType _type;
	char* name;
	char* paramlist;
};

class Disassembler {
	protected:
		Common::File f;
		
		void readParams(Instruction* inst, char* paramInfo);
		virtual void setupOpcodes() = 0;
	public:
		void open(char* filename);
		void disassemble();
};

#endif
