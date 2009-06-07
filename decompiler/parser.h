#ifndef READER_H
#define READER_H


#include <fstream>
#include <vector>

#include <cassert>
#include <cstring>
#include <cstdio>

using namespace std;


#include "misc.h"
#include "instruction.h"


using namespace std;


struct Reader {
	virtual void readInstruction(ifstream &f, vector<Instruction*> &v, uint32 addr) = 0;
};


struct SimpleReader : public Reader {

	string _description;
	int _skip;

	SimpleReader(string description, int skip=0) : _description(description), _skip(skip) {};

	virtual void readInstruction(ifstream &f, vector<Instruction*> &v, uint32 addr) {
		for (int i = 0; i < _skip; i++) {
			char c = f.get();
			printf("SKIPPED: 0x%x\n", (unsigned int) c);
		}
		v.push_back(new Instruction(_description, addr));
	}
};


struct SubopcodeReader : public Reader {

	Reader *_dispatchTable[256];

	SubopcodeReader() {
		memset(_dispatchTable, 0, sizeof(_dispatchTable));
	}

	void registerOpcode(uint8 opcode, Reader *reader) {
		_dispatchTable[opcode] = reader;
	}

	void readInstruction(ifstream& f, vector<Instruction*> &v, uint32 addr) {
		uint8 opcode = f.get();
		Reader* reader = _dispatchTable[opcode];
		assert(reader);
		reader->readInstruction(f, v, addr);
	}
};


struct Scumm6Parser {

	Reader *_dispatchTable[256];

	Scumm6Parser() {
		memset(_dispatchTable, 0, sizeof(_dispatchTable));
		_dispatchTable[0] = new SimpleReader("zero", 0);
		_dispatchTable[1] = new SimpleReader("one", 1);
		_dispatchTable[2] = new SimpleReader("two", 2);
		SubopcodeReader *three = new SubopcodeReader();
		three->registerOpcode(1, new SimpleReader("three/1", 1));
		three->registerOpcode(2, new SimpleReader("three/2", 2));
		_dispatchTable[3] = three;
		_dispatchTable[4] = new SimpleReader("four", 4);
	}

	void parseHeader(ifstream &f) {
		switch (read_be_uint32(f)) {
		case 'LSC2':
			read_le_uint32(f);
			read_le_uint32(f); // script number
			break;
		case 'LSCR':
			read_le_uint32(f);
			f.get(); // script number
			break;
		case 'SCRP':
		case 'ENCD':
		case 'EXCD':
			read_le_uint32(f);
			break;
		case 'VERB':
			read_le_uint32(f);
			uint16 minOffset = 65535;
			for (uint8 code = f.get(); code != 0; code = f.get()) {
				uint16 offset = read_le_uint16(f);
				printf("%2X - %.4X\n", code, offset);
				if (offset < minOffset)
					minOffset = offset;
			}
			f.seekg(minOffset);
			break;
		}
	}

	vector<Instruction*> parseFile(const char *filename) {
		vector<Instruction*> v;
		ifstream f;
		f.open(filename, ios::binary);
		parseHeader(f);
		while (true) {
			uint8 addr = f.tellg();
			uint8 opcode = f.get();
			if (f.eof())
				return v;
			Reader* reader = _dispatchTable[opcode];
			assert(reader);
			reader->readInstruction(f, v, addr);
		}
	}

};


#endif
