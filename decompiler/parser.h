#ifndef PARSER_H
#define PARSER_H


#include <fstream>
#include <vector>

#include <cassert>
#include <cstring>
#include <cstdio>

using namespace std;


#include "misc.h"
#include "reader.h"
#include "instruction.h"


struct Scumm6Parser {

	SubopcodeReader *_reader;

	Scumm6Parser() {
		_reader = new SubopcodeReader();
		_reader->registerOpcode(0x01, new SimpleReader("push", "w"));
		_reader->registerOpcode(0x03, new SimpleReader("pushVar(v->s)", "w"));
		_reader->registerOpcode(0x07, new SimpleReader("arrayRead", "w"));
		_reader->registerOpcode(0x0e, new SimpleReader("=="));
		_reader->registerOpcode(0x0f, new SimpleReader("!="));
		_reader->registerOpcode(0x10, new SimpleReader(">"));
		_reader->registerOpcode(0x12, new SimpleReader("<"));
		_reader->registerOpcode(0x13, new SimpleReader(">="));
		_reader->registerOpcode(0x14, new SimpleReader("+"));
		_reader->registerOpcode(0x43, new SimpleReader("writeVar(s->v)", "w"));
		_reader->registerOpcode(0x4f, new SimpleReader("varInc", "w"));
		_reader->registerOpcode(0x5d, new SimpleReader("jumpIfNot", "w"));
		_reader->registerOpcode(0x5e, new SimpleReader("startScript"));
		_reader->registerOpcode(0x60, new SimpleReader("startObject"));
		_reader->registerOpcode(0x66, new SimpleReader("stopObjectCode"));
		_reader->registerOpcode(0x67, new SimpleReader("endCutscene"));
		_reader->registerOpcode(0x68, new SimpleReader("cutscene"));
		_reader->registerOpcode(0x6d, new SimpleReader("classOfIs"));
		_reader->registerOpcode(0x72, new SimpleReader("getOwner"));
		_reader->registerOpcode(0x73, new SimpleReader("jump", "w"));
		_reader->registerOpcode(0x7c, new SimpleReader("stopScript"));
		_reader->registerOpcode(0x7d, new SimpleReader("walkActorToObj"));
		_reader->registerOpcode(0x7e, new SimpleReader("walkActorTo"));
		_reader->registerOpcode(0x81, new SimpleReader("faceCutscene"));
		_reader->registerOpcode(0x82, new SimpleReader("animateActor"));
		_reader->registerOpcode(0x83, new SimpleReader("doSentence"));
		_reader->registerOpcode(0x8d, new SimpleReader("getObjectX"));
		_reader->registerOpcode(0x8e, new SimpleReader("getObjectY"));
		_reader->registerOpcode(0xa3, new SimpleReader("getVerbEntryPoint"));
		SubopcodeReader *wait = new SubopcodeReader();
		wait->registerOpcode(168, new SimpleReader("wait_forActor", "w"));
		_reader->registerOpcode(0xa9, wait);
		_reader->registerOpcode(0xad, new SimpleReader("isAnyOf"));
		_reader->registerOpcode(0xb0, new SimpleReader("delay"));
		_reader->registerOpcode(0xb3, new SimpleReader("stopSentence"));

		//		SubopcodeReader *three = new SubopcodeReader();
		//		three->registerOpcode(1, new SimpleReader("three/1", 1));
		//		three->registerOpcode(2, new SimpleReader("three/2", 2));
		//		_reader->registerOpcode(3, three);
		//		_reader->registerOpcode(4, new SimpleReader("four", 4));
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
				printf("%2x - %.4x\n", code, offset);
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
		while (_reader->readInstruction(f, v, f.tellg()))
			;
		return v;
	}

};


#endif
