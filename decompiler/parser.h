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


struct Scumm6Parser : public Parser {

	SubopcodeReader *_reader;

	Scumm6Parser() {
		_reader = new SubopcodeReader("main");

		//		_reader->registerOpcode(0x00, new SimpleReader("pushByte", "b"));
		_reader->registerOpcode(0x01, new SimpleReader("push", "W"));
		_reader->registerOpcode(0x03, new SimpleReader("pushVar", "w"));
		_reader->registerOpcode(0x07, new SimpleReader("wordArrayRead", "w"));
		_reader->registerOpcode(0x0c, new SimpleReader("dup"));
		_reader->registerOpcode(0x0d, new SimpleReader("not"));
		_reader->registerOpcode(0x0e, new SimpleReader("=="));
		_reader->registerOpcode(0x0f, new SimpleReader("!="));

		_reader->registerOpcode(0x10, new SimpleReader(">"));
		_reader->registerOpcode(0x11, new SimpleReader("<"));
		_reader->registerOpcode(0x12, new SimpleReader("<="));
		_reader->registerOpcode(0x13, new SimpleReader(">="));
		_reader->registerOpcode(0x14, new SimpleReader("+"));
		_reader->registerOpcode(0x15, new SimpleReader("-"));
		_reader->registerOpcode(0x18, new SimpleReader("&&"));

		_reader->registerOpcode(0x43, new SimpleReader("writeVar", "w"));
		_reader->registerOpcode(0x47, new SimpleReader("wordArrayWrite", "w"));
		_reader->registerOpcode(0x4f, new SimpleReader("wordVarInc", "w"));

		_reader->registerOpcode(0x5c, new CondJumpReader("jumpIf", "o3"));
		_reader->registerOpcode(0x5d, new SeqReader(new SimpleReader("not"),
													new CondJumpReader("jumpIf", "o3")));
		_reader->registerOpcode(0x5e, new SimpleReader("startScript"));
		_reader->registerOpcode(0x5f, new SimpleReader("startScriptQuick"));

		_reader->registerOpcode(0x60, new SimpleReader("startObject"));
		_reader->registerOpcode(0x61, new SimpleReader("drawObject"));
		_reader->registerOpcode(0x65, new SimpleReader("stopObjectCodeA"));
		_reader->registerOpcode(0x66, new SimpleReader("stopObjectCodeB"));
		_reader->registerOpcode(0x67, new SimpleReader("endCutscene"));
		_reader->registerOpcode(0x68, new SimpleReader("cutscene"));
		SubopcodeReader *cursor = new SubopcodeReader("cursor");
		_reader->registerOpcode(0x6b, cursor);
		cursor->registerOpcode(0x90, new SimpleReader("cursor.cursorOn"));
		cursor->registerOpcode(0x92, new SimpleReader("cursor.userPutOn"));
		cursor->registerOpcode(0x99, new SimpleReader("cursor.setCursorImg"));
		cursor->registerOpcode(0x9c, new SimpleReader("cursor.initCharset"));
		//		cursor->registerOpcode(0x9d, new SimpleReader("cursor.charsetColor"));
		_reader->registerOpcode(0x6c, new SimpleReader("breakHere"));	// = yield
		_reader->registerOpcode(0x6d, new SimpleReader("classOfIs"));
		_reader->registerOpcode(0x6e, new SimpleReader("setClass"));
		_reader->registerOpcode(0x6f, new SimpleReader("getState"));

		_reader->registerOpcode(0x70, new SimpleReader("setState"));
		_reader->registerOpcode(0x72, new SimpleReader("getOwner"));
		_reader->registerOpcode(0x73, new JumpReader("jump", "o3"));
		_reader->registerOpcode(0x74, new JumpReader("startSound"));
		_reader->registerOpcode(0x7a, new SimpleReader("setCameraAt"));
		_reader->registerOpcode(0x7b, new SimpleReader("loadRoom"));
		_reader->registerOpcode(0x7c, new SimpleReader("stopScript"));
		_reader->registerOpcode(0x7d, new SimpleReader("walkActorToObj"));
		_reader->registerOpcode(0x7e, new SimpleReader("walkActorTo"));
		_reader->registerOpcode(0x7f, new SimpleReader("putActorAtXY"));

		_reader->registerOpcode(0x81, new SimpleReader("faceCutscene"));
		_reader->registerOpcode(0x82, new SimpleReader("animateActor"));
		_reader->registerOpcode(0x83, new SimpleReader("doSentence"));
		_reader->registerOpcode(0x84, new SimpleReader("pickupObject"));
		_reader->registerOpcode(0x8b, new SimpleReader("delay"));
		_reader->registerOpcode(0x8c, new SimpleReader("getActorRoom"));
		_reader->registerOpcode(0x8d, new SimpleReader("getObjectX"));
		_reader->registerOpcode(0x8e, new SimpleReader("getObjectY"));

		_reader->registerOpcode(0x97, new SimpleReader("setObjectName", "s"));
		_reader->registerOpcode(0x99, new SimpleReader("setBoxFlags"));
		_reader->registerOpcode(0x9a, new SimpleReader("createBoxMatrix"));
		SubopcodeReader *res = new SubopcodeReader("res");
		_reader->registerOpcode(0x9b, res);
		res->registerOpcode(100, new SimpleReader("resOps.loadScript"));
		res->registerOpcode(101, new SimpleReader("resOps.loadSound"));
		res->registerOpcode(102, new SimpleReader("resOps.loadCostume"));
		res->registerOpcode(103, new SimpleReader("resOps.loadRoom"));
		res->registerOpcode(104, new SimpleReader("resOps.nukeScript"));
		res->registerOpcode(105, new SimpleReader("resOps.nukeSound"));
		res->registerOpcode(106, new SimpleReader("resOps.nukeCostume"));
		res->registerOpcode(107, new SimpleReader("resOps.nukeRoom"));
		res->registerOpcode(108, new SimpleReader("resOps.lockScript"));
		res->registerOpcode(109, new SimpleReader("resOps.lockSound"));
		res->registerOpcode(110, new SimpleReader("resOps.lockCostume"));
		res->registerOpcode(111, new SimpleReader("resOps.lockRoom"));
		res->registerOpcode(112, new SimpleReader("resOps.unlockScript"));
		res->registerOpcode(113, new SimpleReader("resOps.unlockSound"));
		res->registerOpcode(114, new SimpleReader("resOps.unlockCostume"));
		res->registerOpcode(115, new SimpleReader("resOps.unlockRoom"));
		SubopcodeReader *room = new SubopcodeReader("room");
		_reader->registerOpcode(0x9c, room);
		room->registerOpcode(172, new SimpleReader("roomOps.scroll"));
		room->registerOpcode(174, new SimpleReader("roomOps.setScreen"));
		room->registerOpcode(175, new SimpleReader("roomOps.roomPalette"));
		room->registerOpcode(181, new SimpleReader("roomOps.fade"));
		SubopcodeReader *actor = new SubopcodeReader("actor");
		_reader->registerOpcode(0x9d, actor);
		actor->registerOpcode(76, new SimpleReader("actorOps.setCostume"));
		actor->registerOpcode(83, new SimpleReader("actorOps.init"));
		actor->registerOpcode(84, new SimpleReader("actorOps.setElevation"));
		actor->registerOpcode(86, new SimpleReader("actorOps.setPalette"));
		actor->registerOpcode(87, new SimpleReader("actorOps.setTalkColor"));
		actor->registerOpcode(92, new SimpleReader("actorOps.setScale"));
		actor->registerOpcode(93, new SimpleReader("actorOps.setNeverClip"));
		actor->registerOpcode(94, new SimpleReader("actorOps.setAlwaysZClip"));
		actor->registerOpcode(95, new SimpleReader("actorOps.setIgnoreBoxes"));
		actor->registerOpcode(97, new SimpleReader("actorOps.setAnimationSpeed"));
		actor->registerOpcode(99, new SimpleReader("actorOps.setTalkPos"));
		actor->registerOpcode(0xc5, new SimpleReader("actorOps.setCurActor"));

		_reader->registerOpcode(0xa3, new SimpleReader("getVerbEntryPoint"));
		SubopcodeReader *array = new SubopcodeReader("array");
		_reader->registerOpcode(0xa4, array);
		array->registerOpcode(205, new SimpleReader("array.assignString", "ws"));
		SubopcodeReader *wait = new SubopcodeReader("wait");
		_reader->registerOpcode(0xa9, wait);
		wait->registerOpcode(168, new SeqReader(new SimpleReader("wait.forActor.pushCond"),
												new CondJumpReader("jumpIf", "o4")));
		_reader->registerOpcode(0xad, new SimpleReader("isAnyOf"));
		_reader->registerOpcode(0xaf, new SimpleReader("isActorInBox"));

		_reader->registerOpcode(0xb0, new SimpleReader("delay"));
		_reader->registerOpcode(0xb3, new SimpleReader("stopSentence"));
		// TODO: The various print* opcodes share all subopcodes. Fix this code duplication
		SubopcodeReader *printDebug = new SubopcodeReader("printDebug");
		_reader->registerOpcode(0xb6, printDebug);
		printDebug->registerOpcode(75, new SimpleReader("printDebug.msg", "s"));
		printDebug->registerOpcode(0xfe, new SimpleReader("printDebug.loadDefault"));
		printDebug->registerOpcode(0xff, new SimpleReader("printDebug.saveDefault"));
		SubopcodeReader *print = new SubopcodeReader("print");
		_reader->registerOpcode(0xb7, print);
		print->registerOpcode(75, new SimpleReader("printSystem.msg", "s"));
		print->registerOpcode(0xfe, new SimpleReader("printSystem.begin"));
		print->registerOpcode(0xff, new SimpleReader("printSystem.saveDefault"));
		SubopcodeReader *dimArray = new SubopcodeReader("dimArray");
		_reader->registerOpcode(0xba, new SimpleReader("actorTalk", "s"));
		_reader->registerOpcode(0xbc, dimArray);
		dimArray->registerOpcode(199, new SimpleReader("dimArray.int", "w"));
		dimArray->registerOpcode(204, new SimpleReader("dimArray.undim", "w"));

		_reader->registerOpcode(0xc9, new SimpleReader("kernelSetFunctions.dummy"));
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

	void parseFile(Script* script, const char *filename) {
		ifstream f;
		f.open(filename, ios::binary);
		parseHeader(f);
		while (_reader->readInstruction(f, script, f.tellg()))
			;
	}

};


#endif
