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

		_reader->registerOpcode(0x00, new SimpleReader("pushByte", "b"));
		_reader->registerOpcode(0x01, new SimpleReader("push", "W"));
		_reader->registerOpcode(0x02, new SimpleReader("pushByteVar", "b"));
		_reader->registerOpcode(0x03, new SimpleReader("pushVar", "w"));
		_reader->registerOpcode(0x06, new SimpleReader("byteArrayRead", "b"));
		_reader->registerOpcode(0x07, new SimpleReader("wordArrayRead", "w"));
		_reader->registerOpcode(0x0a, new SimpleReader("byteArrayIndexedRead", "b"));
		_reader->registerOpcode(0x0b, new SimpleReader("wordArrayIndexedRead", "w"));
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
		_reader->registerOpcode(0x16, new SimpleReader("*"));
		_reader->registerOpcode(0x17, new SimpleReader("/"));
		_reader->registerOpcode(0x18, new SimpleReader("&&"));
		_reader->registerOpcode(0x19, new SimpleReader("||"));
		_reader->registerOpcode(0x1a, new SimpleReader("pop"));


		_reader->registerOpcode(0x42, new SimpleReader("writeByteVar", "b"));
		_reader->registerOpcode(0x43, new SimpleReader("writeWordVar", "w"));
		_reader->registerOpcode(0x46, new SimpleReader("byteArrayWrite", "b"));
		_reader->registerOpcode(0x47, new SimpleReader("wordArrayWrite", "w"));
		_reader->registerOpcode(0x4a, new SimpleReader("byteArrayIndexedWrite", "b"));
		_reader->registerOpcode(0x4b, new SimpleReader("wordArrayIndexedWrite", "w"));
		_reader->registerOpcode(0x4e, new SimpleReader("byteVarInc", "b"));
		_reader->registerOpcode(0x4f, new SimpleReader("wordVarInc", "w"));

		_reader->registerOpcode(0x52, new SimpleReader("byteArrayInc", "b"));
		_reader->registerOpcode(0x53, new SimpleReader("wordArrayInc", "w"));
		_reader->registerOpcode(0x56, new SimpleReader("byteVarDec", "b"));
		_reader->registerOpcode(0x57, new SimpleReader("wordVarDec", "w"));
		_reader->registerOpcode(0x5a, new SimpleReader("byteArrayDec", "b"));
		_reader->registerOpcode(0x5b, new SimpleReader("wordArrayDec", "w"));
		_reader->registerOpcode(0x5c, new SeqReader(new SimpleReader("not"), new CondJumpReader("jumpIfNot", "o3")));
		_reader->registerOpcode(0x5d, new CondJumpReader("jumpIfNot", "o3"));
		_reader->registerOpcode(0x5e, new SimpleReader("startScript"));
		_reader->registerOpcode(0x5f, new SimpleReader("startScriptQuick"));

		_reader->registerOpcode(0x60, new SimpleReader("startObject"));
		_reader->registerOpcode(0x61, new SimpleReader("drawObject"));
		_reader->registerOpcode(0x62, new SimpleReader("drawObjectAt"));
		_reader->registerOpcode(0x63, new SimpleReader("drawBlastObject"));
		_reader->registerOpcode(0x64, new SimpleReader("setBlastObjectWindow"));
		_reader->registerOpcode(0x65, new SimpleReader("stopObjectCodeA"));
		_reader->registerOpcode(0x66, new SimpleReader("stopObjectCodeB"));
		_reader->registerOpcode(0x67, new SimpleReader("endCutscene"));
		_reader->registerOpcode(0x68, new SimpleReader("cutscene"));
		_reader->registerOpcode(0x69, new SimpleReader("stopMusic"));
		_reader->registerOpcode(0x6a, new SimpleReader("freezeUnfreeze"));
		SubopcodeReader *cursor = new SubopcodeReader("cursor"); _reader->registerOpcode(0x6b, cursor);
		_reader->registerOpcode(0x6c, new SimpleReader("breakHere"));	// = yield
		_reader->registerOpcode(0x6d, new SimpleReader("classOfIs"));
		_reader->registerOpcode(0x6e, new SimpleReader("setClass"));
		_reader->registerOpcode(0x6f, new SimpleReader("getState"));

		_reader->registerOpcode(0x70, new SimpleReader("setState"));
		_reader->registerOpcode(0x71, new SimpleReader("setOwner"));
		_reader->registerOpcode(0x72, new SimpleReader("getOwner"));
		_reader->registerOpcode(0x73, new JumpReader("jump", "o3"));
		_reader->registerOpcode(0x74, new SimpleReader("startSound"));
		_reader->registerOpcode(0x75, new SimpleReader("stopSound"));
		_reader->registerOpcode(0x76, new SimpleReader("startMusic"));
		_reader->registerOpcode(0x77, new SimpleReader("stopObjectScript"));
		_reader->registerOpcode(0x78, new SimpleReader("panCameraTo"));
		_reader->registerOpcode(0x79, new SimpleReader("actorFollowCamera"));
		_reader->registerOpcode(0x7a, new SimpleReader("setCameraAt"));
		_reader->registerOpcode(0x7b, new SimpleReader("loadRoom"));
		_reader->registerOpcode(0x7c, new SimpleReader("stopScript"));
		_reader->registerOpcode(0x7d, new SimpleReader("walkActorToObj"));
		_reader->registerOpcode(0x7e, new SimpleReader("walkActorTo"));
		_reader->registerOpcode(0x7f, new SimpleReader("putActorAtXY"));

		_reader->registerOpcode(0x80, new SimpleReader("putActorAtObject"));
		_reader->registerOpcode(0x81, new SimpleReader("faceActor"));
		_reader->registerOpcode(0x82, new SimpleReader("animateActor"));
		_reader->registerOpcode(0x83, new SimpleReader("doSentence"));
		_reader->registerOpcode(0x84, new SimpleReader("pickupObject"));
		_reader->registerOpcode(0x85, new SimpleReader("loadRoomWithEgo"));
		_reader->registerOpcode(0x87, new SimpleReader("getRandomNumber"));
		_reader->registerOpcode(0x88, new SimpleReader("getRandomNumberRange"));
		_reader->registerOpcode(0x8a, new SimpleReader("getActorMoving"));
		_reader->registerOpcode(0x8b, new SimpleReader("delay"));
		_reader->registerOpcode(0x8c, new SimpleReader("getActorRoom"));
		_reader->registerOpcode(0x8d, new SimpleReader("getObjectX"));
		_reader->registerOpcode(0x8e, new SimpleReader("getObjectY"));
		_reader->registerOpcode(0x8f, new SimpleReader("getObjectOldDir"));

		_reader->registerOpcode(0x90, new SimpleReader("getActorWalkBox"));
		_reader->registerOpcode(0x91, new SimpleReader("getActorCostume"));
		_reader->registerOpcode(0x92, new SimpleReader("findInventory"));
		_reader->registerOpcode(0x93, new SimpleReader("getInventoryCount"));
		_reader->registerOpcode(0x94, new SimpleReader("getVerbFromXY"));
		_reader->registerOpcode(0x95, new SimpleReader("beginOverride", "bw")); // TODO: how to handle this?
		_reader->registerOpcode(0x96, new SimpleReader("endOverride"));
		_reader->registerOpcode(0x97, new SimpleReader("setObjectName", "s"));
		_reader->registerOpcode(0x98, new SimpleReader("isSoundRunning"));
		_reader->registerOpcode(0x99, new SimpleReader("setBoxFlags"));
		_reader->registerOpcode(0x9a, new SimpleReader("createBoxMatrix"));
		SubopcodeReader *res   = new SubopcodeReader("res");   _reader->registerOpcode(0x9b, res);
		SubopcodeReader *room  = new SubopcodeReader("room");  _reader->registerOpcode(0x9c, room);
		SubopcodeReader *actor = new SubopcodeReader("actor"); _reader->registerOpcode(0x9d, actor);
		SubopcodeReader *verb  = new SubopcodeReader("verb");  _reader->registerOpcode(0x9e, verb);
		_reader->registerOpcode(0x9f, new SimpleReader("getActorFromXY"));

		_reader->registerOpcode(0xa0, new SimpleReader("findObject"));
		_reader->registerOpcode(0xa1, new SimpleReader("pseudoRoom"));
		_reader->registerOpcode(0xa2, new SimpleReader("getActorElevation"));
		_reader->registerOpcode(0xa3, new SimpleReader("getVerbEntryPoint"));
		SubopcodeReader *array   = new SubopcodeReader("array");   _reader->registerOpcode(0xa4, array);
		SubopcodeReader *srVerbs = new SubopcodeReader("srVerbs"); _reader->registerOpcode(0xa5, srVerbs);
		_reader->registerOpcode(0xa6, new SimpleReader("drawBox"));
		_reader->registerOpcode(0xa7, new SimpleReader("pop"));
		_reader->registerOpcode(0xa8, new SimpleReader("getActorWidth"));
		SubopcodeReader *wait    = new SubopcodeReader("wait");    _reader->registerOpcode(0xa9, wait);
		_reader->registerOpcode(0xaa, new SimpleReader("getActorScaleX"));
		_reader->registerOpcode(0xab, new SimpleReader("getActorAnimCounter"));
		_reader->registerOpcode(0xac, new SimpleReader("soundKludge"));
		_reader->registerOpcode(0xad, new SimpleReader("isAnyOf"));
		SubopcodeReader *system  = new SubopcodeReader("system");  _reader->registerOpcode(0xae, system);
		_reader->registerOpcode(0xaf, new SimpleReader("isActorInBox"));

		_reader->registerOpcode(0xb0, new SimpleReader("delay"));
		_reader->registerOpcode(0xb1, new SimpleReader("delaySeconds"));
		_reader->registerOpcode(0xb2, new SimpleReader("delayMinutes"));
		_reader->registerOpcode(0xb3, new SimpleReader("stopSentence"));
		SubopcodeReader *printLine   = new SubopcodeReader("printLine");   _reader->registerOpcode(0xb4, printLine);
		SubopcodeReader *printText   = new SubopcodeReader("printText");   _reader->registerOpcode(0xb5, printText);
		SubopcodeReader *printDebug  = new SubopcodeReader("printDebug");  _reader->registerOpcode(0xb6, printDebug);
		SubopcodeReader *printSystem = new SubopcodeReader("printSystem"); _reader->registerOpcode(0xb7, printSystem);
		SubopcodeReader *printActor  = new SubopcodeReader("printActor");  _reader->registerOpcode(0xb8, printActor);
		SubopcodeReader *printEgo    = new SubopcodeReader("printEgo");    _reader->registerOpcode(0xb9, printEgo);
		_reader->registerOpcode(0xba, new SimpleReader("talkActor", "s"));
		_reader->registerOpcode(0xbb, new SimpleReader("talkEgo", "s"));
		SubopcodeReader *dimArray = new SubopcodeReader("dimArray");       _reader->registerOpcode(0xbc, dimArray);
		_reader->registerOpcode(0xbd, new SimpleReader("dummy"));
		_reader->registerOpcode(0xbe, new SimpleReader("startObjectQuick"));
		_reader->registerOpcode(0xbf, new SimpleReader("startScriptQuick2"));

		_reader->registerOpcode(0xc0, new SimpleReader("dim2dimArray"));
		_reader->registerOpcode(0xc4, new SimpleReader("abs"));
		_reader->registerOpcode(0xc5, new SimpleReader("distObjectObject"));
		_reader->registerOpcode(0xc6, new SimpleReader("distObjectPt"));
		_reader->registerOpcode(0xc7, new SimpleReader("distPtPt"));
		_reader->registerOpcode(0xc8, new SimpleReader("kernelGetFunctions.*"));
		_reader->registerOpcode(0xc9, new SimpleReader("kernelSetFunctions.*"));
		_reader->registerOpcode(0xca, new SimpleReader("delayFrames"));
		_reader->registerOpcode(0xcb, new SimpleReader("pickOneOf"));
		_reader->registerOpcode(0xcc, new SimpleReader("pickOneOfDefault"));
		_reader->registerOpcode(0xcd, new SimpleReader("StampObject"));

		_reader->registerOpcode(0xd0, new SimpleReader("getDateTime"));
		_reader->registerOpcode(0xd1, new SimpleReader("stopTalking"));
		_reader->registerOpcode(0xd2, new SimpleReader("getAnimateVariable"));
		_reader->registerOpcode(0xd4, new SimpleReader("shuffle", "w"));
		_reader->registerOpcode(0xd5, new SimpleReader("jumpToScript"));
		_reader->registerOpcode(0xd6, new SimpleReader("band"));
		_reader->registerOpcode(0xd7, new SimpleReader("bor"));
		_reader->registerOpcode(0xd8, new SimpleReader("isRoomScriptRunning"));
		_reader->registerOpcode(0xdd, new SimpleReader("findAllObjects"));

		_reader->registerOpcode(0xe1, new SimpleReader("getPixel"));
		_reader->registerOpcode(0xe3, new SimpleReader("pickVarRandom", "w"));
		_reader->registerOpcode(0xe4, new SimpleReader("setBoxSet"));
		_reader->registerOpcode(0xec, new SimpleReader("getActorLayer"));
		_reader->registerOpcode(0xed, new SimpleReader("getObjectNewDir"));

		cursor->registerOpcode(0x90, new SimpleReader("cursor.cursorOn"));
		cursor->registerOpcode(0x91, new SimpleReader("cursor.cursorOff"));
		cursor->registerOpcode(0x92, new SimpleReader("cursor.userPutOn"));
		cursor->registerOpcode(0x93, new SimpleReader("cursor.userPutOff"));
		cursor->registerOpcode(0x94, new SimpleReader("cursor.softOn"));
		cursor->registerOpcode(0x95, new SimpleReader("cursor.softOff"));
		cursor->registerOpcode(0x96, new SimpleReader("cursor.userputSoftOn"));
		cursor->registerOpcode(0x97, new SimpleReader("cursor.userputSoftOff"));
		cursor->registerOpcode(0x99, new SimpleReader("cursor.setCursorImg"));
		cursor->registerOpcode(0x9a, new SimpleReader("cursor.setCursorHotspot"));
		cursor->registerOpcode(0x9c, new SimpleReader("cursor.initCharset"));
		cursor->registerOpcode(0x9d, new SimpleReader("cursor.setCharsetColor"));
		cursor->registerOpcode(0xd6, new SimpleReader("cursor.setTransparent"));

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
		res->registerOpcode(116, new SimpleReader("resOps.clearHeap"));
		res->registerOpcode(117, new SimpleReader("resOps.loadCharset"));
		res->registerOpcode(118, new SimpleReader("resOps.nukeCharset"));
		res->registerOpcode(119, new SimpleReader("resOps.loadObject"));

		room->registerOpcode(172, new SimpleReader("roomOps.scroll"));
		room->registerOpcode(174, new SimpleReader("roomOps.setScreen"));
		room->registerOpcode(175, new SimpleReader("roomOps.roomPalette"));
		room->registerOpcode(176, new SimpleReader("roomOps.shakeOn"));
		room->registerOpcode(177, new SimpleReader("roomOps.shakeOff"));
		room->registerOpcode(179, new SimpleReader("roomOps.darkenPalette"));
		room->registerOpcode(180, new SimpleReader("roomOps.saveGame"));
		room->registerOpcode(181, new SimpleReader("roomOps.fade"));
		room->registerOpcode(182, new SimpleReader("roomOps.rgbIntensity"));
		room->registerOpcode(183, new SimpleReader("roomOps.shadow"));
		room->registerOpcode(184, new SimpleReader("roomOps.saveString"));
		room->registerOpcode(185, new SimpleReader("roomOps.loadString"));
		room->registerOpcode(186, new SimpleReader("roomOps.transform"));
		room->registerOpcode(187, new SimpleReader("roomOps.cycleSpeed"));
		room->registerOpcode(213, new SimpleReader("roomOps.newPalette"));

		actor->registerOpcode(76, new SimpleReader("actorOps.setCostume"));
		actor->registerOpcode(77, new SimpleReader("actorOps.stepDist"));
		actor->registerOpcode(78, new SimpleReader("actorOps.sound"));
		actor->registerOpcode(79, new SimpleReader("actorOps.walkAnimation"));
		actor->registerOpcode(80, new SimpleReader("actorOps.talkAnimation"));
		actor->registerOpcode(81, new SimpleReader("actorOps.standAnimation"));
		actor->registerOpcode(82, new SimpleReader("actorOps.animation"));
		actor->registerOpcode(83, new SimpleReader("actorOps.init"));
		actor->registerOpcode(84, new SimpleReader("actorOps.setElevation"));
		actor->registerOpcode(85, new SimpleReader("actorOps.animationDefault"));
		actor->registerOpcode(86, new SimpleReader("actorOps.setPalette"));
		actor->registerOpcode(87, new SimpleReader("actorOps.setTalkColor"));
		actor->registerOpcode(88, new SimpleReader("actorOps.setName", "s"));
		actor->registerOpcode(89, new SimpleReader("actorOps.initAnimation"));
		actor->registerOpcode(91, new SimpleReader("actorOps.actorWidth"));
		actor->registerOpcode(92, new SimpleReader("actorOps.setScale"));
		actor->registerOpcode(93, new SimpleReader("actorOps.setNeverClip"));
		actor->registerOpcode(94, new SimpleReader("actorOps.setAlwaysZClip"));
		actor->registerOpcode(95, new SimpleReader("actorOps.setIgnoreBoxes"));
		actor->registerOpcode(96, new SimpleReader("actorOps.setFollowBoxes"));
		actor->registerOpcode(97, new SimpleReader("actorOps.setAnimationSpeed"));
		actor->registerOpcode(98, new SimpleReader("actorOps.shadow"));
		actor->registerOpcode(99, new SimpleReader("actorOps.setTalkPos"));
		actor->registerOpcode(197, new SimpleReader("actorOps.setCurActor"));
		actor->registerOpcode(198, new SimpleReader("actorOps.actorVariable"));
		actor->registerOpcode(215, new SimpleReader("actorOps.ignoreTurnsOn"));
		actor->registerOpcode(216, new SimpleReader("actorOps.ignoreTurnsOff"));
		actor->registerOpcode(217, new SimpleReader("actorOps.actorNew"));
		actor->registerOpcode(225, new SimpleReader("actorOps.setAlwaysZClip"));
		actor->registerOpcode(227, new SimpleReader("actorOps.actorDepth"));
		actor->registerOpcode(228, new SimpleReader("actorOps.actorWalkScript"));
		actor->registerOpcode(229, new SimpleReader("actorOps.actorStop"));
		actor->registerOpcode(230, new SimpleReader("actorOps.setDirection"));
		actor->registerOpcode(231, new SimpleReader("actorOps.turnToDirection"));
		actor->registerOpcode(233, new SimpleReader("actorOps.actorWalkPause"));
		actor->registerOpcode(234, new SimpleReader("actorOps.actorWalkResume"));
		actor->registerOpcode(235, new SimpleReader("actorOps.actorTalkScript"));

		verb->registerOpcode(124, new SimpleReader("verbOps.setImage"));
		verb->registerOpcode(125, new SimpleReader("verbOps.setName", "s"));
		verb->registerOpcode(126, new SimpleReader("verbOps.setColor"));
		verb->registerOpcode(127, new SimpleReader("verbOps.setHiColor"));
		verb->registerOpcode(128, new SimpleReader("verbOps.setXY"));
		verb->registerOpcode(129, new SimpleReader("verbOps.setOn"));
		verb->registerOpcode(130, new SimpleReader("verbOps.setOff"));
		verb->registerOpcode(131, new SimpleReader("verbOps.kill"));
		verb->registerOpcode(132, new SimpleReader("verbOps.init"));
		verb->registerOpcode(133, new SimpleReader("verbOps.setDimColor"));
		verb->registerOpcode(134, new SimpleReader("verbOps.setDimmed"));
		verb->registerOpcode(135, new SimpleReader("verbOps.setKey"));
		verb->registerOpcode(136, new SimpleReader("verbOps.setCenter"));
		verb->registerOpcode(137, new SimpleReader("verbOps.setToString"));
		verb->registerOpcode(139, new SimpleReader("verbOps.setToObject"));
		verb->registerOpcode(140, new SimpleReader("verbOps.setBkColor"));
		verb->registerOpcode(196, new SimpleReader("verbOps.setCurVerb"));
		verb->registerOpcode(255, new SimpleReader("verbOps.redraw"));

		array->registerOpcode(205, new SimpleReader("array.assignString", "ws"));
		array->registerOpcode(208, new SimpleReader("array.assignIntList", "w"));
		array->registerOpcode(212, new SimpleReader("array.assign2dimList", "w"));

		srVerbs->registerOpcode(141, new SimpleReader("saveRestoreVerbs.saveVerbs"));
		srVerbs->registerOpcode(142, new SimpleReader("saveRestoreVerbs.restoreVerbs"));
		srVerbs->registerOpcode(143, new SimpleReader("saveRestoreVerbs.deleteVerbs"));

		wait->registerOpcode(168, new SeqReader(new SimpleReader("wait.forActor.pushCondNeg"),	new CondJumpReader("jumpIfNot", "o4")));
		wait->registerOpcode(169, new SimpleReader("wait.forMessage"));
		wait->registerOpcode(170, new SimpleReader("wait.forCamera"));
		wait->registerOpcode(171, new SimpleReader("wait.forSentence"));
		wait->registerOpcode(226, new SeqReader(new SimpleReader("wait.forAnimation.pushCondNeg"),	new CondJumpReader("jumpIfNot", "o4")));
		wait->registerOpcode(232, new SeqReader(new SimpleReader("wait.forTurn.pushCondNeg"),	new CondJumpReader("jumpIfNot", "o4")));

		system->registerOpcode(158, new SimpleReader("system.restart"));
		system->registerOpcode(159, new SimpleReader("system.pause"));
		system->registerOpcode(160, new SimpleReader("system.quit"));

		// TODO: The various print* opcodes share all subopcodes. Fix this code duplication
		printLine->registerOpcode(65, new SimpleReader("printLine.at"));
		printLine->registerOpcode(66, new SimpleReader("printLine.color"));
		printLine->registerOpcode(67, new SimpleReader("printLine.clipped"));
		printLine->registerOpcode(69, new SimpleReader("printLine.center"));
		printLine->registerOpcode(71, new SimpleReader("printLine.left"));
		printLine->registerOpcode(72, new SimpleReader("printLine.overhead"));
		printLine->registerOpcode(74, new SimpleReader("printLine.mumble"));
		printLine->registerOpcode(75, new SimpleReader("printLine.msg", "s"));
		printLine->registerOpcode(0xfe, new SimpleReader("printLine.begin"));
		printLine->registerOpcode(0xff, new SimpleReader("printLine.saveDefault"));

		printText->registerOpcode(65, new SimpleReader("printText.at"));
		printText->registerOpcode(66, new SimpleReader("printText.color"));
		printText->registerOpcode(67, new SimpleReader("printText.clipped"));
		printText->registerOpcode(69, new SimpleReader("printText.center"));
		printText->registerOpcode(71, new SimpleReader("printText.left"));
		printText->registerOpcode(72, new SimpleReader("printText.overhead"));
		printText->registerOpcode(74, new SimpleReader("printText.mumble"));
		printText->registerOpcode(75, new SimpleReader("printText.msg", "s"));
		printText->registerOpcode(0xfe, new SimpleReader("printText.begin"));
		printText->registerOpcode(0xff, new SimpleReader("printText.saveDefault"));

		printDebug->registerOpcode(65, new SimpleReader("printDebug.at"));
		printDebug->registerOpcode(66, new SimpleReader("printDebug.color"));
		printDebug->registerOpcode(67, new SimpleReader("printDebug.clipped"));
		printDebug->registerOpcode(69, new SimpleReader("printDebug.center"));
		printDebug->registerOpcode(71, new SimpleReader("printDebug.left"));
		printDebug->registerOpcode(72, new SimpleReader("printDebug.overhead"));
		printDebug->registerOpcode(74, new SimpleReader("printDebug.mumble"));
		printDebug->registerOpcode(75, new SimpleReader("printDebug.msg", "s"));
		printDebug->registerOpcode(0xfe, new SimpleReader("printDebug.begin"));
		printDebug->registerOpcode(0xff, new SimpleReader("printDebug.saveDefault"));

		printSystem->registerOpcode(65, new SimpleReader("printSystem.at"));
		printSystem->registerOpcode(66, new SimpleReader("printSystem.color"));
		printSystem->registerOpcode(67, new SimpleReader("printSystem.clipped"));
		printSystem->registerOpcode(69, new SimpleReader("printSystem.center"));
		printSystem->registerOpcode(71, new SimpleReader("printSystem.left"));
		printSystem->registerOpcode(72, new SimpleReader("printSystem.overhead"));
		printSystem->registerOpcode(74, new SimpleReader("printSystem.mumble"));
		printSystem->registerOpcode(75, new SimpleReader("printSystem.msg", "s"));
		printSystem->registerOpcode(0xfe, new SimpleReader("printSystem.begin"));
		printSystem->registerOpcode(0xff, new SimpleReader("printSystem.saveDefault"));

		printActor->registerOpcode(65, new SimpleReader("printActor.at"));
		printActor->registerOpcode(66, new SimpleReader("printActor.color"));
		printActor->registerOpcode(67, new SimpleReader("printActor.clipped"));
		printActor->registerOpcode(69, new SimpleReader("printActor.center"));
		printActor->registerOpcode(71, new SimpleReader("printActor.left"));
		printActor->registerOpcode(72, new SimpleReader("printActor.overhead"));
		printActor->registerOpcode(74, new SimpleReader("printActor.mumble"));
		printActor->registerOpcode(75, new SimpleReader("printActor.msg", "s"));
		printActor->registerOpcode(0xfe, new SimpleReader("printActor.begin"));
		printActor->registerOpcode(0xff, new SimpleReader("printActor.saveDefault"));

		printEgo->registerOpcode(65, new SimpleReader("printEgo.at"));
		printEgo->registerOpcode(66, new SimpleReader("printEgo.color"));
		printEgo->registerOpcode(67, new SimpleReader("printEgo.clipped"));
		printEgo->registerOpcode(69, new SimpleReader("printEgo.center"));
		printEgo->registerOpcode(71, new SimpleReader("printEgo.left"));
		printEgo->registerOpcode(72, new SimpleReader("printEgo.overhead"));
		printEgo->registerOpcode(74, new SimpleReader("printEgo.mumble"));
		printEgo->registerOpcode(75, new SimpleReader("printEgo.msg", "s"));
		printEgo->registerOpcode(0xfe, new SimpleReader("printEgo.begin"));
		printEgo->registerOpcode(0xff, new SimpleReader("printEgo.saveDefault"));

		dimArray->registerOpcode(199, new SimpleReader("dimArray.int", "w"));
		dimArray->registerOpcode(200, new SimpleReader("dimArray.bit", "w"));
		dimArray->registerOpcode(201, new SimpleReader("dimArray.nibble", "w"));
		dimArray->registerOpcode(202, new SimpleReader("dimArray.byte", "w"));
		dimArray->registerOpcode(203, new SimpleReader("dimArray.string", "w"));
		dimArray->registerOpcode(204, new SimpleReader("dimArray.undim", "w"));
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
			for (int code = f.get(); code != 0; code = f.get()) {
				uint16 offset = read_le_uint16(f);
				fprintf(stderr, "%2x - %.4x\n", code, offset);
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
		std::streamoff start = f.tellg();
		while (_reader->readInstruction(f, script, (uint32) (f.tellg()-start)))
			;
	}

};


#endif
