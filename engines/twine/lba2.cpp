/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "engines/twine/lba2.h"
#include "engines/twine/shared.h"

static int decompileLBA2MoveScript(int actor, const uint8 *moveScript, int16 moveScriptSize) {
	return 0;
}

static int decompileLBA2LifeScript(int actor, const uint8 *moveScript, int16 moveScriptSize) {
	return 0;
}

int decompileLBA2(const uint8 *data, int size) {
	Common::MemoryReadStream stream(data, size);

	uint8 sceneTextBank = stream.readByte();
	uint8 currentGameOverScene = stream.readByte();
	stream.skip(4);

	int16 alphaLight = stream.readSint16LE();
	int16 betaLight = stream.readSint16LE();

	uint8 isOutsideScene = stream.readByte();

	uint16 sampleAmbiance[4];
	uint16 sampleRepeat[4];
	uint16 sampleRound[4];
	uint16 sampleFrequency[4];
	uint16 sampleVolume[4];

	for (int i = 0; i < 4; ++i) {
		sampleAmbiance[i] = stream.readUint16LE();
		sampleRepeat[i] = stream.readUint16LE();
		sampleRound[i] = stream.readUint16LE();
		sampleFrequency[i] = stream.readUint16LE();
		sampleVolume[i] = stream.readUint16LE();
	}

	uint16 sampleMinDelay = stream.readUint16LE();
	uint16 sampleMinDelayRnd = stream.readUint16LE();

	uint8 sceneMusic = stream.readByte();

	// load hero properties
	int16 sceneHeroPosx = stream.readSint16LE();
	int16 sceneHeroPosy = stream.readSint16LE();
	int16 sceneHeroPosz = stream.readSint16LE();

	printf("Actor 0\n");

	int16 moveScriptSize = stream.readSint16LE();
	printf(" - move script: %i\n", (int)moveScriptSize);
	const uint8 *moveScript = data + stream.pos();
	stream.skip(moveScriptSize);
	decompileLBA2MoveScript(0, moveScript, moveScriptSize);

	int16 lifeScriptSize = stream.readSint16LE();
	printf(" - life script: %i\n", (int)lifeScriptSize);
	const uint8 *lifeScript = data + stream.pos();
	stream.skip(lifeScriptSize);
	decompileLBA2LifeScript(0, lifeScript, lifeScriptSize);

	int16 sceneNumActors = stream.readSint16LE();
	for (int32 a = 1; a < sceneNumActors; a++) {
		printf("\n");
		uint32 staticflags = stream.readUint32LE();
		int16 body = stream.readSint16LE();
		int16 genBody = stream.readSint16LE();
		uint8 genAnim = stream.readByte();
		int16 sprite = stream.readSint16LE();
		int16 posx = stream.readSint16LE();
		int16 posy = stream.readSint16LE();
		int16 posz = stream.readSint16LE();
		uint8 strengthOfHit = stream.readByte();
		uint16 dynamicFlags = stream.readUint16LE();
		uint16 beta = stream.readSint16LE();
		uint16 speed = stream.readSint16LE();
		uint8 controlMode = stream.readByte();
		int16 cropLeft = stream.readSint16LE();
		int16 cropTop = stream.readSint16LE();
		int16 cropRight = stream.readSint16LE();
		int16 cropBottom = stream.readSint16LE();
		int16 bonusAmount = stream.readSint16LE();
		uint8 talkColor = stream.readByte();
		printf("Actor %i\n", a);
		if (staticflags & 0x040000) { // bHasSpriteAnim3D
			int32 spriteAnim3DNumber = stream.readSint32LE();
			int16 spriteSizeHit = stream.readSint16LE();
			printf(" - spriteAnim3DNumber: %i\n", (int)spriteAnim3DNumber);
			printf(" - spriteSizeHit: %i\n", (int)spriteSizeHit);
		}
		uint8 armor = stream.readByte();
		uint8 lifePoints = stream.readByte();

		printf(" - staticflags: %i\n", (int)staticflags);
		printf(" - body: %i\n", (int)body);
		printf(" - genBody: %i\n", (int)genBody);
		printf(" - genAnim: %i\n", (int)genAnim);
		printf(" - sprite: %i\n", (int)sprite);
		printf(" - posx: %i\n", (int)posx);
		printf(" - posy: %i\n", (int)posy);
		printf(" - posz: %i\n", (int)posz);
		printf(" - strengthOfHit: %i\n", (int)strengthOfHit);
		printf(" - dynamicFlags: %i\n", (int)dynamicFlags);
		printf(" - beta: %i\n", (int)beta);
		printf(" - speed: %i\n", (int)speed);
		printf(" - controlMode: %i\n", (int)controlMode);
		printf(" - cropLeft: %i\n", (int)cropLeft);
		printf(" - cropTop: %i\n", (int)cropTop);
		printf(" - cropRight: %i\n", (int)cropRight);
		printf(" - cropBottom: %i\n", (int)cropBottom);
		printf(" - bonusAmount: %i\n", (int)bonusAmount);
		printf(" - talkColor: %i\n", (int)talkColor);
		printf(" - armor: %i\n", (int)armor);
		printf(" - lifePoints: %i\n", (int)lifePoints);

		moveScriptSize = (int16)stream.readUint16LE();
		printf(" - move script: %i\n", (int)moveScriptSize);
		moveScript = data + stream.pos();
		stream.skip(moveScriptSize);
		decompileLBA2MoveScript(0, moveScript, moveScriptSize);

		lifeScriptSize = (int16)stream.readUint16LE();
		printf(" - life script: %i\n", (int)lifeScriptSize);
		lifeScript = data + stream.pos();
		stream.skip(lifeScriptSize);
		decompileLBA2LifeScript(0, lifeScript, lifeScriptSize);
	}

	int16 sceneNumZones = stream.readSint16LE();
	for (int32 i = 0; i < sceneNumZones; i++) {
		int32 zoneminsx = stream.readSint32LE();
		int32 zoneminsy = stream.readSint32LE();
		int32 zoneminsz = stream.readSint32LE();
		int32 zonemaxsx = stream.readSint32LE();
		int32 zonemaxsy = stream.readSint32LE();
		int32 zonemaxsz = stream.readSint32LE();
		int32 info0 = stream.readSint32LE();
		int32 info1 = stream.readSint32LE();
		int32 info2 = stream.readSint32LE();
		int32 info3 = stream.readSint32LE();
		int32 info4 = stream.readSint32LE();
		int32 info5 = stream.readSint32LE();
		int32 info6 = stream.readSint32LE();
		int32 info7 = stream.readSint32LE();
		uint16 zonetype = stream.readUint16LE();
		int16 zonenum = stream.readSint16LE();
		printf("Zone: %i\n", i);
		printf(" - zoneminsx: %i\n", zoneminsx);
		printf(" - zoneminsy: %i\n", zoneminsy);
		printf(" - zoneminsz: %i\n", zoneminsz);
		printf(" - zonemaxsx: %i\n", zonemaxsx);
		printf(" - zonemaxsy: %i\n", zonemaxsy);
		printf(" - zonemaxsz: %i\n", zonemaxsz);
		printf(" - zonetype: %i\n", zonetype);
		printf(" - zonenum: %i\n", zonenum);
		printf(" - info0: %i\n", info0);
		printf(" - info1: %i\n", info1);
		printf(" - info2: %i\n", info2);
		printf(" - info3: %i\n", info3);
		printf(" - info4: %i\n", info4);
		printf(" - info5: %i\n", info5);
		printf(" - info6: %i\n", info6);
		printf(" - info7: %i\n", info7);
		printf(" - zonetype: %i\n", zonetype);
		printf(" - zonenum: %i\n", zonenum);
	}

	int16 sceneNumTracks = stream.readSint16LE();
	for (int16 i = 0; i < sceneNumTracks; i++) {
		int32 pointx = stream.readSint32LE();
		int32 pointy = stream.readSint32LE();
		int32 pointz = stream.readSint32LE();
		printf("Track: %i\n", i);
		printf(" - pointx: %i\n", pointx);
		printf(" - pointy: %i\n", pointy);
		printf(" - pointz: %i\n", pointz);
	}

	uint16 sceneNumPatches = stream.readUint16LE();
	for (uint16 i = 0; i < sceneNumPatches; i++) {
		uint16 psize = stream.readUint16LE();
		uint16 poffset = stream.readUint16LE();
		printf("Patch: %i\n", i);
		printf(" - size: %u\n", psize);
		printf(" - offset: %u\n", poffset);
	}

	printf("Scene\n");
	printf(" - sceneTextBank: %i\n", sceneTextBank);
	printf(" - isOutsideScene: %i\n", isOutsideScene);
	printf(" - currentGameOverScene: %i\n", currentGameOverScene);
	printf(" - alphaLight: %i\n", alphaLight);
	printf(" - betaLight: %i\n", betaLight);
	printf(" - sampleFrequency: %i %i %i %i\n", sampleFrequency[0], sampleFrequency[1], sampleFrequency[2], sampleFrequency[3]);
	printf(" - sampleVolume: %i %i %i %i\n", sampleVolume[0], sampleVolume[1], sampleVolume[2], sampleVolume[3]);
	printf(" - sampleAmbiance: %i %i %i %i\n", sampleAmbiance[0], sampleAmbiance[1], sampleAmbiance[2], sampleAmbiance[3]);
	printf(" - sampleRepeat: %i %i %i %i\n", sampleRepeat[0], sampleRepeat[1], sampleRepeat[2], sampleRepeat[3]);
	printf(" - sampleRound: %i %i %i %i\n", sampleRound[0], sampleRound[1], sampleRound[2], sampleRound[3]);
	printf(" - sampleMinDelay: %i\n", sampleMinDelay);
	printf(" - sampleMinDelayRnd: %i\n", sampleMinDelayRnd);
	printf(" - sceneMusic: %i\n", sceneMusic);
	printf(" - sceneHeroPosx: %i\n", sceneHeroPosx);
	printf(" - sceneHeroPosy: %i\n", sceneHeroPosy);
	printf(" - sceneHeroPosz: %i\n", sceneHeroPosz);

	if (stream.err()) {
		return 1;
	}
	return 0;
}
