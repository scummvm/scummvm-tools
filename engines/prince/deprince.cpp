/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Prince script decompiler */

#include "common/file.h"
#include "common/endian.h"
#include "common/util.h"

#include "utils.h"
#include "flags.h"

#include <assert.h>

static const int16 kMaxRooms = 60;

struct OpCodes {
	const char *name;
	const char *params;
	bool nf;
} static opcodes[] = {
	{ "O_WAITFOREVER", "", true },
	{ "O_BLACKPALETTE", "", false },
	{ "O_SETUPPALETTE", "", false },
	{ "O_INITROOM", "f", true },
	{ "O_SETSAMPLE", "fs", false },
	{ "O_FREESAMPLE", "f", false },
	{ "O_PLAYSAMPLE", "fh", false },
	{ "O_PUTOBJECT", "fff", false },
	{ "O_REMOBJECT", "ff", false },
	{ "O_SHOWANIM", "ff", false },
	{ "O_CHECKANIMEND", "f", false },
	{ "O_FREEANIM", "f", false },
	{ "O_CHECKANIMFRAME", "ff", false },
	{ "O_PUTBACKANIM", "ffi", false },
	{ "O_REMBACKANIM", "ff", false },
	{ "O_CHECKBACKANIMFRAME", "ff", false },
	{ "O_FREEALLSAMPLES", "r", false },
	{ "O_SETMUSIC", "h", false },
	{ "O_STOPMUSIC", "", false },
	{ "O__WAIT", "f", false },
	{ "O_UPDATEOFF", "r", false },
	{ "O_UPDATEON", "r", false },
	{ "O_UPDATE", "r", false },
	{ "O_CLS", "r", false },
	{ "O__CALL", "o", false },
	{ "O_RETURN", "", false },
	{ "O_GO", "o", false },
	{ "O_BACKANIMUPDATEOFF", "f", false },
	{ "O_BACKANIMUPDATEON", "f", false },
	{ "O_CHANGECURSOR", "f", false },
	{ "O_CHANGEANIMTYPE", "r", false },
	{ "O__SETFLAG", "df", false },
	{ "O_COMPARE", "df", false },
	{ "O_JUMPZ", "o", false },
	{ "O_JUMPNZ", "o", false },
	{ "O_EXIT", "", true },
	{ "O_ADDFLAG", "df", false },
	{ "O_TALKANIM", "ff", false },
	{ "O_SUBFLAG", "df", false },
	{ "O_SETSTRING", "i", false },
	{ "O_ANDFLAG", "df", false },
	{ "O_GETMOBDATA", "dff", false },
	{ "O_ORFLAG", "df", false },
	{ "O_SETMOBDATA", "fff", false },
	{ "O_XORFLAG", "df", false },
	{ "O_GETMOBTEXT", "f", false },
	{ "O_MOVEHERO", "ffff", false },
	{ "O_WALKHERO", "f", true },
	{ "O_SETHERO", "ffff", false },
	{ "O_HEROOFF", "f", false },
	{ "O_HEROON", "f", false },
	{ "O_CLSTEXT", "f", false },
	{ "O_CALLTABLE", "dt", false },
	{ "O_CHANGEMOB", "ff", false },
	{ "O_ADDINV", "ff", false },
	{ "O_REMINV", "ff", false },
	{ "O_REPINV", "r", false },
	{ "O_OBSOLETE_GETACTION", "r", false },
	{ "O_ADDWALKAREA", "r", false },
	{ "O_REMWALKAREA", "r", false },
	{ "O_RESTOREWALKAREA", "r", false },
	{ "O_WAITFRAME", "", true },
	{ "O_SETFRAME", "ff", false },
	{ "O_RUNACTION", "r", false },
	{ "O_COMPAREHI", "df", false },
	{ "O_COMPARELO", "df", false },
	{ "O_PRELOADSET", "r", false },
	{ "O_FREEPRELOAD", "r", false },
	{ "O_CHECKINV", "r", false },
	{ "O_TALKHERO", "f", false },
	{ "O_WAITTEXT", "f", false },
	{ "O_SETHEROANIM", "fi", false },
	{ "O_WAITHEROANIM", "f", true },
	{ "O_GETHERODATA", "dff", false },
	{ "O_GETMOUSEBUTTON", "", false },
	{ "O_CHANGEFRAMES", "ffff", false },
	{ "O_CHANGEBACKFRAMES", "ffff", false },
	{ "O_GETBACKANIMDATA", "dff", false },
	{ "O_GETANIMDATA", "dff", false },
	{ "O_SETBGCODE", "o", false },
	{ "O_SETBACKFRAME", "ff", false },
	{ "O_GETRND", "dh", false },
	{ "O_TALKBACKANIM", "ff", false },
	{ "O_LOADPATH", "i", false },
	{ "O_GETCHAR", "d", false },
	{ "O_SETDFLAG", "do", false },
	{ "O_CALLDFLAG", "d", false },
	{ "O_PRINTAT", "fff", false },
	{ "O_ZOOMIN", "f", false },
	{ "O_ZOOMOUT", "f", false },
	{ "O_SETSTRINGOFFSET", "r", false },
	{ "O_GETOBJDATA", "dff", false },
	{ "O_SETOBJDATA", "fff", false },
	{ "O_SWAPOBJECTS", "r", false },
	{ "O_CHANGEHEROSET", "ff", false },
	{ "O_ADDSTRING", "r", false },
	{ "O_SUBSTRING", "f", false },
	{ "O_INITDIALOG", "", false },
	{ "O_ENABLEDIALOGOPT", "f", false },
	{ "O_DISABLEDIALOGOPT", "f", false },
	{ "O_SHOWDIALOGBOX", "f", false },
	{ "O_STOPSAMPLE", "f", false },
	{ "O_BACKANIMRANGE", "fhff", false },
	{ "O_CLEARPATH", "", false },
	{ "O_SETPATH", "", false },
	{ "O_GETHEROX", "fd", false },
	{ "O_GETHEROY", "fd", false },
	{ "O_GETHEROD", "fd", false },
	{ "O_PUSHSTRING", "", false },
	{ "O_POPSTRING", "", false },
	{ "O_SETFGCODE", "o", false },
	{ "O_STOPHERO", "f", false },
	{ "O_ANIMUPDATEOFF", "f", false },
	{ "O_ANIMUPDATEON", "f", false },
	{ "O_FREECURSOR", "", false },
	{ "O_ADDINVQUIET", "ff", false },
	{ "O_RUNHERO", "ffff", false },
	{ "O_SETBACKANIMDATA", "hhd", false },
	{ "O_VIEWFLC", "f", false },
	{ "O_CHECKFLCFRAME", "f", false },
	{ "O_CHECKFLCEND", "", false },
	{ "O_FREEFLC", "", false },
	{ "O_TALKHEROSTOP", "f", false },
	{ "O_HEROCOLOR", "ff", false },
	{ "O_GRABMAPA", "", false },
	{ "O_ENABLENAK", "f", false },
	{ "O_DISABLENAK", "f", false },
	{ "O_GETMOBNAME", "f", false },
	{ "O_SWAPINVENTORY", "f", false },
	{ "O_CLEARINVENTORY", "f", false },
	{ "O_SKIPTEXT", "", false },
	{ "O_SETVOICEH", "f", false },
	{ "O_SETVOICEA", "f", false },
	{ "O_SETVOICEB", "f", false },
	{ "O_SETVOICEC", "f", false },
	{ "O_VIEWFLCLOOP", "f", false },
	{ "O_FLCSPEED", "f", false },
	{ "O_OPENINVENTORY", "", true },
	{ "O_KRZYWA", "", false },
	{ "O_GETKRZYWA", "", false },
	{ "O_GETMOB", "dff", false },
	{ "O_INPUTLINE", "r", false },
	{ "O_SETVOICED", "f", false },
	{ "O_BREAK_POINT", "r", false }
};

struct ScriptInfo {
	int rooms;
	int startGame;
	int restoreGame;
	int stdExamine;
	int stdPickup;
	int stdUse;
	int stdOpen;
	int stdClose;
	int stdTalk;
	int stdGive;
	int usdCode;
	int invObjExam;
	int invObjUse;
	int invObjUU;
	int stdUseItem;
	int lightSources;
	int specRout;
	int invObjGive;
	int stdGiveItem;
	int goTester;
};

struct Room {
	int mobs; // mob flag offset
	int backAnim; // offset to array of animation numbers
	int obj; // offset to array of object numbers
	int nak; // offset to array of masks
	int itemUse;
	int itemGive;
	int walkTo; // offset to array of WALKTO events or 0
	int examine; // offset to array of EXAMINE events or 0
	int pickup;
	int use;
	int pushOpen;
	int pullClose;
	int talk;
	int give;
};

void printUsage(const char *appName) {
	printf("Usage: %s skrypt.dat\n", appName);
}

byte *data;
uint32 dataLen;
bool *dataMark;

#define ADVANCE2() dataMark[pos] = true; pos++; dataMark[pos] = true; pos++
#define ADVANCE4() ADVANCE2(); ADVANCE2()

void decompile(const char *sname, int pos) {
	printf("Script %s\n", sname);

	bool nf = false;
	int tableOffset = -1;

	while (!nf) {
		uint16 op = READ_LE_UINT16(&data[pos]); ADVANCE2();

		nf = opcodes[op].nf;

		const char *param = opcodes[op].params;

		printf("  %s", opcodes[op].name);

		if (*param)
			printf(" ");

		int v;

		while (*param) {
			switch (*param) {
			case 'f':
				v = READ_LE_UINT16(&data[pos]); ADVANCE2();

				if (v & 0x8000) {
					printf("%s", Flags::getFlagName(v));
				} else {
					printf("%d", v);
				}
				break;
			case 'h':
				v = READ_LE_UINT16(&data[pos]); ADVANCE2();
				printf("%d", v);
				break;
			case 'i':
				v = READ_LE_UINT32(&data[pos]); ADVANCE4();
				printf("%d", v);
				break;
			case 'd':
				v = READ_LE_UINT16(&data[pos]); ADVANCE2();
				printf("%s", Flags::getFlagName(v));
				break;
			case 'o':
				v = READ_LE_UINT32(&data[pos]); ADVANCE4();
				printf("[%d]", v);
				break;
			case 't':
				v = READ_LE_UINT32(&data[pos]); ADVANCE4();
				if (tableOffset != -1 && tableOffset != v) {
					error("Duplicate tableOffset: %d vs %d", tableOffset, v);
				}
				tableOffset = v;
				printf("<tableOffset>");
				break;
			case 'r':
				error("Unsupported op %s", opcodes[op].name);
				return;
			default:
				error("Unhandled param '%c' for %s", *param, opcodes[op].name);
				return;
			}

			param++;

			if (*param)
				printf(", ");
		}

		printf("\n");
	}

	if (tableOffset != -1) {
		printf("tableOffset: %d\n[", tableOffset);

		pos = tableOffset;

		for (int i = 0; i < kMaxRooms; i++) {
			if (i && !(i % 10))
				printf("\n ");

			printf("%d", (uint32)READ_LE_UINT32(&data[pos])); ADVANCE4();
			if (i != kMaxRooms - 1)
				printf(", ");
		}
		printf("]\n");
	}

	printf("End Script\n\n");
}

int main(int argc, char *argv[]) {
	if (argc != 2) {
		printUsage(argv[0]);
		return 1;
	}

	Common::File scriptFile(argv[1], "rb");
	if (!scriptFile.isOpen()) {
		error("couldn't load file '%s'", argv[1]);
		return 1;
	}

	uint32 size = scriptFile.size();
	uint8 *fdata = new uint8[size];
	assert(fdata);
	if (size != scriptFile.read_noThrow(fdata, size)) {
		delete [] fdata;
		error("couldn't read all bytes from file '%s'", argv[1]);
		return 1;
	}

	scriptFile.close();

	Decompressor dec;
	dataLen = READ_BE_UINT32(fdata + 14);
	data = (byte *)malloc(dataLen);
	dec.decompress(fdata + 18, data, dataLen);
	delete [] fdata;

	dataMark = (bool *)calloc(dataLen, sizeof(bool));

	int pos = 0;

	ScriptInfo scriptInfo;

	scriptInfo.rooms = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.startGame = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.restoreGame = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdExamine = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdPickup = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdUse = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdOpen = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdClose = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdTalk = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdGive = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.usdCode = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.invObjExam = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.invObjUse = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.invObjUU = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdUseItem = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.lightSources = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.specRout = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.invObjGive = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdGiveItem = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.goTester = READ_LE_UINT32(&data[pos]); ADVANCE4();

	printf("Rooms: %d\n", scriptInfo.rooms);
	printf("StartGame: %d\n", scriptInfo.startGame);
	printf("restoreGame: %d\n", scriptInfo.restoreGame);
	printf("stdExamine: %d\n", scriptInfo.stdExamine);
	printf("stdPickup: %d\n", scriptInfo.stdPickup);
	printf("stdUse: %d\n", scriptInfo.stdUse);
	printf("stdOpen: %d\n", scriptInfo.stdOpen);
	printf("stdClose: %d\n", scriptInfo.stdClose);
	printf("stdTalk: %d\n", scriptInfo.stdTalk);
	printf("stdGive: %d\n", scriptInfo.stdGive);
	printf("usdCode: %d\n", scriptInfo.usdCode);
	printf("invObjExam: %d\n", scriptInfo.invObjExam);
	printf("invObjUse: %d\n", scriptInfo.invObjUse);
	printf("invObjUU: %d\n", scriptInfo.invObjUU);
	printf("stdUseItem: %d\n", scriptInfo.stdUseItem);
	printf("lightSources: %d\n", scriptInfo.lightSources);
	printf("specRout: %d\n", scriptInfo.specRout);
	printf("invObjGive: %d\n", scriptInfo.invObjGive);
	printf("stdGiveItem: %d\n", scriptInfo.stdGiveItem);
	printf("goTester: %d\n", scriptInfo.goTester);

	Room rooms[kMaxRooms];

	for (int i = 0; i < kMaxRooms; i++) {
		pos = scriptInfo.rooms + i * 64;

		rooms[i].mobs = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].backAnim = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].obj = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].nak = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].itemUse = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].itemGive = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].walkTo = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].examine = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].pickup = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].use = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].pushOpen = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].pullClose = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].talk = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].give = READ_LE_UINT32(&data[pos]); ADVANCE4();

		printf("r%02d mobs: %d\n", i, rooms[i].mobs);
		printf("r%02d backAnim: %d\n", i, rooms[i].backAnim);
		printf("r%02d obj: %d\n", i, rooms[i].obj);
		printf("r%02d nak: %d\n", i, rooms[i].nak);
		printf("r%02d itemUse: %d\n", i, rooms[i].itemUse);
		printf("r%02d itemGive: %d\n", i, rooms[i].itemGive);
		printf("r%02d walkTo: %d\n", i, rooms[i].walkTo);
		printf("r%02d examine: %d\n", i, rooms[i].examine);
		printf("r%02d pickup: %d\n", i, rooms[i].pickup);
		printf("r%02d use: %d\n", i, rooms[i].use);
		printf("r%02d pushOpen: %d\n", i, rooms[i].pushOpen);
		printf("r%02d pullClose: %d\n", i, rooms[i].pullClose);
		printf("r%02d talk: %d\n", i, rooms[i].talk);
		printf("r%02d give: %d\n", i, rooms[i].give);
	}

	decompile("StartGame", scriptInfo.startGame);
	decompile("RestoreGame", scriptInfo.restoreGame);

	for (uint i = 0; i < dataLen & 0; i++)
		printf("%c", dataMark[i] ? '*' : '.');

	printf("\n");

	return 0;
}
