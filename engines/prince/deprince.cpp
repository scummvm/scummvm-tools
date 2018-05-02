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

#include "common/array.h"
#include "common/file.h"
#include "common/endian.h"
#include "common/str.h"
#include "common/util.h"

#include "utils.h"
#include "flags.h"

#include <assert.h>

static const int16 kMaxRooms = 60;
static const int kMaxBackAnims = 64;
static const int kMaxMobs = 64;
static const int kMaxObjects = 64;

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
	{ "O_FREESAMPLE", "f", false }, 	// 5
	{ "O_PLAYSAMPLE", "fh", false },
	{ "O_PUTOBJECT", "fff", false },
	{ "O_REMOBJECT", "ff", false },
	{ "O_SHOWANIM", "ff", false },
	{ "O_CHECKANIMEND", "f", false },	// 10
	{ "O_FREEANIM", "f", false },
	{ "O_CHECKANIMFRAME", "ff", false },
	{ "O_PUTBACKANIM", "ffi", false },
	{ "O_REMBACKANIM", "ff", false },
	{ "O_CHECKBACKANIMFRAME", "ff", false }, // 15
	{ "O_FREEALLSAMPLES", "r", false },
	{ "O_SETMUSIC", "h", false },
	{ "O_STOPMUSIC", "", false },
	{ "O__WAIT", "f", false },
	{ "O_UPDATEOFF", "r", false },		// 20
	{ "O_UPDATEON", "r", false },
	{ "O_UPDATE", "r", false },
	{ "O_CLS", "r", false },
	{ "O__CALL", "o", false },
	{ "O_RETURN", "", true },			// 25
	{ "O_GO", "o", false },
	{ "O_BACKANIMUPDATEOFF", "f", false },
	{ "O_BACKANIMUPDATEON", "f", false },
	{ "O_CHANGECURSOR", "f", false },
	{ "O_CHANGEANIMTYPE", "r", false },	// 30
	{ "O__SETFLAG", "df", false },
	{ "O_COMPARE", "df", false },
	{ "O_JUMPZ", "o", false },
	{ "O_JUMPNZ", "o", false },
	{ "O_EXIT", "", true },				// 35
	{ "O_ADDFLAG", "df", false },
	{ "O_TALKANIM", "ff", false },
	{ "O_SUBFLAG", "df", false },
	{ "O_SETSTRING", "i", false },
	{ "O_ANDFLAG", "df", false },		// 40
	{ "O_GETMOBDATA", "dff", false },
	{ "O_ORFLAG", "df", false },
	{ "O_SETMOBDATA", "fff", false },
	{ "O_XORFLAG", "df", false },
	{ "O_GETMOBTEXT", "f", false },		// 45
	{ "O_MOVEHERO", "ffff", false },
	{ "O_WALKHERO", "f", true },
	{ "O_SETHERO", "ffff", false },
	{ "O_HEROOFF", "f", false },
	{ "O_HEROON", "f", false },			// 50
	{ "O_CLSTEXT", "f", false },
	{ "O_CALLTABLE", "dt", false },
	{ "O_CHANGEMOB", "ff", false },
	{ "O_ADDINV", "ff", false },
	{ "O_REMINV", "ff", false },		// 55
	{ "O_REPINV", "r", false },
	{ "O_OBSOLETE_GETACTION", "r", false },
	{ "O_ADDWALKAREA", "r", false },
	{ "O_REMWALKAREA", "r", false },
	{ "O_RESTOREWALKAREA", "r", false },// 60
	{ "O_WAITFRAME", "", true },
	{ "O_SETFRAME", "ff", false },
	{ "O_RUNACTION", "r", false },
	{ "O_COMPAREHI", "df", false },
	{ "O_COMPARELO", "df", false },		// 65
	{ "O_PRELOADSET", "r", false },
	{ "O_FREEPRELOAD", "r", false },
	{ "O_CHECKINV", "r", false },
	{ "O_TALKHERO", "f", false },
	{ "O_WAITTEXT", "f", false },		// 70
	{ "O_SETHEROANIM", "fi", false },
	{ "O_WAITHEROANIM", "f", true },
	{ "O_GETHERODATA", "dff", false },
	{ "O_GETMOUSEBUTTON", "", false },
	{ "O_CHANGEFRAMES", "ffff", false },// 75
	{ "O_CHANGEBACKFRAMES", "ffff", false },
	{ "O_GETBACKANIMDATA", "dff", false },
	{ "O_GETANIMDATA", "dff", false },
	{ "O_SETBGCODE", "o", false },
	{ "O_SETBACKFRAME", "ff", false },	// 80
	{ "O_GETRND", "dh", false },
	{ "O_TALKBACKANIM", "ff", false },
	{ "O_LOADPATH", "i", false },
	{ "O_GETCHAR", "d", false },
	{ "O_SETDFLAG", "do", false },		// 85
	{ "O_CALLDFLAG", "d", false },
	{ "O_PRINTAT", "fff", false },
	{ "O_ZOOMIN", "f", false },
	{ "O_ZOOMOUT", "f", false },
	{ "O_SETSTRINGOFFSET", "r", false },// 90
	{ "O_GETOBJDATA", "dff", false },
	{ "O_SETOBJDATA", "fff", false },
	{ "O_SWAPOBJECTS", "r", false },
	{ "O_CHANGEHEROSET", "ff", false },
	{ "O_ADDSTRING", "r", false },		// 95
	{ "O_SUBSTRING", "f", false },
	{ "O_INITDIALOG", "", false },
	{ "O_ENABLEDIALOGOPT", "f", false },
	{ "O_DISABLEDIALOGOPT", "f", false },
	{ "O_SHOWDIALOGBOX", "f", false },	// 100
	{ "O_STOPSAMPLE", "f", false },
	{ "O_BACKANIMRANGE", "fhff", false },
	{ "O_CLEARPATH", "", false },
	{ "O_SETPATH", "", false },
	{ "O_GETHEROX", "fd", false },		// 105
	{ "O_GETHEROY", "fd", false },
	{ "O_GETHEROD", "fd", false },
	{ "O_PUSHSTRING", "", false },
	{ "O_POPSTRING", "", false },
	{ "O_SETFGCODE", "o", false },		// 110
	{ "O_STOPHERO", "f", false },
	{ "O_ANIMUPDATEOFF", "f", false },
	{ "O_ANIMUPDATEON", "f", false },
	{ "O_FREECURSOR", "", false },
	{ "O_ADDINVQUIET", "ff", false },	// 115
	{ "O_RUNHERO", "ffff", false },
	{ "O_SETBACKANIMDATA", "hhd", false },
	{ "O_VIEWFLC", "f", false },
	{ "O_CHECKFLCFRAME", "f", false },
	{ "O_CHECKFLCEND", "", false },		// 120
	{ "O_FREEFLC", "", false },
	{ "O_TALKHEROSTOP", "f", false },
	{ "O_HEROCOLOR", "ff", false },
	{ "O_GRABMAPA", "", false },
	{ "O_ENABLENAK", "f", false },		// 125
	{ "O_DISABLENAK", "f", false },
	{ "O_GETMOBNAME", "f", false },
	{ "O_SWAPINVENTORY", "f", false },
	{ "O_CLEARINVENTORY", "f", false },
	{ "O_SKIPTEXT", "", false },		// 130
	{ "O_SETVOICEH", "f", false },
	{ "O_SETVOICEA", "f", false },
	{ "O_SETVOICEB", "f", false },
	{ "O_SETVOICEC", "f", false },
	{ "O_VIEWFLCLOOP", "f", false },	// 135
	{ "O_FLCSPEED", "f", false },
	{ "O_OPENINVENTORY", "", true },
	{ "O_KRZYWA", "", false },
	{ "O_GETKRZYWA", "", false },
	{ "O_GETMOB", "dff", false },		// 140
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
	int unk1;
	int unk2;
};

struct Mask {
	uint16 _state; // visible / invisible
	int16 _flags; // turning on / turning off of an mask
	int16 _x1;
	int16 _y1;
	int16 _x2;
	int16 _y2;
	int16 _z;
	int16 _number; // number of mask for background recreating
	int16 _width;
	int16 _height;
};

void printUsage(const char *appName) {
	printf("Usage: %s skrypt.dat\n", appName);
}

byte *data;
uint32 dataLen;
bool *dataMark;
bool *dataDecompile;
int numscripts = 0;

Common::String *labels;

#define ADVANCE() dataMark[pos] = true; pos++
#define ADVANCE2() ADVANCE(); ADVANCE()
#define ADVANCE4() ADVANCE2(); ADVANCE2()

#define ADVANCES() dataMark[pos] = dataDecompile[pos] = true; pos++
#define ADVANCES2() ADVANCES(); ADVANCES()
#define ADVANCES4() ADVANCES2(); ADVANCES2()

void printArray(int offset, int type, int size, bool split = true, bool offsets = false) {
	printf("[");

	int pos = offset;

	for (int i = 0; i < size; i++) {
		if (split && i && !(i % 10))
			printf("\n ");

		if (type == 1) {
			printf("%d", data[pos]); ADVANCE();
		} else if (type == 2) {
			printf("%d", (uint16)READ_LE_UINT16(&data[pos])); ADVANCE2();
		} else if (type == 4) {
			uint32 v = (uint32)READ_LE_UINT32(&data[pos]); ADVANCE4();
			if (offsets && v && !labels[v].empty())
				printf("%s[%d]", labels[v].c_str(), v);
			else
				printf("%d", v);
		} else {
			error("printArray: unknown type %d", type);
		}

		if (i != size - 1)
			printf(", ");
	}

	printf("]\n");
}

void decompile(const char *sname, int pos, bool printOut = false) {
	if (pos == 0)
		return;

	if (labels[pos].empty() || labels[pos].hasPrefix("script")) {
		labels[pos] = sname;
	}

	if (!printOut)
		numscripts++;

	if (printOut)
		printf("%s:\n", sname);

	bool nf = false;
	int tableOffset = -1;

	char buf[100];

	while (!nf) {
		if (!printOut && dataDecompile[pos])
			break;

		uint16 op = READ_LE_UINT16(&data[pos]); ADVANCES2();

		if (op >= ARRAYSIZE(opcodes))
			error("Invalid op: %d at %d", op, pos - 2);

		nf = opcodes[op].nf;

		const char *param = opcodes[op].params;

		if (printOut)
			printf("  %s", opcodes[op].name);

		if (*param && printOut)
			printf(" ");

		int v;

		while (*param) {
			switch (*param) {
			case 'f':
				v = READ_LE_UINT16(&data[pos]); ADVANCES2();

				if (!printOut)
					break;

				if (v & 0x8000) {
					printf("%s", Flags::getFlagName(v));
				} else {
					printf("%d", v);
				}
				break;
			case 'h':
				v = READ_LE_UINT16(&data[pos]); ADVANCES2();

				if (printOut)
					printf("%d", v);
				break;
			case 'i':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();

				if (printOut)
					printf("%d", v);
				break;
			case 'd':
				v = READ_LE_UINT16(&data[pos]); ADVANCES2();

				if (printOut)
					printf("%s", Flags::getFlagName(v));
				break;
			case 'o':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();

				if (printOut) {
					printf("%s[%d]", labels[pos + v - 4].c_str(), v);
				} else {
					sprintf(buf, "script%06d", pos + v - 4);
					decompile(buf, pos + v - 4);
				}

				break;
			case 's':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();

				if (printOut)
					printf("\"%s\"", &data[pos + v - 4]);

				v = pos + v - 4;
				while (data[v]) {
					dataMark[v] = dataDecompile[v] = true;
					v++;
				}
				dataMark[v] = dataDecompile[v] = true;

				break;
			case 't':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();
				if (tableOffset != -1 && tableOffset != v) {
					error("Duplicate tableOffset: %d vs %d", tableOffset, v);
				}
				tableOffset = v;

				if (printOut)
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

			if (*param && printOut)
				printf(", ");
		}

		if (printOut)
			printf("\n");
	}

	if (tableOffset != -1 && printOut) {
		printf("tableOffset: %d\n", tableOffset);

		printArray(tableOffset, 4, kMaxRooms, true, true);
	}

	if (printOut)
		printf("\n");

	if (tableOffset != -1) {
		pos = tableOffset;

		for (int i = 0; i < kMaxRooms; i++) {
			sprintf(buf, "tableOffset%02d", i);

			uint off = READ_LE_UINT32(&data[pos]); ADVANCES4();

			decompile(buf, off);
		}
	}
}

void loadMask(int offset) {
	Mask tempMask;

	int pos = offset;
	int n = 0;

	while (1) {
		tempMask._state = READ_LE_UINT16(&data[pos]); ADVANCE2();
		if (tempMask._state == 0xffff) {
			break;
		}
		tempMask._flags = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._x1 = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._y1 = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._x2 = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._y2 = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._z = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._number = READ_LE_UINT16(&data[pos]); ADVANCE2();

		printf("  mask%d state=%d fl=%d x1=%d y1=%d x2=%d y2=%d z=%d number=%d\n", n, tempMask._state,
					tempMask._flags, tempMask._x1, tempMask._y1, tempMask._x2, tempMask._y2,
					tempMask._z, tempMask._number);

		n++;
	}
}

void loadMobEvents(int offset, const char *name) {
	if (!offset)
		return;

	int pos = offset;
	int i = 0;
	int16 mob;
	int32 code;

	char buf[100];

	while(1) {
		mob = (int16)READ_LE_UINT16(&data[pos]); ADVANCE2();

		if (mob == -1)
			break;

		code = READ_LE_UINT32(&data[pos]); ADVANCE4();

		printf("  mob%02d: mob=%d code=%d\n", i, mob, code);

		sprintf(buf, "%s.mob%d", name, mob);
		decompile(buf, code);

		i++;
	}
}

void loadMobEventsWithItem(int offset, const char *name) {
	if (!offset)
		return;

	int pos = offset;
	int i = 0;
	int16 mob;
	int16 item;
	int32 code;

	char buf[100];

	while(1) {
		mob = (int16)READ_LE_UINT16(&data[pos]); ADVANCE2();

		if (mob == -1)
			break;

		item = READ_LE_UINT16(&data[pos]); ADVANCE2();
		code = READ_LE_UINT32(&data[pos]); ADVANCE4();

		printf("  mobitem%02d: mob=%d item=%d code=%d\n", i, mob, item, code);

		sprintf(buf, "%s.mobitem%d", name, mob);
		decompile(buf, code);

		i++;
	}
}

void loadLightSources(int offset) {
	int pos = offset;

	for (int i = 0; i < kMaxRooms; i++) {
		int x = READ_LE_UINT16(&data[pos]); ADVANCE2();
		int y = READ_LE_UINT16(&data[pos]); ADVANCE2();
		int scale = READ_LE_UINT16(&data[pos]); ADVANCE2();
		int unk = READ_LE_UINT16(&data[pos]); ADVANCE2();

		printf("  light%02d: x=%d y=%d scale=%d unk=%d\n", i, x, y, scale, unk);
	}
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

#if 0
	Common::File dumpFile("skrypt.dump", "wb");
	if (!dumpFile.isOpen()) {
		error("couldn't load file '%s'", argv[1]);
		return 1;
	}
	dumpFile.write(data, dataLen);
	dumpFile.close();
#endif

	dataMark = (bool *)calloc(dataLen, sizeof(bool));
	dataDecompile = (bool *)calloc(dataLen, sizeof(bool));
	labels = new Common::String[dataLen];

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
	printf("invObjExam: [%d]\n", scriptInfo.invObjExam);
	loadMobEvents(scriptInfo.invObjExam, "invObjExam");
	printf("end invObjExam\n");
	printf("invObjUse: [%d]\n", scriptInfo.invObjUse);
	loadMobEvents(scriptInfo.invObjUse, "invObjUse");
	printf("end invObjUse\n");
	printf("invObjUU: %d\n", scriptInfo.invObjUU);
	loadMobEventsWithItem(scriptInfo.invObjUU, "invObjUU");
	printf("end invObjUU\n");
	printf("stdUseItem: %d\n", scriptInfo.stdUseItem);
	printf("lightSources: [%d]\n", scriptInfo.lightSources);
	loadLightSources(scriptInfo.lightSources);
	printf("end lightSources\n");
	printf("specRout: %d\n", scriptInfo.specRout);
	printf("invObjGive: %d\n", scriptInfo.invObjGive);
	printf("stdGiveItem: %d\n", scriptInfo.stdGiveItem);
	printf("goTester: %d\n", scriptInfo.goTester);

	Room rooms[kMaxRooms];

	for (int i = 0; i < kMaxRooms; i++) {
		pos = scriptInfo.rooms + i * 64;

		rooms[i].mobs = READ_LE_UINT32(&data[pos]); ADVANCE4();			// byte[kMaxMobs]
		rooms[i].backAnim = READ_LE_UINT32(&data[pos]); ADVANCE4();		// int32[kMaxBackAnims]
		rooms[i].obj = READ_LE_UINT32(&data[pos]); ADVANCE4();			// byte [kMaxObjects]
		rooms[i].nak = READ_LE_UINT32(&data[pos]); ADVANCE4();			// offset pointing to Mask structure
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
		rooms[i].unk1 = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].unk2 = READ_LE_UINT32(&data[pos]); ADVANCE4();

		char buf[100];

		printf("r%02d mobs: [%d]: ", i, rooms[i].mobs);
		printArray(rooms[i].mobs, 1, kMaxMobs, false);
		printf("r%02d backAnim: [%d]: ", i, rooms[i].backAnim);
		printArray(rooms[i].backAnim, 4, kMaxBackAnims, false);
		printf("r%02d obj: [%d]: ", i, rooms[i].obj);
		printArray(rooms[i].obj, 1, kMaxObjects, false);
		printf("r%02d masks [%d]\n", i, rooms[i].nak);
		loadMask(rooms[i].nak);
		printf("end masks\n");
		printf("r%02d itemUse: [%d]\n", i, rooms[i].itemUse);
		sprintf(buf, "rooms%02d.itemUse", i);
		loadMobEventsWithItem(rooms[i].itemUse, buf);
		printf("end itemUse\n");
		printf("r%02d itemGive: [%d]\n", i, rooms[i].itemGive);
		sprintf(buf, "rooms%02d.itemGive", i);
		loadMobEventsWithItem(rooms[i].itemGive, buf);
		printf("end itemGive\n");
		printf("r%02d walkTo: [%d]\n", i, rooms[i].walkTo);
		sprintf(buf, "rooms%02d.walkTo", i);
		loadMobEvents(rooms[i].walkTo, buf);
		printf("end walkTo\n");
		printf("r%02d examine: [%d]\n", i, rooms[i].examine);
		sprintf(buf, "rooms%02d.examine", i);
		loadMobEvents(rooms[i].examine, buf);
		printf("end examine\n");
		printf("r%02d pickup: [%d]\n", i, rooms[i].pickup);
		sprintf(buf, "rooms%02d.pickup", i);
		loadMobEvents(rooms[i].pickup, buf);
		printf("end pickup\n");
		printf("r%02d use: [%d]\n", i, rooms[i].use);
		sprintf(buf, "rooms%02d.use", i);
		loadMobEvents(rooms[i].use, buf);
		printf("end use\n");
		printf("r%02d pushOpen: [%d]\n", i, rooms[i].pushOpen);
		sprintf(buf, "rooms%02d.pushOpen", i);
		loadMobEvents(rooms[i].pushOpen, buf);
		printf("end pushOpen\n");
		printf("r%02d pullClose: [%d]\n", i, rooms[i].pullClose);
		sprintf(buf, "rooms%02d.pullClose", i);
		loadMobEvents(rooms[i].pullClose, buf);
		printf("end pullClose\n");
		printf("r%02d talk: [%d]\n", i, rooms[i].talk);
		sprintf(buf, "rooms%02d.talk", i);
		loadMobEvents(rooms[i].talk, buf);
		printf("end talk\n");
		printf("r%02d give: [%d]\n", i, rooms[i].give);
		sprintf(buf, "rooms%02d.give", i);
		loadMobEvents(rooms[i].give, buf);
		printf("end give\n");
		printf("r%02d unk1: %d\n", i, rooms[i].unk1);
		printf("r%02d unk2: %d\n", i, rooms[i].unk2);
	}

	decompile("StartGame", scriptInfo.startGame);
	decompile("RestoreGame", scriptInfo.restoreGame);
	decompile("stdExamine", scriptInfo.stdExamine);
	decompile("stdPickup", scriptInfo.stdPickup);
	decompile("stdUse", scriptInfo.stdUse);
	decompile("stdOpen", scriptInfo.stdOpen);
	decompile("stdClose", scriptInfo.stdClose);
	decompile("stdTalk", scriptInfo.stdTalk);
	decompile("stdGive", scriptInfo.stdGive);
	decompile("usdCode", scriptInfo.usdCode);
	decompile("stdUseItem", scriptInfo.stdUseItem);
	decompile("stdGiveItem", scriptInfo.stdGiveItem);

#if 1
	int n = 0;
	const char *shades[] = {" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"};
	for (uint i = 0; i < dataLen; i++) {
		if (i % 8 == 0 && i) {
			printf("%s", shades[n]);
			n = 0;
		}

		if (dataMark[i])
			n++;
	}

	printf("\n");
#endif

	printf("Total scripts: %d\n", numscripts);

	bool inDB = false;

	for (int i = 0; i < dataLen; i++)
		if (!labels[i].empty()) {
			if (inDB) {
				printf("\n\n");
				inDB = false;
			}
			decompile(labels[i].c_str(), i, true);
		} else if (!dataMark[i]) {
			if (!inDB) {
				printf("db %d", data[i]);
				inDB = true;
			} else {
				printf(", %d", data[i]);
			}
		}

	return 0;
}
