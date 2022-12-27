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

/* GobEngine Script disassembler */

#ifndef DEGOB_SCRIPT_H
#define DEGOB_SCRIPT_H

#include <string>
#include <list>
#include <map>

#include "common/scummsys.h"

#define _OPCODET(ver, x) TYPE_TEXTDESC, 0, #x
#define _OPCODEF(ver, x) TYPE_FUNCDESC, &ver::x, #x
#define _OPCODEB(ver, x) TYPE_BOTHDESC, &ver::x, #x

class ExtTable {
public:
	ExtTable(byte *data, uint32 size, byte *dataCom = 0, uint32 sizeCom = 0);
	~ExtTable();

	byte *getItem(uint16 i, uint32 &size) const;

private:
	struct Item {
		int32 offset;
		uint32 size;
		uint32 width;
		uint32 height;
		bool isPacked;
	};

	byte *_data;
	uint32 _size;
	byte *_dataCom;
	uint32 _sizeCom;

	uint16 _itemsCount;
	Item *_items;

	void init();
	byte *unpack(const byte *packedData, uint32 &size) const;
};

class Script {
public:
	Script(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script();

	uint32 getPos() const;
	void skip(uint32 off);
	void seek(uint32 off, int whence = SEEK_SET);

	// Properties getter
	uint16 getStart() const;
	uint16 getTextCenter() const;
	uint16 getVarsCount() const;
	uint32 getTotTextCount() const;
	uint32 getTotResOffset() const;
	uint16 getTotResCount() const;
	uint16 getAnimDataSize() const;
	uint8 getVerScript() const;
	uint8 getVerIMEX() const;
	uint8 getSuffixIM() const;
	uint8 getSuffixEX() const;
	uint32 getFuncNamesCount() const;

	void loadIDE(const byte *ideData);

	void deGob(int32 offset = -1, bool isLib = false);

protected:
	enum FuncType {
		TYPE_NONE = 0,   // No description
		TYPE_TEXTDESC,   // Description by a string
		TYPE_FUNCDESC,   // Description by a function
		TYPE_BOTHDESC    // Description by both
	};
	enum Param {
		PARAM_NONE = 0,  // No parameters / Last parameter
		PARAM_UINT8,     // Immediate uint8
		PARAM_UINT16,    // Immediate uint16
		PARAM_UINT32,    // Immediate uint32
		PARAM_INT8,      // Immediate int8
		PARAM_INT16,     // Immediate int16
		PARAM_INT32,     // Immediate int32
		PARAM_STR,       // Immediate string
		PARAM_EXPR,      // Expression
		PARAM_VARINDEX,  // Variable index
		PARAM_GOB        // Special GobFunc params
	};
	struct FuncParams {
		const char *desc;
		byte cmdCount;
		byte counter;
		int16 retFlag;
		int16 extraData;
		int16 objIndex;
	};

	uint32 _indent;

	virtual void setupOpcodes() = 0;
	virtual void drawOpcode(byte i, FuncParams &params) = 0;
	virtual void funcOpcode(byte i, byte j, FuncParams &params) = 0;
	virtual void goblinOpcode(int i, FuncParams &params) = 0;

	// Helper function for printing
	void putString(const char *s) const;
	void print(const char *s, ...) const;
	void printIndent() const;
	void printLine(const char *s) const;
	std::string printStr(const char *s, ...) const;

	void incIndent();
	void decIndent();

	uint8 readUint8();
	uint16 readUint16();
	uint32 readUint32();
	const char *readString();

	uint8 peekUint8(int32 offset = 0) const;
	uint16 peekUint16(int32 offset = 0) const;
	uint32 peekUint32(int32 offset = 0) const;
	const char *peekString(int32 offset = 0) const;

	void skipExpr(char stopToken = 99);
	std::string readExpr(char stopToken = 99);
	std::string readVarIndex(uint16 *arg_0 = 0, uint16 *arg_4 = 0);

	uint16 getBlockSize() const;

	void evaluateParams(const Param *params);
	void printFuncDesc(const FuncParams &fParams, const Param *params);
	void printFuncDesc(const FuncParams &fParams) const;

	void startFunc(const FuncParams &fParams) const;
	void endFunc() const;

	void loadProperties(byte *data);

	void funcBlock(int16 retFlag);

	void addStartingOffsets();

	void addFuncOffset(uint32 offset);
	void deGobFunction();

private:
	byte *_totData, *_ptr;
	uint32 _totSize;

protected:
	ExtTable *_extTable;

	std::list<uint32> _funcOffsets;
	std::map<uint32, std::string> _funcOffsetsNames;

	// Script properties
	uint16 _start, _textCenter;
	uint16 _varsCount;
	uint32 _totTextCount;
	uint32 _totResOffset;
	uint16 _totResCount;
	uint16 _animDataSize;
	uint8 _verScript, _verIMEX;
	// If > 0, script loads stuff out of commun.exN and commun.imN, where N is this suffix
	uint8 _suffixIM, _suffixEX;
};

class Script_v1 : public Script {
public:
	Script_v1(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_v1();

protected:
	typedef void (Script_v1::*OpcodeDrawProcV1)(FuncParams &);
	typedef void (Script_v1::*OpcodeFuncProcV1)(FuncParams &);
	typedef void (Script_v1::*OpcodeGoblinProcV1)(FuncParams &);
	struct OpcodeDrawEntryV1 {
		FuncType type;
		OpcodeDrawProcV1 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryV1 {
		FuncType type;
		OpcodeFuncProcV1 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryV1 {
		FuncType type;
		OpcodeGoblinProcV1 proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryV1 *_opcodesDrawV1;
	const OpcodeFuncEntryV1 *_opcodesFuncV1;
	const OpcodeGoblinEntryV1 *_opcodesGoblinV1;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);

	// Extended opcode functions
	void o1_drawOperations(FuncParams &params);
	void o1_goblinFunc(FuncParams &params);

	// Control functions
	void o1_callSub(FuncParams &params);
	void o1_switch(FuncParams &params);
	void o1_repeatUntil(FuncParams &params);
	void o1_whileDo(FuncParams &params);
	void o1_if(FuncParams &params);
	void o1_return(FuncParams &params);
	void o1_returnTo(FuncParams &params);
	void o1_setcmdCount(FuncParams &params);
	void o1_assign(FuncParams &params);

	void o1_palLoad(FuncParams &params);
	void o1_loadSpriteToPos(FuncParams &params);
	void o1_printText(FuncParams &params);
	void o1_loadTot(FuncParams &params);
	void o1_loadSound(FuncParams &params);

	void o1_loadMult(FuncParams &params);
	void o1_loadAnim(FuncParams &params);
	void o1_loadStatic(FuncParams &params);
	void o1_loadMultObject(FuncParams &params);

	void o1_dummy(FuncParams &params);
	void o1_copySprite(FuncParams &params);
};

class Script_v2 : public Script_v1 {
public:
	Script_v2(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_v2();

protected:
	typedef void (Script_v2::*OpcodeDrawProcV2)(FuncParams &);
	typedef void (Script_v2::*OpcodeFuncProcV2)(FuncParams &);
	typedef void (Script_v2::*OpcodeGoblinProcV2)(FuncParams &);
	struct OpcodeDrawEntryV2 {
		FuncType type;
		OpcodeDrawProcV2 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryV2 {
		FuncType type;
		OpcodeFuncProcV2 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryV2 {
		FuncType type;
		OpcodeGoblinProcV2 proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryV2 *_opcodesDrawV2;
	const OpcodeFuncEntryV2 *_opcodesFuncV2;
	const OpcodeGoblinEntryV2 *_opcodesGoblinV2;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);

	void o2_goblinFunc(FuncParams &params);

	void o2_totSub(FuncParams &params);
	void o2_assign(FuncParams &params);
	void o2_pushVars(FuncParams &params);
	void o2_popVars(FuncParams &params);

	void o2_loadSound(FuncParams &params);

	void o2_loadMult(FuncParams &params);
	void o2_loadMultObject(FuncParams &params);
	void o2_loadMapObjects(FuncParams &params);

	void o2_playMult(FuncParams &params);
	void o2_printText(FuncParams &params);

	void o2_loadInfogramesIns(FuncParams &params);
	void o2_playInfogrames(FuncParams &params);
	void o2_handleGoblins(FuncParams &params);
};

class Script_Geisha : public Script_v1 {
public:
	Script_Geisha(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_Geisha();

protected:
	typedef void (Script_Geisha::*OpcodeDrawProcGeisha)(FuncParams &);
	typedef void (Script_Geisha::*OpcodeFuncProcGeisha)(FuncParams &);
	typedef void (Script_Geisha::*OpcodeGoblinProcGeisha)(FuncParams &);
	struct OpcodeDrawEntryGeisha {
		FuncType type;
		OpcodeDrawProcGeisha proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryGeisha {
		FuncType type;
		OpcodeFuncProcGeisha proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryGeisha {
		FuncType type;
		OpcodeGoblinProcGeisha proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryGeisha *_opcodesDrawGeisha;
	const OpcodeFuncEntryGeisha *_opcodesFuncGeisha;
	const OpcodeGoblinEntryGeisha *_opcodesGoblinGeisha;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);

	void oGeisha_goblinFunc(FuncParams &params);
};

class Script_Bargon : public Script_v2 {
public:
	Script_Bargon(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_Bargon();

protected:
	typedef void (Script_Bargon::*OpcodeDrawProcBargon)(FuncParams &);
	typedef void (Script_Bargon::*OpcodeFuncProcBargon)(FuncParams &);
	typedef void (Script_Bargon::*OpcodeGoblinProcBargon)(FuncParams &);
	struct OpcodeDrawEntryBargon {
		FuncType type;
		OpcodeDrawProcBargon proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryBargon {
		FuncType type;
		OpcodeFuncProcBargon proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryBargon {
		FuncType type;
		OpcodeGoblinProcBargon proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryBargon *_opcodesDrawBargon;
	const OpcodeFuncEntryBargon *_opcodesFuncBargon;
	const OpcodeGoblinEntryBargon *_opcodesGoblinBargon;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);
};

class Script_Fascin : public Script_v2 {
public:
	Script_Fascin(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_Fascin();

protected:
	typedef void (Script_Fascin::*OpcodeDrawProcFascin)(FuncParams &);
	typedef void (Script_Fascin::*OpcodeFuncProcFascin)(FuncParams &);
	typedef void (Script_Fascin::*OpcodeGoblinProcFascin)(FuncParams &);
	struct OpcodeDrawEntryFascin {
		FuncType type;
		OpcodeDrawProcFascin proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryFascin {
		FuncType type;
		OpcodeFuncProcFascin proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryFascin {
		FuncType type;
		OpcodeGoblinProcFascin proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryFascin *_opcodesDrawFascin;
	const OpcodeFuncEntryFascin *_opcodesFuncFascin;
	const OpcodeGoblinEntryFascin *_opcodesGoblinFascin;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);
};

class Script_LittleRed : public Script_v2 {
public:
	Script_LittleRed(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_LittleRed();

protected:
	typedef void (Script_LittleRed::*OpcodeDrawProcLittleRed)(FuncParams &);
	typedef void (Script_LittleRed::*OpcodeFuncProcLittleRed)(FuncParams &);
	typedef void (Script_LittleRed::*OpcodeGoblinProcLittleRed)(FuncParams &);
	struct OpcodeDrawEntryLittleRed {
		FuncType type;
		OpcodeDrawProcLittleRed proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryLittleRed {
		FuncType type;
		OpcodeFuncProcLittleRed proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryLittleRed {
		FuncType type;
		OpcodeGoblinProcLittleRed proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryLittleRed *_opcodesDrawLittleRed;
	const OpcodeFuncEntryLittleRed *_opcodesFuncLittleRed;
	const OpcodeGoblinEntryLittleRed *_opcodesGoblinLittleRed;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);
};

class Script_v3 : public Script_v2 {
public:
	Script_v3(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_v3();

protected:
	typedef void (Script_v3::*OpcodeDrawProcV3)(FuncParams &);
	typedef void (Script_v3::*OpcodeFuncProcV3)(FuncParams &);
	typedef void (Script_v3::*OpcodeGoblinProcV3)(FuncParams &);
	struct OpcodeDrawEntryV3 {
		FuncType type;
		OpcodeDrawProcV3 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryV3 {
		FuncType type;
		OpcodeFuncProcV3 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryV3 {
		FuncType type;
		OpcodeGoblinProcV3 proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryV3 *_opcodesDrawV3;
	const OpcodeFuncEntryV3 *_opcodesFuncV3;
	const OpcodeGoblinEntryV3 *_opcodesGoblinV3;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);
};

class Script_v4 : public Script_v3 {
public:
	Script_v4(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_v4();

protected:
	typedef void (Script_v4::*OpcodeDrawProcV4)(FuncParams &);
	typedef void (Script_v4::*OpcodeFuncProcV4)(FuncParams &);
	typedef void (Script_v4::*OpcodeGoblinProcV4)(FuncParams &);
	struct OpcodeDrawEntryV4 {
		FuncType type;
		OpcodeDrawProcV4 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryV4 {
		FuncType type;
		OpcodeFuncProcV4 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryV4 {
		FuncType type;
		OpcodeGoblinProcV4 proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryV4 *_opcodesDrawV4;
	const OpcodeFuncEntryV4 *_opcodesFuncV4;
	const OpcodeGoblinEntryV4 *_opcodesGoblinV4;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);
};

class Script_v5 : public Script_v4 {
public:
	Script_v5(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_v5();

protected:
	typedef void (Script_v5::*OpcodeDrawProcV5)(FuncParams &);
	typedef void (Script_v5::*OpcodeFuncProcV5)(FuncParams &);
	typedef void (Script_v5::*OpcodeGoblinProcV5)(FuncParams &);
	struct OpcodeDrawEntryV5 {
		FuncType type;
		OpcodeDrawProcV5 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryV5 {
		FuncType type;
		OpcodeFuncProcV5 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryV5 {
		FuncType type;
		OpcodeGoblinProcV5 proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryV5 *_opcodesDrawV5;
	const OpcodeFuncEntryV5 *_opcodesFuncV5;
	const OpcodeGoblinEntryV5 *_opcodesGoblinV5;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);

	void o5_spaceShooter(FuncParams &params);

	void o5_istrlen(FuncParams &params);
};

class Script_v6 : public Script_v5 {
public:
	Script_v6(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_v6();

protected:
	typedef void (Script_v6::*OpcodeDrawProcV6)(FuncParams &);
	typedef void (Script_v6::*OpcodeFuncProcV6)(FuncParams &);
	typedef void (Script_v6::*OpcodeGoblinProcV6)(FuncParams &);
	struct OpcodeDrawEntryV6 {
		FuncType type;
		OpcodeDrawProcV6 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryV6 {
		FuncType type;
		OpcodeFuncProcV6 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryV6 {
		FuncType type;
		OpcodeGoblinProcV6 proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryV6 *_opcodesDrawV6;
	const OpcodeFuncEntryV6 *_opcodesFuncV6;
	const OpcodeGoblinEntryV6 *_opcodesGoblinV6;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);

	void o6_loadCursor(FuncParams &params);
	void o6_assign(FuncParams &params);
	void o6_createSprite(FuncParams &params);
};

class Script_v7 : public Script_v6 {
public:
	Script_v7(byte *totData, uint32 totSize, ExtTable *extTable = 0);
	virtual ~Script_v7();

protected:
	typedef void (Script_v7::*OpcodeDrawProcV7)(FuncParams &);
	typedef void (Script_v7::*OpcodeFuncProcV7)(FuncParams &);
	typedef void (Script_v7::*OpcodeGoblinProcV7)(FuncParams &);
	struct OpcodeDrawEntryV7 {
		FuncType type;
		OpcodeDrawProcV7 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeFuncEntryV7 {
		FuncType type;
		OpcodeFuncProcV7 proc;
		const char *desc;
		const Param params[16];
	};
	struct OpcodeGoblinEntryV7 {
		FuncType type;
		OpcodeGoblinProcV7 proc;
		const char *desc;
		const Param params[16];
	};
	const OpcodeDrawEntryV7 *_opcodesDrawV7;
	const OpcodeFuncEntryV7 *_opcodesFuncV7;
	const OpcodeGoblinEntryV7 *_opcodesGoblinV7;
	static const int _goblinFuncLookUp[][2];

	virtual void setupOpcodes();
	virtual void drawOpcode(byte i, FuncParams &params);
	virtual void funcOpcode(byte i, byte j, FuncParams &params);
	virtual void goblinOpcode(int i, FuncParams &params);

	void o7_loadCursor(FuncParams &params);
	void oPlaytoons_printText(FuncParams &params);
	void o7_oemToANSI(FuncParams &params);
	void oPlaytoons_freeSprite(FuncParams &params);
};

#endif // DEGOB_SCRIPT_H
