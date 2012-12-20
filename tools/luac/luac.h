/*
** $Id$
** definitions for luac
** See Copyright Notice in lua.h
*/

#include "tools/lua/lauxlib.h"
#include "tools/lua/lfunc.h"
#include "tools/lua/lobject.h"
#include "tools/lua/lopcodes.h"
#include "tools/lua/lstring.h"
#include "tools/lua/lundump.h"
#include "common/endian.h"

typedef struct
{
 const char* name;
 int size;
 int op;
 int op_class;
 int arg;
 int arg2;
} Opcode;

int OpcodeInfo(TProtoFunc* tf, Byte* p, Opcode* I, const char* xFILE, int xLINE);
int CodeSize(TProtoFunc* tf);

#define INFO(tf,p,I)	OpcodeInfo(tf,p,I,__FILE__,__LINE__)
#define fileName(tf)	( (tf->fileName)==NULL ? NULL : tf->fileName->str )

#define NOP	255
#define STACK	-1
#define ARGS	-2
#define VARARGS	-3
