/*
** $Id$
** opcode information
** See Copyright Notice in lua.h
*/

#include "luac.h"

static const Opcode Info[]=			/* ORDER lopcodes.h */
{
#include "opcode.h"
};

#define NOPCODES	(sizeof(Info)/sizeof(Info[0]))

int OpcodeInfo(TProtoFunc* tf, Byte* p, Opcode* I, const char* xFILE, int xLINE) {
 Opcode OP;
 Byte* code=tf->code;
 uint op=*p;
 if (p==code)
 {
  OP.name="STACK";
  OP.size=1;
  OP.op=STACK;
  OP.op_class=STACK;
  OP.arg=op;
 }
 else if (p==code+1)
 {
  OP.size=1;
  if (op>=ZEROVARARG)
  {
   OP.name="VARARGS";
   OP.op=VARARGS;
   OP.op_class=VARARGS;
   OP.arg=op-ZEROVARARG;
  }
  else
  {
   OP.name="ARGS";
   OP.op=ARGS;
   OP.op_class=ARGS;
   OP.arg=op;
  }
 }
 else if (op>=NOPCODES)
 {
  OP.name="NOP";
  OP.size=1;
  OP.op=NOP;
  OP.op_class=NOP;
 }
 else
 {
  OP=Info[op];
  if (op==SETLIST || op==CLOSURE || op==CALLFUNC)
  {
   OP.arg=p[1];
   OP.arg2=p[2];
  }
  else if (OP.size == 2) OP.arg = p[1];
  else if (OP.size >= 3) OP.arg = READ_LE_UINT16(p + 1);
  if (op == SETLISTW) OP.arg2 = p[3];
 }
 *I=OP;
 return OP.size;
}

int CodeSize(TProtoFunc* tf)
{
 Byte* code=tf->code;
 Byte* p=code;
 while (1)
 {
  Opcode OP;
  p+=INFO(tf,p,&OP);
  if (OP.op==ENDCODE) break;
 }
 return p-code;
}
