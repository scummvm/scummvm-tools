/*
** $Id$
** print bytecodes
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include "luac.h"

void PrintConstant1(TProtoFunc* tf, int i)
{
 TObject* o=tf->consts+i;
 printf("%6d ",i);
 if (i<0 || i>=tf->nconsts)
  printf("(bad constant #%d: max=%d)",i,tf->nconsts);
 else
  switch (ttype(o))
  {
   case LUA_T_NUMBER:
	 printf("N\t" NUMBER_FMT "\n", nvalue(o));	/* LUA_NUMBER */
	 break;
   case LUA_T_STRING:
	 printf("S\t\"%s\"\n", svalue(o));
	 break;
   case LUA_T_PROTO:
	 printf("F\n");
	 break;
   default:				/* cannot happen */
	 printf("? %d\n",ttype(o)); 
   break;
  }
}

static void PrintConstants(TProtoFunc* tf)
{
 int i,n=tf->nconsts;
 printf("constants (%d):\n",n);
 for (i=0; i<n; i++) PrintConstant1(tf,i);
}

static void PrintConstant(TProtoFunc* tf, int i)
{
 if (i<0 || i>=tf->nconsts)
  printf("(bad constant #%d: max=%d)",i,tf->nconsts);
 else
 {
  TObject* o=tf->consts+i;
  switch (ttype(o))
  {
   case LUA_T_NUMBER:
	printf(NUMBER_FMT,nvalue(o));		/* LUA_NUMBER */
	break;
   case LUA_T_STRING:
	printf("\"%s\"",svalue(o));
	break;
   case LUA_T_PROTO:
	printf("function");
	break;
   case LUA_T_NIL:
	printf("(nil)");
	break;
   default:				/* cannot happen */
	printf("(bad constant #%d: type=%d [%s])\n",i,ttype(o),luaO_typename(o));
	break;
  }
 }
}

#define VarStr(i)       svalue(tf->consts+i)

static void PrintCode(TProtoFunc* tf)
{
 Byte* code=tf->code;
 Byte* p=code;
 int line=0;
 while (1)
 {
	Opcode OP;
	int n=INFO(tf,p,&OP);
	int op=OP.op;
	int i=OP.arg;
	printf("%6d  ",(int)(p-code));
	{
	 Byte* q=p;
	 int j=n;
	 while (j--) printf("%02X",*q++);
	}
	printf("%*s%-13s",2*(5-n),"",OP.name);

	if (n!=1 || op<0) printf("\t%d",i); else if (i>=0) printf("\t");

	switch (OP.op_class)
	{

	case ENDCODE:
		printf("\n");
		return;

	case CLOSURE:
		printf(" %d",OP.arg2);
		// fall-through
	case PUSHCONSTANT:
	case GETDOTTED:
	case PUSHSELF:
		printf("\t; ");
		PrintConstant(tf,i);
		break;

	case PUSHLOCAL:
	case SETLOCAL:
	{
		char* s=luaF_getlocalname(tf,i+1,line);
		if (s) printf("\t; %s",s);
		break;
	}

	case GETGLOBAL:
	case SETGLOBAL:
		printf("\t; %s",VarStr(i));
		break;

	case SETLIST:
	case CALLFUNC:
		if (n>=3) printf(" %d",OP.arg2);
		break;

	case SETLINE:
		printf("\t; \"%s\":%d",fileName(tf),line=i);
		break;

/* suggested by Norman Ramsey <nr@cs.virginia.edu> */
	case IFTUPJMP:
	case IFFUPJMP:
		i=-i;
		// fall-through
	case ONTJMP:
	case ONFJMP:
	case JMP:
	case IFFJMP:
		printf("\t; to %d",(int)(p-code)+i+n);
		break;

	}
	printf("\n");
	p+=n;
 }
}

static void PrintLocals(TProtoFunc* tf)
{
 LocVar* v=tf->locvars;
 int n,i=0;
 if (v==NULL || v->varname==NULL) return;
 n=tf->code[1]; if (n>=ZEROVARARG) n-=ZEROVARARG;

 printf("locals:");
 if (n>0)
 {
  for (i=0; i<n; v++,i++) printf(" %s",v->varname->str);
 }
 if (v->varname!=NULL)
 {
  for (; v->line>=0; v++)
  {
   if (v->varname==NULL)
   {
    printf(")"); --i;
   }
   else
   {
    printf(" (%s",v->varname->str); i++;
   }
  }
  i-=n;
  while (i--) printf(")");
 }
 printf("\n");
}

static void PrintHeader(TProtoFunc* tf, TProtoFunc* Main, int at)
{
 int size=CodeSize(tf);
 if (IsMain(tf))
  printf("\nmain of \"%s\" (%d bytes at %p)\n",fileName(tf),size,(void*)tf);
 else if (Main)
 {
  printf("\nfunction defined at \"%s\":%d (%d bytes); used at ",
	fileName(tf),tf->lineDefined,size);
  if (IsMain(Main))
   printf("main");
  else
   printf("%p",(void*)Main);
  printf("+%d\n",at);
 }
}

static void PrintFunction(TProtoFunc* tf, TProtoFunc* Main, int at);

static void PrintFunctions(TProtoFunc* Main)
{
 Byte* code=Main->code;
 Byte* p=code;
 while (1)
 {
  Opcode OP;
  int n=INFO(Main,p,&OP);
  if (OP.op_class==ENDCODE) break;
  if (OP.op_class==PUSHCONSTANT || OP.op_class==CLOSURE)
  {
   int i=OP.arg;
   TObject* o=Main->consts+i;
   if (ttype(o)==LUA_T_PROTO) PrintFunction(tfvalue(o),Main,(int)(p-code));
  }
  p+=n;
 }
}

static void PrintFunction(TProtoFunc* tf, TProtoFunc* Main, int at)
{
 PrintHeader(tf,Main,at);
 PrintLocals(tf);
 PrintCode(tf);
 PrintConstants(tf);
 PrintFunctions(tf);
}

void PrintChunk(TProtoFunc* Main)
{
 PrintFunction(Main,0,0);
}
