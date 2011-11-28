/*
** $Id$
** save bytecodes to file
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include "luac.h"
#include "common/endian.h"

#define NotWord(x)		((unsigned short)x!=x)
#define DumpBlock(b,size,D)	fwrite(b,size,1,D)
#define	DumpNative(t,D)		DumpBlock(&t,sizeof(t),D)

static void DumpWord(int i, FILE* D) {
 byte out[2];
 WRITE_BE_UINT16(out, i);
 DumpBlock(out,2,D);
}

static void DumpLong(long i, FILE* D) {
 byte out[4];
 WRITE_BE_UINT32(out, i);
 DumpBlock(out,4,D);
}

/* LUA_NUMBER */
/* assumes sizeof(long)==4 and sizeof(float)==4 (IEEE) */
static void DumpFloat(float f, FILE* D) {
	byte *fdata = (byte *)(&f);

	fputc(fdata[3],D);
	fputc(fdata[2],D);
	fputc(fdata[1],D);
	fputc(fdata[0],D);
}

static void DumpCode(TProtoFunc* tf, FILE* D) {
 int size=CodeSize(tf);
 if (NotWord(size))
  fprintf(stderr,"luac: warning: "
	"\"%s\":%d code too long for 16-bit machines (%d bytes)\n",
	fileName(tf),tf->lineDefined,size);
 DumpLong(size,D);
 DumpBlock(tf->code,size,D);
}

static void DumpString(char* s, int size, FILE* D) {
 if (s == NULL)
  DumpWord(0,D);
 else {
  if (NotWord(size))
   luaL_verror("string too long (%d bytes): \"%.32s...\"",size,s);
  DumpWord(size,D);
  for(int i = 0; i < size; ++i)
	  fputc(s[i] ^ 0xFF, D);
 }
}

static void DumpTString(TaggedString* s, FILE* D) {
 if (s == NULL) DumpString(NULL,0,D); else DumpString(s->str,s->u.s.len+1,D);
}

static void DumpLocals(TProtoFunc* tf, FILE* D) {
 int n;
 LocVar* lv;
 for (n=0,lv=tf->locvars; lv && lv->line>=0; lv++) ++n;
 DumpWord(n,D);
 for (lv=tf->locvars; lv && lv->line>=0; lv++) {
  DumpWord(lv->line,D);
  DumpTString(lv->varname,D);
 }
}

static void DumpFunction(TProtoFunc* tf, FILE* D);

static void DumpSubFunctions(TProtoFunc* tf, FILE* D) {
	int i,n;
	n = tf->nconsts;
	for (i=0; i<n; i++) {
		TObject* o=tf->consts+i;
		if (ttype(o) == LUA_T_PROTO) {
			fputc('#',D);
			DumpWord(i,D);
			DumpFunction(tfvalue(o),D);
		}
	}
	fputc('$',D);
}

static void DumpConstants(TProtoFunc* tf, FILE* D) {
 int i,n;
 n = tf->nconsts;
 DumpWord(n,D);
 for (i=0; i<n; i++) {
  TObject* o=tf->consts+i;
  switch (ttype(o)) {
   case LUA_T_NUMBER:
	fputc('N',D);
	DumpNumber(nvalue(o),D);
	break;
   case LUA_T_STRING:
	fputc('S',D);
	DumpTString(tsvalue(o),D);
	break;
   case LUA_T_PROTO:
	fputc('F',D);
	break;
   case LUA_T_NIL:
	fputc(-ttype(o),D);
	break;
   default:				/* cannot happen */
	luaL_verror("cannot dump constant #%d: type=%d [%s]",
		i,ttype(o),luaO_typename(o));
	break;
  }
 }
}

static void DumpFunction(TProtoFunc* tf, FILE* D) {
 DumpWord(tf->lineDefined,D);
 DumpTString(tf->fileName,D);
 DumpCode(tf,D);
 DumpConstants(tf,D);
 DumpLocals(tf,D);
 DumpSubFunctions(tf,D);
}

static void DumpHeader(TProtoFunc* Main, FILE* D) {
 real t=TEST_NUMBER;
 fputc(ID_CHUNK,D);
 fputs(SIGNATURE,D);
 fputc(VERSION,D);
 fputc(sizeof(t),D);
 fputc(ID_NUMBER,D);
 DumpBlock("\x0A\xBF\x17",3,D);		//Instead TEST_NUMBER, it dumps the same sequence found in GF scripts
}

void DumpChunk(TProtoFunc* Main, FILE* D) {
 DumpHeader(Main,D);
 DumpFunction(Main,D);
}
