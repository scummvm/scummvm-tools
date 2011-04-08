/*
** $Id: lfunc.h 905 2008-07-20 21:08:22Z aquadran $
** Lua Function structures
** See Copyright Notice in lua.h
*/

#ifndef lfunc_h
#define lfunc_h


#include "lobject.h"



TProtoFunc *luaF_newproto (void);
Closure *luaF_newclosure (int32 nelems);
void luaF_freeproto (TProtoFunc *l);
void luaF_freeclosure (Closure *l);

char *luaF_getlocalname (TProtoFunc *func, int32 local_number, int32 line);


#endif
