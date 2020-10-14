/*
** $Id: lgc.h 934 2008-07-26 18:06:34Z aquadran $
** Garbage Collector
** See Copyright Notice in lua.h
*/

#ifndef lgc_h
#define lgc_h


#include "lobject.h"


void luaC_checkGC (void);
TObject* luaC_getref (int32 ref);
int32 luaC_ref (TObject *o, int32 lock);
void luaC_hashcallIM (Hash *l);
void luaC_strcallIM (TaggedString *l);


#endif
