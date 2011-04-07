/*
** $Id$
** Syntax analizer and code generator
** See Copyright Notice in lua.h
*/

#ifndef lparser_h
#define lparser_h

#include <tools/lua/lobject.h>
#include <tools/lua/lzio.h>


void luaY_codedebugline (int line);
TProtoFunc *luaY_parser (ZIO *z);
void luaY_error (char *s);
void luaY_syntaxerror (char *s, char *token);


#endif
