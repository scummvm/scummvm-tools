/*
** $Id: lobject.h 905 2008-07-20 21:08:22Z aquadran $
** Type definitions for Lua objects
** See Copyright Notice in lua.h
*/

#ifndef lobject_h
#define lobject_h


#include "lua.h"


#ifdef DEBUG
#include "lauxlib.h"
#define LUA_INTERNALERROR(s)	\
	luaL_verror("INTERNAL ERROR - %s [%s:%d]",(s),__FILE__,__LINE__)
#define LUA_ASSERT(c,s) { if (!(c)) LUA_INTERNALERROR(s); }
#else
#define LUA_INTERNALERROR(s)  /* empty */
#define LUA_ASSERT(c,s)  /* empty */
#endif


/*
** "real" is the type "number" of Lua
** GREP LUA_NUMBER to change that
*/
#ifndef LUA_NUM_TYPE
#define LUA_NUM_TYPE float
#endif

/*
** format to convert number to strings
*/
#define NUMBER_FMT  "%g"

typedef LUA_NUM_TYPE real;

#define Byte lua_Byte	/* some systems have Byte as a predefined type */
typedef byte Byte;  /* unsigned 8 bits */


#define MAX_INT   (2147483647-2)  /* maximum value of an int (-2 for safety) */
#define MAX_WORD        65534

typedef size_t IntPoint; /* unsigned with same size as a pointer (for hashing) */


/*
** Lua TYPES
** WARNING: if you change the order of this enumeration,
** grep "ORDER LUA_T"
*/
typedef enum {
  LUA_T_USERDATA =  0,  /* tag default for userdata */
  LUA_T_NUMBER   = -1,  /* fixed tag for numbers */
  LUA_T_STRING   = -2,  /* fixed tag for strings */
  LUA_T_ARRAY    = -3,  /* tag default for tables (or arrays) */
  LUA_T_PROTO    = -4,  /* fixed tag for functions */
  LUA_T_CPROTO   = -5,  /* fixed tag for Cfunctions */
  LUA_T_TASK     = -6,  /* task tag */
  LUA_T_NIL      = -7,  /* last "pre-defined" tag */
  LUA_T_CLOSURE  = -8,
  LUA_T_CLMARK   = -9,  /* mark for closures */
  LUA_T_PMARK    = -10,  /* mark for Lua prototypes */
  LUA_T_CMARK    = -11, /* mark for C prototypes */
  LUA_T_LINE     = -12
} lua_Type;

#define NUM_TYPES 12
#define NUM_TAGS  8


typedef union {
  lua_CFunction f;  /* LUA_T_CPROTO, LUA_T_CMARK */
  real n;  /* LUA_T_NUMBER */
  struct TaggedString *ts;  /* LUA_T_STRING, LUA_T_USERDATA */
  struct TProtoFunc *tf;  /* LUA_T_PROTO, LUA_T_PMARK */
  struct Closure *cl;  /* LUA_T_CLOSURE, LUA_T_CLMARK */
  struct Hash *a;  /* LUA_T_ARRAY */
  int32 i;  /* LUA_T_LINE */
} Value;


typedef struct TObject {
  lua_Type ttype;
  Value value;
} TObject;



/*
** generic header for garbage collector lists
*/
typedef struct GCnode {
  struct GCnode *next;
  int32 marked;
} GCnode;


/*
** String headers for string table
*/

typedef struct TaggedString {
  GCnode head;
  unsigned long hash;
  int32 constindex;  /* hint to reuse constants (= -1 if this is a userdata) */
  union {
    struct {
      TObject globalval;
      int32 len;  /* if this is a string, here is its length */
    } s;
    struct {
      int32 tag;
      void *v;  /* if this is a userdata, here is its value */
    } d;
  } u;
  char str[1];   /* \0 byte already reserved */
} TaggedString;




/*
** Function Prototypes
*/
typedef struct TProtoFunc {
  GCnode head;
  struct TObject *consts;
  int32 nconsts;
  Byte *code;  /* ends with opcode ENDCODE */
  int32 lineDefined;
  TaggedString  *fileName;
  struct LocVar *locvars;  /* ends with line = -1 */
} TProtoFunc;

typedef struct LocVar {
  TaggedString *varname;           /* NULL signals end of scope */
  int32 line;
} LocVar;





/* Macros to access structure members */
#define ttype(o)        ((o)->ttype)
#define nvalue(o)       ((o)->value.n)
#define svalue(o)       ((o)->value.ts->str)
#define tsvalue(o)      ((o)->value.ts)
#define clvalue(o)      ((o)->value.cl)
#define avalue(o)       ((o)->value.a)
#define fvalue(o)       ((o)->value.f)
#define tfvalue(o)	((o)->value.tf)

#define protovalue(o)	((o)->value.cl->consts)


/*
** Closures
*/
typedef struct Closure {
  GCnode head;
  int32 nelems;  /* not included the first one (always the prototype) */
  TObject consts[1];  /* at least one for prototype */
} Closure;



typedef struct node {
  TObject ref;
  TObject val;
} Node;

typedef struct Hash {
  GCnode head;
  Node *node;
  int32 nhash;
  int32 nuse;
  int32 htag;
} Hash;


extern const char *luaO_typenames[];

extern TObject luaO_nilobject;

int32 luaO_equalObj (TObject *t1, TObject *t2);
int32 luaO_redimension (int32 oldsize);
void luaO_insertlist (GCnode *root, GCnode *node);

#define luaO_memup(d,s,n)	memmove(d,s,n)
#define luaO_memdown(d,s,n)	memmove(d,s,n)

#endif
