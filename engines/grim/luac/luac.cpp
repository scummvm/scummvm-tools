/*
** $Id$
** lua compiler (saves bytecodes to files; also list binary files)
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "luac.h"
#include "engines/grim/lua/lparser.h"
#include "engines/grim/lua/lzio.h"
#include "engines/grim/lua/luadebug.h"

#define	OUTPUT	"luac.out"		/* default output file */

extern void DumpChunk(TProtoFunc* Main, FILE* D);
extern void PrintChunk(TProtoFunc* Main);
extern void OptChunk(TProtoFunc* Main);
extern void rebase(TProtoFunc* Main, TProtoFunc* base);

static void load_base_script(const char* fname);
static FILE* efopen(const char* name, const char* mode);
static void doit(int undump, const char* filename);

static int listing=0;			/* list bytecodes? */
static int debugging=0;			/* debug? */
static int dumping=1;			/* dump bytecodes? */
static int undumping=0;			/* undump bytecodes? */
static int optimizing=0;		/* optimize? */
static int parsing=0;			/* parse only? */
static int verbose=0;			/* tell user what is done */
static FILE* D;				/* output file */
static TProtoFunc *bs = NULL;

static void usage(void)
{
 fprintf(stderr,"usage: "
 "luac [-c | -u] [-D name] [-d] [-l] [-o output] [-O] [-p] [-q] [-v] [-V] [-b base] [files]\n"
 " -c\tcompile (default)\n"
 " -u\tundump\n"
 " -d\tgenerate debugging information\n"
 " -D\tpredefine symbol for conditional compilation\n"
 " -l\tlist (default for -u)\n"
 " -o\toutput file for -c (default is \"" OUTPUT "\")\n"
 " -O\toptimize\n"
 " -p\tparse only\n"
 " -q\tquiet (default for -c)\n"
 " -v\tshow version information\n"
 " -V\tverbose\n"
 " -b\tused the specified script as base for compiling (useful for patch, see diffr manual)\n"
 " -\tcompile \"stdin\"\n"
 );
 exit(1);
}

#define	IS(s)	(strcmp(argv[i],s)==0)

int main(int argc, char* argv[])
{
 const char* d = OUTPUT;			/* output file name */
 const char* base_s = NULL;			/* base script file name */
 int i;
 lua_open();
 for (i=1; i<argc; i++)
 {
  if (argv[i][0]!='-')			/* end of options */
   break;
  else if (IS("-"))			/* use stdin */
   break;
  else if (IS("-c"))			/* compile (and dump) */
  {
   dumping=1;
   undumping=0;
  }
  else if (IS("-D"))			/* $define */
  {
   TaggedString* s=luaS_new(argv[++i]);
   s->u.s.globalval.ttype=LUA_T_NUMBER;
   s->u.s.globalval.value.n=1;
  }
  else if (IS("-d"))			/* debug */
   debugging=1;
  else if (IS("-l"))			/* list */
   listing=1;
  else if (IS("-b"))			/* base script */
   base_s=argv[++i];
  else if (IS("-o"))			/* output file */
   d=argv[++i];
  else if (IS("-O"))			/* optimize */
   optimizing=1; 
  else if (IS("-p"))			/* parse only */
  {
   dumping=0;
   parsing=1;
  }
  else if (IS("-q"))			/* quiet */
   listing=0;
  else if (IS("-u"))			/* undump */
  {
   dumping=0;
   undumping=1;
   listing=1;
  }
  else if (IS("-v"))			/* show version */
   printf("%s  %s\n(written by %s)\n\n",LUA_VERSION,LUA_COPYRIGHT,LUA_AUTHORS);
  else if (IS("-V"))			/* verbose */
   verbose=1;
  else					/* unknown option */
   usage();
 }
 --i;					/* fake new argv[0] */
 argc-=i;
 argv+=i;

	if (dumping || parsing) {
		if (argc < 2)
			usage();

		if (dumping) {
			for (i = 1; i < argc; i++)		/* play safe with output file */
				if (IS(d))
					luaL_verror("will not overwrite input file \"%s\"",d);
			D = efopen(d,"wb");			/* must open in binary mode */
		}

		if (dumping && base_s != NULL)
			load_base_script(base_s);

		for (i = 1; i < argc; i++)
			doit(0, IS("-")? NULL : argv[i]);
		if (dumping)
			fclose(D);
	}

	if (undumping) {
		if (argc < 2)
			doit(1, OUTPUT);
		else
		for (i = 1; i < argc; i++)
			doit(1,IS("-")? NULL : argv[i]);
	}
	return 0;
}

static void load_base_script(const char* fname) {
	FILE* f;
	ZIO z;
	f = efopen(fname, "rb");
	zFopen(&z,f,fname);
	bs = luaU_undump1(&z);
	fclose(f);
}

static void do_compile(ZIO* z)
{
 TProtoFunc* Main;
 if (optimizing) lua_debug=0;		/* set debugging before parsing */
 if (debugging)  lua_debug=1;
 Main=luaY_parser(z);
 if (bs) rebase(Main, bs);
 if (optimizing) OptChunk(Main);
 if (listing) PrintChunk(Main);
 if (dumping) DumpChunk(Main,D);
}

static void do_undump(ZIO* z)
{
 while (1)
 {
  TProtoFunc* Main=luaU_undump1(z);
  if (Main==NULL) break;
  if (optimizing) OptChunk(Main);
  if (listing) PrintChunk(Main);
 }
}

static void doit(int undump, const char* filename) {
	FILE* f;
	ZIO z;
	const char *fn;
	if (filename==NULL) {
		fn = "(stdin)";
		f = stdin;
	} else {
		fn = filename;
		f = efopen(fn, undump ? "rb" : "r");
	}
	zFopen(&z,f,fn);
	if (verbose)
		fprintf(stderr,"%s\n",fn);
	if (undump)
		do_undump(&z);
	else
		do_compile(&z);
	if (f != stdin)
		fclose(f);
}

static FILE* efopen(const char* name, const char* mode) {
 FILE* f=fopen(name,mode);
 if (f==NULL)
 {
  fprintf(stderr,"luac: cannot open %sput file ",mode[0]=='r' ? "in" : "out");
  perror(name);
  exit(1);
 }
 return f; 
}
