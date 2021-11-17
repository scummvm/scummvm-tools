/*
** $Id: llex.cpp 940 2008-07-26 19:30:44Z aquadran $
** Lexical Analizer
** See Copyright Notice in lua.h
*/


#include "lauxlib.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "luadebug.h"
#include "lzio.h"



int32 lua_debug=0;


#define next(LS) (LS->current = zgetc(LS->lex_z))


#define save(c)	luaL_addchar(c)
#define save_and_next(LS)  (save(LS->current), next(LS))


const char *reserved [] = {"and", "do", "else", "elseif", "end", "function",
    "if", "local", "nil", "not", "or", "repeat", "return", "then",
    "until", "while"};


void luaX_init (void)
{
  uint32 i;
  for (i=0; i<(sizeof(reserved)/sizeof(reserved[0])); i++) {
    TaggedString *ts = luaS_new(reserved[i]);
    ts->head.marked = FIRST_RESERVED+i;  /* reserved word  (always > 255) */
  }
}


void luaX_syntaxerror (LexState *ls, const char *s, const char *token) {
  if (token[0] == 0)
    token = "<eof>";
  luaL_verror("%.100s;\n  last token read: `%.50s' at line %d in chunk `%.50s'",
              s, token, ls->linenumber, zname(ls->lex_z));
}


void luaX_error (LexState *ls, const char *s) {
  save(0);
  luaX_syntaxerror(ls, s, luaL_buffer());
}


void luaX_token2str (LexState * /*ls*/, int32 token, char *s) {
  if (token < 255) {
    s[0] = token;
    s[1] = 0;
  }
  else
    strcpy(s, reserved[token-FIRST_RESERVED]);
}


static void luaX_invalidchar (LexState *ls, int32 c) {
  char buff[10];
  sprintf(buff, "0x%X", (int)c);
  luaX_syntaxerror(ls, "invalid control char", buff);
}


static void firstline (LexState *LS)
{
  int32 c = zgetc(LS->lex_z);
  if (c == '#')
    while ((c=zgetc(LS->lex_z)) != '\n' && c != EOZ) /* skip first line */;
  zungetc(LS->lex_z);
}


void luaX_setinput (LexState *LS, ZIO *z)
{
  LS->current = '\n';
  LS->linenumber = 0;
  LS->iflevel = 0;
  LS->ifstate[0].skip = 0;
  LS->ifstate[0].elsepart = 1;  /* to avoid a free $else */
  LS->lex_z = z;
  LS->fs = NULL;
  firstline(LS);
  luaL_resetbuffer();
}



/*
** =======================================================
** PRAGMAS
** =======================================================
*/

#define PRAGMASIZE	20

static void skipspace (LexState *LS)
{
  while (LS->current == ' ' || LS->current == '\t' || LS->current == '\r')
    next(LS);
}


static int32 checkcond (LexState *LS, char *buff)
{
  static const char *opts[] = {"nil", "1", NULL};
  int32 i = luaL_findstring(buff, opts);
  if (i >= 0) return i;
  else if (isalpha((byte)buff[0]) || buff[0] == '_')
    return luaS_globaldefined(buff);
  else {
    luaX_syntaxerror(LS, "invalid $if condition", buff);
    return 0;  /* to avoid warnings */
  }
}


static void readname (LexState *LS, char *buff)
{
  int32 i = 0;
  skipspace(LS);
  while (isalnum(LS->current) || LS->current == '_') {
    if (i >= PRAGMASIZE) {
      buff[PRAGMASIZE] = 0;
      luaX_syntaxerror(LS, "pragma too long", buff);
    }
    buff[i++] = LS->current;
    next(LS);
  }
  buff[i] = 0;
}


static void inclinenumber (LexState *LS);


static void ifskip (LexState *LS)
{
  while (LS->ifstate[LS->iflevel].skip) {
    if (LS->current == '\n')
      inclinenumber(LS);
    else if (LS->current == EOZ)
      luaX_error(LS, "input ends inside a $if");
    else next(LS);
  }
}


static void inclinenumber (LexState *LS)
{
  static const char *pragmas [] =
    {"debug", "nodebug", "endinput", "end", "ifnot", "if", "else", NULL};
  next(LS);  /* skip '\n' */
  ++LS->linenumber;
  if (LS->current == '$') {  /* is a pragma? */
    char buff[PRAGMASIZE+1];
    int32 ifnot = 0;
    int32 skip = LS->ifstate[LS->iflevel].skip;
    next(LS);  /* skip $ */
    readname(LS, buff);
    switch (luaL_findstring(buff, pragmas)) {
      case 0:  /* debug */
        if (!skip) lua_debug = 1;
        break;
      case 1:  /* nodebug */
        if (!skip) lua_debug = 0;
        break;
      case 2:  /* endinput */
        if (!skip) {
          LS->current = EOZ;
          LS->iflevel = 0;  /* to allow $endinput inside a $if */
        }
        break;
      case 3:  /* end */
        if (LS->iflevel-- == 0)
          luaX_syntaxerror(LS, "unmatched $end", "$end");
        break;
      case 4:  /* ifnot */
        ifnot = 1;
        /* fall through */
      case 5:  /* if */
        if (LS->iflevel == MAX_IFS-1)
          luaX_syntaxerror(LS, "too many nested $ifs", "$if");
        readname(LS, buff);
        LS->iflevel++;
        LS->ifstate[LS->iflevel].elsepart = 0;
        LS->ifstate[LS->iflevel].condition = checkcond(LS, buff) ? !ifnot : ifnot;
        LS->ifstate[LS->iflevel].skip = skip || !LS->ifstate[LS->iflevel].condition;
        break;
      case 6:  /* else */
        if (LS->ifstate[LS->iflevel].elsepart)
          luaX_syntaxerror(LS, "unmatched $else", "$else");
        LS->ifstate[LS->iflevel].elsepart = 1;
        LS->ifstate[LS->iflevel].skip = LS->ifstate[LS->iflevel-1].skip ||
                                      LS->ifstate[LS->iflevel].condition;
        break;
      default:
        luaX_syntaxerror(LS, "unknown pragma", buff);
    }
    skipspace(LS);
    if (LS->current == '\n')  /* pragma must end with a '\n' ... */
      inclinenumber(LS);
    else if (LS->current != EOZ)  /* or eof */
      luaX_syntaxerror(LS, "invalid pragma format", buff);
    ifskip(LS);
  }
}


/*
** =======================================================
** LEXICAL ANALIZER
** =======================================================
*/





static int32 read_long_string (LexState *LS)
{
  int32 cont = 0;
  while (1) {
    switch (LS->current) {
      case EOZ:
        luaX_error(LS, "unfinished long string");
        return EOS;  /* to avoid warnings */
      case '[':
        save_and_next(LS);
        if (LS->current == '[') {
          cont++;
          save_and_next(LS);
        }
        continue;
      case ']':
        save_and_next(LS);
        if (LS->current == ']') {
          if (cont == 0) goto endloop;
          cont--;
          save_and_next(LS);
        }
        continue;
      case '\n':
        save('\n');
        inclinenumber(LS);
        continue;
      default:
        save_and_next(LS);
    }
  } endloop:
  save_and_next(LS);  /* pass the second ']' */
  LS->seminfo.ts = luaS_newlstr(L->Mbuffbase+2,
                          L->Mbuffnext-(L->Mbuffbase-L->Mbuffer)-4);
  return STRING;
}


int32 luaX_lex (LexState *LS) {
  double a;
  luaL_resetbuffer();
  while (1) {
    switch (LS->current) {

      case ' ': case '\t': case '\r':  /* CR: to avoid problems with DOS */
        next(LS);
        continue;

      case '\n':
        inclinenumber(LS);
        continue;

      case '-':
        save_and_next(LS);
        if (LS->current != '-') return '-';
        do { next(LS); } while (LS->current != '\n' && LS->current != EOZ);
        luaL_resetbuffer();
        continue;

      case '[':
        save_and_next(LS);
        if (LS->current != '[') return '[';
        else {
          save_and_next(LS);  /* pass the second '[' */
          return read_long_string(LS);
        }

      case '=':
        save_and_next(LS);
        if (LS->current != '=') return '=';
        else { save_and_next(LS); return EQ; }

      case '<':
        save_and_next(LS);
        if (LS->current != '=') return '<';
        else { save_and_next(LS); return LE; }

      case '>':
        save_and_next(LS);
        if (LS->current != '=') return '>';
        else { save_and_next(LS); return GE; }

      case '~':
        save_and_next(LS);
        if (LS->current != '=') return '~';
        else { save_and_next(LS); return NE; }

      case '"':
      case '\'': {
        int32 del = LS->current;
        save_and_next(LS);
        while (LS->current != del) {
          switch (LS->current) {
            case EOZ:
            case '\n':
              luaX_error(LS, "unfinished string");
              return EOS;  /* to avoid warnings */
            case '\\':
              next(LS);  /* do not save the '\' */
              switch (LS->current) {
                case 'a': save('\a'); next(LS); break;
                case 'b': save('\b'); next(LS); break;
                case 'f': save('\f'); next(LS); break;
                case 'n': save('\n'); next(LS); break;
                case 'r': save('\r'); next(LS); break;
                case 't': save('\t'); next(LS); break;
                case 'v': save('\v'); next(LS); break;
                case '\n': save('\n'); inclinenumber(LS); break;
                default : {
                  if (isdigit(LS->current)) {
                    int32 c = 0;
                    int32 i = 0;
                    do {
                      c = 10*c + (LS->current-'0');
                      next(LS);
                    } while (++i<3 && isdigit(LS->current));
                    if (c >= 256)
                      luaX_error(LS, "escape sequence too large");
                    save(c);
                  }
                  else {  /* handles \, ", ', and ? */
                    save(LS->current);
                    next(LS);
                  }
                  break;
                }
              }
              break;
            default:
              save_and_next(LS);
          }
        }
        save_and_next(LS);  /* skip delimiter */
        LS->seminfo.ts = luaS_newlstr(L->Mbuffbase+1,
                                L->Mbuffnext-(L->Mbuffbase-L->Mbuffer)-2);
        return STRING;
      }

      case '.':
        save_and_next(LS);
        if (LS->current == '.')
        {
          save_and_next(LS);
          if (LS->current == '.')
          {
            save_and_next(LS);
            return DOTS;   /* ... */
          }
          else return CONC;   /* .. */
        }
        else if (!isdigit(LS->current)) return '.';
        /* LS->current is a digit: goes through to number */
	a=0.0;
        goto fraction;

      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
	a=0.0;
        do {
          a = 10.0*a + (LS->current-'0');
          save_and_next(LS);
        } while (isdigit(LS->current));
        if (LS->current == '.') {
          save_and_next(LS);
          if (LS->current == '.') {
            save('.');
            luaX_error(LS, 
              "ambiguous syntax (decimal point x string concatenation)");
          }
        }
      fraction:
	{ double da=0.1;
	  while (isdigit(LS->current))
	  {
            a += (LS->current-'0')*da;
            da /= 10.0;
            save_and_next(LS);
          }
          if (toupper(LS->current) == 'E') {
	    int32 e = 0;
	    int32 neg;
	    double ea;
            save_and_next(LS);
	    neg = (LS->current=='-');
            if (LS->current == '+' || LS->current == '-') save_and_next(LS);
            if (!isdigit(LS->current))
              luaX_error(LS, "invalid numeral format");
            do {
              e = 10*e + (LS->current-'0');
              save_and_next(LS);
            } while (isdigit(LS->current));
	    for (ea=neg?0.1:10.0; e>0; e>>=1)
	    {
	      if (e & 1) a *= ea;
	      ea *= ea;
	    }
          }
          LS->seminfo.r = (real)a;
          return NUMBER;
        }

      case EOZ:
        if (LS->iflevel > 0)
          luaX_error(LS, "input ends inside a $if");
        return EOS;

      default:
        if (LS->current != '_' && !isalpha(LS->current)) {
          int32 c = LS->current;
          if (iscntrl(c))
            luaX_invalidchar(LS, c);
          save_and_next(LS);
          return c;
        }
        else {  /* identifier or reserved word */
          TaggedString *ts;
          do {
            save_and_next(LS);
          } while (isalnum(LS->current) || LS->current == '_');
          save(0);
          ts = luaS_new(L->Mbuffbase);
          if (ts->head.marked >= 'A')
            return ts->head.marked;  /* reserved word */
          LS->seminfo.ts = ts;
          return NAME;
        }
    }
  }
}

