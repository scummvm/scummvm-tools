/* Simon1Decr - Decrunch Simon the Sorcerer 1 Amiga (AGA/ECS) graphics and music files
 * Copyright (C) 2003  The ScummVM Team
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef unsigned int ULONG;
typedef unsigned char UBYTE;

#define EndGetM32(a)  ((((a)[0])<<24)|(((a)[1])<<16)|(((a)[2])<<8)|((a)[3]))

#define SD_GETBIT(var) do { \
  if (!bits--) { s -= 4; if (s < src) return 0; bb=EndGetM32(s); bits=31; } \
  (var) = bb & 1; bb >>= 1; \
} while (0)

#define SD_GETBITS(var, nbits) do { \
  bc=(nbits); (var)=0; while (bc--) {(var)<<=1; SD_GETBIT(bit); (var)|=bit; } \
} while (0)

#define SD_TYPE_LITERAL (0)
#define SD_TYPE_MATCH   (1)

int simon_decr(UBYTE *src, UBYTE *dest, ULONG srclen) {
  UBYTE *s = &src[srclen - 4];
  ULONG destlen = EndGetM32(s), bb, x, y;
  UBYTE *d = &dest[destlen], bc, bit, bits, type;

  /* initialise bit buffer */
  s -= 4; x = EndGetM32(s); bb = x;
  bits = 0; do { x >>= 1; bits++; } while (x); bits--;

  while (d > dest) {
    SD_GETBIT(x);
    if (x) {
      SD_GETBITS(x, 2);
           if (x == 0) { type = SD_TYPE_MATCH;   x = 9;  y = 2;            }
      else if (x == 1) { type = SD_TYPE_MATCH;   x = 10; y = 3;            }
      else if (x == 2) { type = SD_TYPE_MATCH;   x = 12; SD_GETBITS(y, 8); }
      else             { type = SD_TYPE_LITERAL; x = 8;  y = 8;            }
    }
    else {
      SD_GETBIT(x);
      if (x)           { type = SD_TYPE_MATCH;   x = 8;  y = 1;            }
      else             { type = SD_TYPE_LITERAL; x = 3;  y = 0;            }
    }

    if (type == SD_TYPE_LITERAL) {
      SD_GETBITS(x, x); y += x;
      if ((y + 1) > (d - dest)) return 0; /* overflow? */
      do { SD_GETBITS(x, 8); *--d = x; } while (y-- > 0);
    }
    else {
      if ((y + 1) > (d - dest)) return 0; /* overflow? */
      SD_GETBITS(x, x);
      if ((d + x) > (dest + destlen)) return 0; /* offset overflow? */
      do { d--; *d = d[x]; } while (y-- > 0);
    }
  }
  /* successful decrunch */
  return 1;
}

ULONG simon_decr_length(UBYTE *src, ULONG srclen) {
  return EndGetM32(&src[srclen - 4]);
}


/* - loadfile(filename) loads a file from disk, and returns a pointer to that
 *   loaded file, or returns NULL on failure
 * - call free() on ptr to free memory
 * - size of loaded file is available in global var 'filelen'
 */
size_t filelen;
void *loadfile(char *name) {
  void *mem = NULL; FILE *fd;
  if ((fd = fopen(name, "rb"))) {
    if ((fseek(fd, 0, SEEK_END) == 0) && (filelen = ftell(fd))
    &&  (fseek(fd, 0, SEEK_SET) == 0) && (mem = malloc(filelen))) {
      if (fread(mem, 1, filelen, fd) < filelen) { free(mem); mem = NULL; }
    }
    fclose(fd);
  }
  return mem;
}

/* - savefile(filename, mem, length) saves [length] bytes from [mem] into
 *   the file named by [filename]
 * - returns zero if failed, or non-zero if successful
 */
int savefile(char *name, void *mem, size_t length) {
  FILE *fd = fopen(name, "wb");
  int ok = fd && (fwrite(mem, 1, length, fd) == length);
  if (fd) fclose(fd);
  return ok;
}

char filename[256];

int main(int argc, char *argv[]) {
  for (argv++; *argv; argv++) {
    UBYTE *x = (UBYTE *) loadfile(*argv);
    if (x) {
      ULONG decrlen = simon_decr_length(x, (ULONG) filelen);
      UBYTE *out = (UBYTE *) malloc(decrlen);
      if (out) {
        if (simon_decr(x, out, filelen)) {
          strcpy(filename, *argv);
          strcat(filename, ".out");
          savefile(filename, out, decrlen);
        }
        else {
          printf("%s: decrunch error\n", filename);
        }
        free((void *) x);
      }
    }
  }
  return 0;
}
