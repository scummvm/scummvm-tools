SRC=.

CC      := gcc
CXX     := g++
CFLAGS  := -g -O -Wall -Wstrict-prototypes -Wuninitialized -Wno-long-long -Wno-multichar
LDFLAGS :=

# Additional warnings
CFLAGS+= -Wshadow
CFLAGS+= -pedantic
CFLAGS+= -Wpointer-arith -Wcast-qual -Wcast-align
# -Wconversion
CFLAGS+= -Wshadow -Wimplicit -Wundef -Wwrite-strings 

# Uncomment this if you are on a big endian system
# CFLAGS += -DSCUMM_BIG_ENDIAN

TARGETS := \
	descumm$(EXEEXT) \
	extract$(EXEEXT) \
	mm_nes_extract$(EXEEXT) \
	queenrebuild$(EXEEXT) \
	rescumm$(EXEEXT) \
	simon1decr$(EXEEXT) \
	simon2mp3$(EXEEXT)

all: $(TARGETS)

descumm$(EXEEXT): descumm-tool.o descumm.o descumm6.o descumm-common.o
	$(CXX) $(LFLAGS) -o $@ $+

extract$(EXEEXT): extract.o
	$(CC) $(LFLAGS) -o $@ $+

mm_nes_extract$(EXEEXT): mm_nes_extract.o
	$(CC) $(LFLAGS) -o $@ $+

queenrebuild$(EXEEXT): queenrebuild.o
	$(CC) $(LFLAGS) -o $@ $+

rescumm$(EXEEXT): rescumm.o
	$(CC) $(LFLAGS) -o $@ $+

simon1decr$(EXEEXT): simon1decr.o
	$(CC) $(LFLAGS) -o $@ $+

simon2mp3$(EXEEXT): simon2mp3.o
	$(CC) $(LFLAGS) -o $@ $+


descumm.o descumm6.o descumm-common.o descumm-tool.o: descumm.h

clean:
	rm -f *.o $(TARGETS)

.cpp.o:
	$(CXX) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o
