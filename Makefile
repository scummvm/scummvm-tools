SRC=.

CC      := gcc
CXX     := g++
CFLAGS  := -g -O -Wall -Wuninitialized -Wno-long-long -Wno-multichar
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
	compress_san$(EXEEXT) \
	convbdf$(EXEEXT) \
	descumm$(EXEEXT) \
	desword2$(EXEEXT) \
	extract$(EXEEXT) \
	md5table$(EXEEXT) \
	mm_nes_extract$(EXEEXT) \
	queenrebuild$(EXEEXT) \
	rescumm$(EXEEXT) \
	simon1decr$(EXEEXT) \
	simon2mp3$(EXEEXT)

all: $(TARGETS)

compress_san$(EXEEXT): compress_san.o util.o
	$(CXX) $(LFLAGS) -o $@ $+ -lz

convbdf$(EXEEXT): convbdf.o util.o
	$(CXX) $(LFLAGS) -o $@ $+

descumm$(EXEEXT): descumm-tool.o descumm.o descumm6.o descumm-common.o util.o
	$(CXX) $(LFLAGS) -o $@ $+

desword2$(EXEEXT): desword2.o util.o
	$(CXX) $(LFLAGS) -o $@ $+

extract$(EXEEXT): extract.o extract-common.o util.o
	$(CC) $(LFLAGS) -o $@ $+

md5table$(EXEEXT): md5table.o util.o
	$(CXX) $(LFLAGS) -o $@ $+

mm_nes_extract$(EXEEXT): mm_nes_extract.o
	$(CC) $(LFLAGS) -o $@ $+

queenrebuild$(EXEEXT): queenrebuild.o util.o
	$(CC) $(LFLAGS) -o $@ $+

rescumm$(EXEEXT): rescumm.o util.o
	$(CC) $(LFLAGS) -o $@ $+

simon1decr$(EXEEXT): simon1decr.o
	$(CC) $(LFLAGS) -o $@ $+

simon2mp3$(EXEEXT): simon2mp3.o extract-common.o util.o
	$(CC) $(LFLAGS) -o $@ $+


descumm.o descumm6.o descumm-common.o descumm-tool.o: descumm.h util.h
extract.o simon2mp3.o extract-common.o: util.h extract.h
desword2.o md5table.o queenrebuild.o rescumm.o util.o: util.h

clean:
	rm -f *.o $(TARGETS)

.cpp.o:
	$(CXX) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o
