SRC=.

CC      := gcc
CXX     := g++
CFLAGS  += -g -O -Wall -Wuninitialized -Wno-long-long -Wno-multichar
# LDFLAGS +=

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
	loom_tg16_extract$(EXEEXT) \
	md5table$(EXEEXT) \
	mm_nes_extract$(EXEEXT) \
	queenrebuild$(EXEEXT) \
	rescumm$(EXEEXT) \
	simon1decr$(EXEEXT) \
	simon2mp3$(EXEEXT) \
	sword1mp3$(EXEEXT) \
	sword2mp3$(EXEEXT) \
	saga2mp3$(EXEEXT)

all: $(TARGETS)

compress_san$(EXEEXT): compress_san.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+ -lz

convbdf$(EXEEXT): convbdf.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

descumm$(EXEEXT): descumm-tool.o descumm.o descumm6.o descumm-common.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

desword2$(EXEEXT): desword2.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

extract$(EXEEXT): extract.o extract-common.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

loom_tg16_extract$(EXEEXT): loom_tg16_extract.o
	$(CC) $(LDFLAGS) -o $@ $+

md5table$(EXEEXT): md5table.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

mm_nes_extract$(EXEEXT): mm_nes_extract.o
	$(CC) $(LDFLAGS) -o $@ $+

queenrebuild$(EXEEXT): queenrebuild.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

rescumm$(EXEEXT): rescumm.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

saga2mp3$(EXEEXT): saga2mp3.o extract-common.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

simon1decr$(EXEEXT): simon1decr.o
	$(CC) $(LDFLAGS) -o $@ $+

simon2mp3$(EXEEXT): simon2mp3.o extract-common.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

sword1mp3$(EXEEXT): sword1mp3.o extract-common.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

sword2mp3$(EXEEXT): sword2mp3.o extract-common.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

descumm.o descumm6.o descumm-common.o descumm-tool.o: descumm.h util.h
extract.o simon2mp3.o sword2mp3.o extract-common.o: util.h extract.h
desword2.o md5table.o queenrebuild.o rescumm.o util.o: util.h

clean:
	rm -f *.o $(TARGETS)

.cpp.o:
	$(CXX) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o
