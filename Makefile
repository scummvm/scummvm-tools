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
	dekyra$(EXEEXT) \
	kyra_unpak$(EXEEXT) \
	compress_scumm_sou$(EXEEXT) \
	loom_tg16_extract$(EXEEXT) \
	md5table$(EXEEXT) \
	mm_nes_extract$(EXEEXT) \
	queenrebuild$(EXEEXT) \
	rescumm$(EXEEXT) \
	simon1decr$(EXEEXT) \
	compress_simon$(EXEEXT) \
	compress_sword1$(EXEEXT) \
	compress_sword2$(EXEEXT) \
	compress_saga$(EXEEXT)

all: $(TARGETS)

compress_san$(EXEEXT): compress_san.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+ -lz

convbdf$(EXEEXT): convbdf.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

descumm$(EXEEXT): descumm-tool.o descumm.o descumm6.o descumm-common.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

desword2$(EXEEXT): desword2.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

dekyra$(EXEEXT): dekyra.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

kyra_unpak$(EXEEXT): kyra_unpak.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

compress_scumm_sou$(EXEEXT): compress_scumm_sou.o extract-common.o util.o
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

compress_saga$(EXEEXT): compress_saga.o extract-common.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

simon1decr$(EXEEXT): simon1decr.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_simon$(EXEEXT): compress_simon.o extract-common.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_sword1$(EXEEXT): compress_sword1.o extract-common.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_sword2$(EXEEXT): compress_sword2.o extract-common.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

descumm.o descumm6.o descumm-common.o descumm-tool.o: descumm.h util.h
compress_saga.o compress_scumm_sou.o compress_simon.o compress_sword1.o compress_sword2.o extract-common.o: util.h extract.h
desword2.o md5table.o queenrebuild.o rescumm.o util.o: util.h

clean:
	rm -f *.o $(TARGETS)

.cpp.o:
	$(CXX) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o
