# $URL$
# $Id$

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
	compress_kyra$(EXEEXT) \
	compress_queen$(EXEEXT) \
	compress_saga$(EXEEXT) \
	compress_san$(EXEEXT) \
	compress_scumm_sou$(EXEEXT) \
	compress_scumm_bun$(EXEEXT) \
	compress_simon$(EXEEXT) \
	compress_sword1$(EXEEXT) \
	compress_sword2$(EXEEXT) \
	dekyra$(EXEEXT) \
	descumm$(EXEEXT) \
	desword2$(EXEEXT) \
	extract_kyra$(EXEEXT) \
	extract_loom_tg16$(EXEEXT) \
	extract_mm_c64$(EXEEXT) \
	extract_mm_nes$(EXEEXT) \
	extract_scumm_mac$(EXEEXT) \
	extract_simon1_amiga$(EXEEXT) \
	extract_zak_c64$(EXEEXT)

all: $(TARGETS)

compress_san$(EXEEXT): compress_san.o compress.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+ -lz

descumm$(EXEEXT): descumm-tool.o descumm.o descumm6.o descumm-common.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

desword2$(EXEEXT): desword2.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

dekyra$(EXEEXT): dekyra.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

extract_kyra$(EXEEXT): extract_kyra.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

compress_scumm_sou$(EXEEXT): compress_scumm_sou.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_scumm_bun$(EXEEXT): compress_scumm_bun.o compress.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

extract_loom_tg16$(EXEEXT): extract_loom_tg16.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_mm_c64$(EXEEXT): extract_mm_c64.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_mm_nes$(EXEEXT): extract_mm_nes.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_queen$(EXEEXT): compress_queen.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_scumm_mac$(EXEEXT): extract_scumm_mac.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_saga$(EXEEXT): compress_saga.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_simon1_amiga$(EXEEXT): extract_simon1_amiga.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_zak_c64$(EXEEXT): extract_zak_c64.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_simon$(EXEEXT): compress_simon.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_sword1$(EXEEXT): compress_sword1.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_sword2$(EXEEXT): compress_sword2.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

compress_kyra$(EXEEXT): compress_kyra.o compress.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+


descumm.o descumm6.o descumm-common.o descumm-tool.o: descumm.h

# Most compress_* tools (except for compress_queen) use compress.h
compress_saga.o compress_scumm_sou.o compress_scumm_bun.o \
compress_simon.o compress_sword1.o compress_sword2.o \
compress_kyra.o compress.o: compress.h

# Virtually everything depends on util.h
compress_saga.o compress_scumm_sou.o compress_scumm_bun.o \
compress_simon.o compress_sword1.o compress_sword2.o \
compress.o \
compress_queen.o \
compress_kyra.o \
descumm.o descumm6.o descumm-common.o descumm-tool.o \
dekyra.o \
desword2.o \
extract_kyra.o \
extract_loom_tg16.o \
extract_mm_c64.o \
extract_mm_nes.o \
extract_scumm_mac.o \
extract_zak_c64.o \
util.o: util.h

clean:
	rm -f *.o $(TARGETS)

.cpp.o:
	$(CXX) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o
