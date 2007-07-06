# $URL$
# $Id$

SRC=.

CC      := gcc
CXX     := g++
CFLAGS  += -g -O -Wall -Wuninitialized -Wno-long-long -Wno-multichar -DUNIX
# LDFLAGS +=

# Additional warnings
CFLAGS+= -Wshadow
CFLAGS+= -pedantic
CFLAGS+= -Wpointer-arith -Wcast-qual -Wcast-align
# -Wconversion
CFLAGS+= -Wshadow -Wimplicit -Wundef -Wwrite-strings

TARGETS := \
	dekyra$(EXEEXT) \
	descumm$(EXEEXT) \
	desword2$(EXEEXT) \

all: $(TARGETS)

dekyra$(EXEEXT): dekyra.o dekyra_v1.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

descumm$(EXEEXT): descumm-tool.o descumm.o descumm6.o descumm-common.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

desword2$(EXEEXT): desword2.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

encode_dxa$(EXEEXT): encode_dxa.o compress.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+ -lpng -lz

extract_agos$(EXEEXT): extract_agos.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_kyra$(EXEEXT): extract_kyra.o kyra_pak.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

extract_loom_tg16$(EXEEXT): extract_loom_tg16.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_mm_apple$(EXEEXT): extract_mm_apple.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_mm_c64$(EXEEXT): extract_mm_c64.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_mm_nes$(EXEEXT): extract_mm_nes.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_scumm_mac$(EXEEXT): extract_scumm_mac.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_zak_c64$(EXEEXT): extract_zak_c64.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

descumm.o descumm6.o descumm-common.o descumm-tool.o: descumm.h

descumm.o descumm6.o descumm-common.o descumm-tool.o \
dekyra.o \
dekyra_v1.o \
desword2.o \
util.o: util.h

clean:
	rm -f *.o $(TARGETS)

.cpp.o:
	$(CXX) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o
