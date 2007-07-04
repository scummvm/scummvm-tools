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
	compress_agos$(EXEEXT) \
	compress_kyra$(EXEEXT) \
	compress_queen$(EXEEXT) \
	compress_saga$(EXEEXT) \
	compress_scumm_bun$(EXEEXT) \
	compress_scumm_san$(EXEEXT) \
	compress_scumm_sou$(EXEEXT) \
	compress_sword1$(EXEEXT) \
	compress_sword2$(EXEEXT) \
	compress_touche$(EXEEXT) \
	dekyra$(EXEEXT) \
	descumm$(EXEEXT) \
	desword2$(EXEEXT) \
	encode_dxa$(EXEEXT) \
	extract_agos$(EXEEXT) \
	extract_kyra$(EXEEXT) \
	extract_loom_tg16$(EXEEXT) \
	extract_mm_apple$(EXEEXT) \
	extract_mm_c64$(EXEEXT) \
	extract_mm_nes$(EXEEXT) \
	extract_parallaction$(EXEEXT) \
	extract_scumm_mac$(EXEEXT) \
	extract_zak_c64$(EXEEXT)

UTILS := \
	utils/adpcm.o \
	utils/audiostream.o \
	utils/file.o \
	utils/md5.o \
	utils/voc.o \
	utils/wave.o

all: $(TARGETS)

compress_agos$(EXEEXT): compress_agos.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+ -lvorbis -logg -lvorbisenc -lFLAC

compress_kyra$(EXEEXT): compress_kyra.o kyra_pak.o compress.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+ -lvorbis -logg -lvorbisenc -lFLAC

compress_queen$(EXEEXT): compress_queen.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+ -lvorbis -logg -lvorbisenc -lFLAC

compress_saga$(EXEEXT): compress_saga.o compress.o util.o $(UTILS)
	$(CXX) $(LDFLAGS) -o $@ $+ -lvorbis -logg -lvorbisenc -lFLAC

compress_scumm_bun$(EXEEXT): compress_scumm_bun.o compress.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+ -lvorbis -logg -lvorbisenc -lFLAC

compress_scumm_san$(EXEEXT): compress_scumm_san.o compress.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+ -lz -lvorbis -logg -lvorbisenc -lFLAC

compress_scumm_sou$(EXEEXT): compress_scumm_sou.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+ -lvorbis -logg -lvorbisenc -lFLAC

compress_sword1$(EXEEXT): compress_sword1.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+ -lvorbis -logg -lvorbisenc -lFLAC

compress_sword2$(EXEEXT): compress_sword2.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+ -lvorbis -logg -lvorbisenc -lFLAC

compress_touche$(EXEEXT): compress_touche.o compress.o util.o
	$(CC) $(LDFLAGS) -o $@ $+ -lvorbis -logg -lvorbisenc -lFLAC

dekyra$(EXEEXT): dekyra.o dekyra_v1.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

descumm$(EXEEXT): descumm-tool.o descumm.o descumm6.o descumm-common.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

desword2$(EXEEXT): desword2.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

encode_dxa$(EXEEXT): encode_dxa.o compress.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+ -lpng -lz -lvorbis -logg -lvorbisenc -lFLAC

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

extract_parallaction$(EXEEXT): extract_parallaction.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

extract_scumm_mac$(EXEEXT): extract_scumm_mac.o util.o
	$(CC) $(LDFLAGS) -o $@ $+

extract_zak_c64$(EXEEXT): extract_zak_c64.o util.o
	$(CC) $(LDFLAGS) -o $@ $+


descumm.o descumm6.o descumm-common.o descumm-tool.o: descumm.h

# All compress_* tools use compress.h
compress_agos.o compress_saga.o compress_scumm_sou.o \
compress_scumm_bun.o compress_sword1.o compress_sword2.o \
compress_kyra.o compress_queen.o compress.o encode_dxa.o: compress.h

# extract_parallaction.h
extract_parallaction.o: extract_parallaction.h

# Virtually everything depends on util.h
compress_agos.o  compress_saga.o compress_scumm_sou.o \
compress_scumm_bun.o compress_sword1.o compress_sword2.o \
compress.o \
compress_queen.o \
compress_kyra.o \
descumm.o descumm6.o descumm-common.o descumm-tool.o \
dekyra.o \
dekyra_v1.o \
desword2.o \
encode_dxa.o \
extract_kyra.o \
extract_loom_tg16.o \
extract_mm_apple.o \
extract_mm_c64.o \
extract_mm_nes.o \
extract_parallaction.o \
extract_scumm_mac.o \
extract_zak_c64.o \
kyra_pak.o \
util.o: util.h

clean:
	rm -f *.o utils/*.o $(TARGETS)

.cpp.o:
	$(CXX) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o
