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
	antipasto$(EXEEXT)

all: $(TARGETS)

dekyra$(EXEEXT): dekyra.o dekyra_v1.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

descumm$(EXEEXT): descumm-tool.o descumm.o descumm6.o descumm-common.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

desword2$(EXEEXT): desword2.o util.o
	$(CXX) $(LDFLAGS) -o $@ $+

antipasto$(EXEEXT): antipasto.scm util.scm cfgg.scm graph.scm structuring.scm pseudo.scm scummv5.scm
	csc $< -o $@ -postlude [main]

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
