# Where is scummvm will be installed
SCUMMVMPATH=C:/scummvm
SRC=.

CC      = cc
CFLAGS  = -g -O -Wall -Wstrict-prototypes -Wuninitialized -Wno-long-long -Wno-multichar 
LDFLAGS :=

TARGETS := descumm3$(EXEEXT) descumm5$(EXEEXT) descumm6$(EXEEXT) extract$(EXEEXT) rescumm$(EXEEXT)

all: $(TARGETS)

descumm3$(EXEEXT) : descumm3.o
	$(CC) $(LFLAGS) -o $@ $+

descumm5$(EXEEXT) : descumm5.o
	$(CC) $(LFLAGS) -o $@ $+

descumm6$(EXEEXT) : descumm6.o
	$(CC) $(LFLAGS) -o $@ $+

extract$(EXEEXT) : extract.o
	$(CC) $(LFLAGS) -o $@ $+

rescumm$(EXEEXT) : rescumm.o
	$(CC) $(LFLAGS) -o $@ $+

clean:
	rm -f *.o $(TARGETS)

.cpp.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(<) -o $*.o
