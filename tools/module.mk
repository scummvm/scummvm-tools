
MODULE := tools

MODULE_DIRS += \
	tools/

#######################################################################
# Tools directory
#######################################################################

TOOLS := \
	tools/delua$(EXEEXT) \
	tools/imc2wav$(EXEEXT) \
	tools/int2flt$(EXEEXT) \
	tools/cosb2cos$(EXEEXT) \
	tools/meshb2obj$(EXEEXT) \
	tools/sklb2txt$(EXEEXT) \
	tools/animb2txt$(EXEEXT) \
	tools/setb2set$(EXEEXT) \
	tools/set2fig$(EXEEXT) \
	tools/til2bmp$(EXEEXT) \
	tools/unlab$(EXEEXT) \
	tools/mklab$(EXEEXT) \
	tools/vima$(EXEEXT) \
	tools/labcopy$(EXEEXT) \
	tools/luac/luac$(EXEEXT) \
	tools/patchex/patchex$(EXEEXT) \
	tools/diffr$(EXEEXT) \
	tools/patchr$(EXEEXT)

# below not added as it depends for ppm, bpm library
#	tools/mat2ppm$(EXEEXT)
#	tools/bm2ppm$(EXEEXT)

# Make sure the 'all' / 'clean' targets build/clean the tools, too
#all:
clean: clean-tools

# Main target
tools: $(TOOLS)

clean-tools:
	-$(RM) $(TOOLS)
	-$(RM) tools/emi/*.o
	-$(RM) tools/patchex/*.o
	-$(RM) -r tools/patchex/.deps
	-$(RM) -r tools/luac/*.o
	-$(RM) -r tools/luac/.deps

#
# Build rules for the tools
#

tools/diffr$(EXEEXT): $(srcdir)/tools/diffr.cpp $(srcdir)/common/md5.o $(srcdir)/common/zlib.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common $(srcdir)/common/md5.o  $(srcdir)/common/zlib.o -lz -o $@ $< $(LDFLAGS)

tools/patchr$(EXEEXT): $(srcdir)/tools/patchr.cpp $(srcdir)/common/md5.o $(srcdir)/common/zlib.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common $(srcdir)/common/md5.o  $(srcdir)/common/zlib.o -lz -o $@ $< $(LDFLAGS)

tools/delua$(EXEEXT): $(srcdir)/tools/delua.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common -Ltools/lua -o $@ $< $(LDFLAGS) -llua

#g++ -DHAVE_CONFIG_H -DUNIX -I. -I./tools/luac  -I ./tools/lua   -c -o tools/luac/print.o tools/luac/print.c

tools/luac/luac$(EXEEXT):
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I$(srcdir)/tools/lua -c -o tools/luac/dump.o tools/luac/dump.c
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I$(srcdir)/tools/lua -c -o tools/luac/luac.o tools/luac/luac.c
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I$(srcdir)/tools/lua -c -o tools/luac/opcode.o tools/luac/opcode.c
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I$(srcdir)/tools/lua -c -o tools/luac/opt.o tools/luac/opt.c
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I$(srcdir)/tools/lua -c -o tools/luac/print.o tools/luac/print.c
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I$(srcdir)/tools/lua -c -o tools/luac/rebase.o tools/luac/rebase.c
	$(MKDIR) tools/luac/$(DEPDIR)
	$(CXX) $(CFLAGS) tools/luac/dump.o tools/luac/luac.o tools/luac/opcode.o tools/luac/opt.o tools/luac/print.o tools/luac/rebase.o -Wall -L$(srcdir)/tools/lua -llua -o $@ $< $(LDFLAGS)

tools/mat2ppm$(EXEEXT): $(srcdir)/tools/mat2ppm.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -lppm -o $@ $< $(LDFLAGS)

tools/bmtoppm$(EXEEXT): $(srcdir)/tools/bmtoppm.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -lppm -lpbm -o $@ $< $(LDFLAGS)

tools/imc2wav$(EXEEXT): $(srcdir)/tools/imc2wav.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $< $(LDFLAGS)

tools/int2flt$(EXEEXT): $(srcdir)/tools/int2flt.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $< $(LDFLAGS)

tools/cosb2cos$(EXEEXT): $(srcdir)/tools/emi/cosb2cos.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common -o $@ $< $(LDFLAGS)

tools/meshb2obj$(EXEEXT): $(srcdir)/tools/emi/meshb2obj.o $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -o $@ $< $(LDFLAGS)

tools/animb2txt$(EXEEXT): $(srcdir)/tools/emi/animb2txt.cpp $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -o $@ $< $(LDFLAGS)

tools/setb2set$(EXEEXT): $(srcdir)/tools/emi/setb2set.cpp $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -o $@ $< $(LDFLAGS)

tools/sklb2txt$(EXEEXT): $(srcdir)/tools/emi/sklb2txt.cpp $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -o $@ $< $(LDFLAGS)

tools/set2fig$(EXEEXT): $(srcdir)/tools/set2fig.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $< $(LDFLAGS)

tools/til2bmp$(EXEEXT): $(srcdir)/tools/emi/til2bmp.cpp $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -o $@ $< $(LDFLAGS) -lz

tools/unlab$(EXEEXT): $(srcdir)/tools/unlab.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $< $(LDFLAGS)

tools/mklab$(EXEEXT): $(srcdir)/tools/mklab.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $< $(LDFLAGS)

tools/vima$(EXEEXT): $(srcdir)/tools/vima.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $< $(LDFLAGS)

tools/labcopy$(EXEEXT): $(srcdir)/tools/labcopy.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -Wall \
	-L$(srcdir)/common -o $@ $< $(LDFLAGS)

tools/patchex/patchex$(EXEEXT): tools/patchex/patchex.o tools/patchex/mszipd.o tools/patchex/cabd.o
	$(MKDIR) tools/patchex/$(DEPDIR)
	$(CXX) $(CFLAGS) tools/patchex/mszipd.o tools/patchex/cabd.o -Wall -o $@ $< $(LDFLAGS)

.PHONY: clean-tools tools
