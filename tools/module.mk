# $URL: https://residual.svn.sourceforge.net/svnroot/residual/residual/trunk/tools/module.mk $
# $Id: module.mk 1426 2009-05-31 08:59:56Z aquadran $

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
	tools/patchex/patchex$(EXEEXT)

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

#
# Build rules for the tools
#

tools/delua$(EXEEXT): $(srcdir)/tools/delua.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common -Ltools/lua -o $@ $< -llua

tools/mat2ppm$(EXEEXT): $(srcdir)/tools/mat2ppm.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -lppm -o $@ $<

tools/bmtoppm$(EXEEXT): $(srcdir)/tools/bmtoppm.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -lppm -lpbm -o $@ $<

tools/imc2wav$(EXEEXT): $(srcdir)/tools/imc2wav.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/int2flt$(EXEEXT): $(srcdir)/tools/int2flt.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/cosb2cos$(EXEEXT): $(srcdir)/tools/emi/cosb2cos.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common -o $@ $<

tools/meshb2obj$(EXEEXT): $(srcdir)/tools/emi/meshb2obj.o $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -o $@ $<

tools/animb2txt$(EXEEXT): $(srcdir)/tools/emi/animb2txt.cpp $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -o $@ $<

tools/setb2set$(EXEEXT): $(srcdir)/tools/emi/setb2set.cpp $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -o $@ $<

tools/sklb2txt$(EXEEXT): $(srcdir)/tools/emi/sklb2txt.cpp $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -o $@ $<

tools/set2fig$(EXEEXT): $(srcdir)/tools/set2fig.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/til2bmp$(EXEEXT): $(srcdir)/tools/emi/til2bmp.cpp $(srcdir)/tools/emi/lab.o
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common tools/emi/lab.o -lz -o $@ $<

tools/unlab$(EXEEXT): $(srcdir)/tools/unlab.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/mklab$(EXEEXT): $(srcdir)/tools/mklab.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/vima$(EXEEXT): $(srcdir)/tools/vima.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/labcopy$(EXEEXT): $(srcdir)/tools/labcopy.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -Wall \
	-L$(srcdir)/common -o $@ $<

tools/patchex/patchex$(EXEEXT): tools/patchex/patchex.o tools/patchex/mszipd.o tools/patchex/cabd.o
	$(MKDIR) tools/patchex/$(DEPDIR)
	$(CXX) $(CFLAGS) tools/patchex/mszipd.o tools/patchex/cabd.o -Wall -o $@ $<

.PHONY: clean-tools tools
